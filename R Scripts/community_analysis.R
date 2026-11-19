
library(tidyverse)
library(data.table)
library(igraph)

# --------------------------------------------------
# 1. Load and prepare data
# --------------------------------------------------

games <- read.csv(
  "./games_preprocessed.csv",
  stringsAsFactors = FALSE
)

games <- games %>%
  filter(!is.na(AppID)) %>%
  distinct(AppID, .keep_all = TRUE)

# Create a game-tag table
game_tags <- games %>%
  select(AppID, Tags) %>%
  filter(!is.na(Tags), Tags != "") %>%
  separate_rows(Tags, sep = ",") %>%
  mutate(Tags = str_trim(str_to_lower(Tags))) %>%
  filter(Tags != "") %>%
  distinct(AppID, Tags)

# --------------------------------------------------
# 2. Filter overly common tags
# --------------------------------------------------

tag_frequency <- game_tags %>%
  count(Tags, name = "Game.Count")

# Keep tags appearing in at least 2 games,
# but no more than 200 games.
max_tag_games <- 200

retained_tags <- tag_frequency %>%
  filter(
    Game.Count >= 2,
    Game.Count <= max_tag_games
  ) %>%
  pull(Tags)

game_tags <- game_tags %>%
  filter(Tags %in% retained_tags)

# --------------------------------------------------
# 3. Generate pairs of games sharing tags
# --------------------------------------------------

# Convert to data.table for efficient grouping
tag_dt <- as.data.table(game_tags)

# Number of retained tags per game
game_tag_counts <- tag_dt[
  ,
  .(Tag.Count = uniqueN(Tags)),
  by = AppID
]

# Generate game pairs separately for each tag.
# The progress message shows which tag is being processed.

tags_to_process <- unique(tag_dt$Tags)

pair_list <- vector("list", length(tags_to_process))

for (i in seq_along(tags_to_process)) {
  
  current_tag <- tags_to_process[i]
  
  current_games <- tag_dt[
    Tags == current_tag,
    unique(AppID)
  ]
  
  if (length(current_games) >= 2) {
    pairs <- t(combn(current_games, 2))
    
    pair_list[[i]] <- data.table(
      Game1 = pairs[, 1],
      Game2 = pairs[, 2]
    )
  }
  
  if (i %% 25 == 0 || i == length(tags_to_process)) {
    cat(
      "Processed", i, "of",
      length(tags_to_process), "tags\n"
    )
  }
}

# Combine and count shared tags
pair_dt <- rbindlist(pair_list, use.names = TRUE)

shared_counts <- pair_dt[
  ,
  .(Shared.Tags = .N),
  by = .(Game1, Game2)
]

# --------------------------------------------------
# 4. Calculate Jaccard similarity
# --------------------------------------------------

counts_dt <- as.data.table(game_tag_counts)

shared_counts <- merge(
  shared_counts,
  counts_dt,
  by.x = "Game1",
  by.y = "AppID"
)

setnames(
  shared_counts,
  "Tag.Count",
  "Game1.Tags"
)

shared_counts <- merge(
  shared_counts,
  counts_dt,
  by.x = "Game2",
  by.y = "AppID"
)

setnames(
  shared_counts,
  "Tag.Count",
  "Game2.Tags"
)

shared_counts[
  ,
  Jaccard := Shared.Tags /
    (Game1.Tags + Game2.Tags - Shared.Tags)
]

# --------------------------------------------------
# 5. Filter final edges
# --------------------------------------------------

jaccard_threshold <- 0.15
minimum_shared_tags <- 2

final_edges <- shared_counts[
  Shared.Tags >= minimum_shared_tags &
    Jaccard >= jaccard_threshold
]

cat("Final edges:", nrow(final_edges), "\n")

# --------------------------------------------------
# 5. Construct game network
# --------------------------------------------------

# Only include games that have at least one final edge
network_game_ids <- unique(c(
  final_edges$Game1,
  final_edges$Game2
))

game_lookup <- games %>%
  filter(AppID %in% network_game_ids) %>%
  transmute(
    name = as.character(AppID),
    AppID,
    GameName = Name,
    Peak.CCU,
    Positive,
    Negative
  )

network_edges <- final_edges %>%
  transmute(
    from = as.character(Game1),
    to = as.character(Game2),
    weight = Jaccard,
    Shared.Tags
  )

game_net <- graph_from_data_frame(
  network_edges,
  directed = FALSE,
  vertices = game_lookup
)

# --------------------------------------------------
# 7. Community detection and analysis
# --------------------------------------------------

weights <- E(game_net)$weight

stopifnot(
  ecount(game_net) > 0,
  !is_directed(game_net),
  length(weights) == ecount(game_net),
  all(is.finite(weights)),
  all(weights > 0)
)

# Louvain community detection
communities <- cluster_louvain(
  game_net,
  weights = weights
)

V(game_net)$community <- membership(communities)

community_sizes <- tibble(
  Community = membership(communities)
) %>%
  count(Community, name = "Game.Count") %>%
  arrange(desc(Game.Count))

comm_densities <- do.call(rbind, lapply(seq_along(communities), function(i) {
  sub_g <- induced_subgraph(game_net, which(membership(communities) == i))
  members = V(sub_g)
  data.frame(
    "Community" = i,
    "Density" = edge_density(sub_g),
    "Average Peak CCU" = mean(members$Peak.CCU),
    "Average Rating" = mean(members$Positive/(members$Positive + members$Negative)),
    check.names        = FALSE
  )
}))

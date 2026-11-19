
library(tidyverse)
library(data.table)
library(igraph)

# --------------------------------------------------
# 1. Load and prepare data
# --------------------------------------------------

games <- read.csv(
  "../games_preprocessed.csv",
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

jaccard_threshold <- 0.15
minimum_shared_tags <- 2

final_edges <- shared_counts[
  Shared.Tags >= minimum_shared_tags &
    Jaccard >= jaccard_threshold
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
# 6. Construct game network
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

plot.new()
# Community size distribution
ggplot(
  community_sizes,
  aes(x = reorder(
    factor(Community),
    Game.Count
  ), y = Game.Count)
) +
  geom_col() +
  scale_y_log10() +
  labs(
    title = "Community Sizes",
    subtitle = "Steam game similarity network",
    x = "Community",
    y = "Number of games (log scale)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

ggsave(
  "../output/community_size_distribution.png",
  width = 10,
  height = 6
)

# Largest Community
largest_community_id <- community_sizes$Community[1]

largest_community <- induced_subgraph(
  game_net,
  vids = V(game_net)[
    communities$membership == largest_community_id
  ]
) 

# Calculate Average peak concurrent users, average rating, and edge density for each community
comm_densities <- do.call(rbind, lapply(seq_along(communities), function(i) {
  sub_g <- induced_subgraph(game_net, which(membership(communities) == i))
  members = V(sub_g)
  
  mean.rating = mean(members$Positive/(members$Positive + members$Negative))
  if (is.nan(mean.rating)) {
    mean.rating = 0
  }
  
  data.frame(
    "Community" = i,
    "Density" = edge_density(sub_g),
    "Average.Peak.CCU" = mean(members$Peak.CCU),
    # The Rating is the number of positive reviews over the total number of reviews
    "Average.Rating" = mean.rating,
    "Size" = community_sizes[which(community_sizes$Community == i), ]$Game.Count,
    "Average.Shared.Tags" = mean(E(sub_g)$Shared.Tags),
    check.names = FALSE
  )
})) %>% arrange(desc(Size))

cat("Number of communities:", length(communities), "\n")

cat("Modularity:", modularity(communities), "\n")

cat("Largest community:", max(comm_densities$Size), "games\n")

# Show Top 10 Communities by size
print(head(comm_densities, 10))

# Show Top 10 Communities by Avg. Peak CCU
print(head(arrange(comm_densities, desc(Average.Peak.CCU)), 10))

# Show Top 10 Communities by Avg. Rating
print(head(arrange(comm_densities, desc(Average.Rating)), 10))

plot.new()
plot(comm_densities$Density, comm_densities$Average.Peak.CCU,
     main = "Community Density vs Average Peak CCU",
     xlab = "Community Density",
     ylab = "Average Peak CCU"
)

plot.new()
plot(comm_densities$Density, comm_densities$Average.Rating,
     main = "Community Density vs Average Rating",
     xlab = "Community Density",
     ylab = "Average Rating"
)


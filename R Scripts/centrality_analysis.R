
library(tidyverse)
library(data.table)
library(igraph)


input_file <- "../pre-processing/games_preprocessed.csv"

output_dir <- "../output/centrality_analysis"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

games <- read.csv(
  input_file,
  stringsAsFactors = FALSE
)

# Standardize IDs and remove duplicate games
games <- games %>%
  filter(!is.na(AppID)) %>%
  mutate(AppID = as.character(AppID)) %>%
  distinct(AppID, .keep_all = TRUE)

# Prepare game-tag relationships
game_tags <- games %>%
  select(AppID, Tags) %>%
  filter(!is.na(Tags), Tags != "") %>%
  separate_rows(Tags, sep = ",") %>%
  mutate(Tags = str_trim(str_to_lower(Tags))) %>%
  filter(Tags != "") %>%
  distinct(AppID, Tags)

# ==================================================
# 2. RECONSTRUCT SIMILARITY NETWORK
# ==================================================

# Use the same filtering parameters as the previous analysis
max_tag_games <- 200
minimum_shared_tags <- 2
jaccard_threshold <- 0.15

# Retain tags appearing in no more than 200 games
tag_frequency <- game_tags %>%
  count(Tags, name = "Game.Count")

retained_tags <- tag_frequency %>%
  filter(
    Game.Count >= 2,
    Game.Count <= max_tag_games
  ) %>%
  pull(Tags)

game_tags <- game_tags %>%
  filter(Tags %in% retained_tags)

cat("Games with retained tags:",
    n_distinct(game_tags$AppID), "\n")
cat("Retained tags:",
    n_distinct(game_tags$Tags), "\n")
cat("Game-tag relationships:",
    nrow(game_tags), "\n")

# Convert to data.table
tag_dt <- as.data.table(game_tags)

# Generate candidate pairs sharing at least one retained tag
tags_to_process <- unique(tag_dt$Tags)

pair_list <- vector("list", length(tags_to_process))

for (i in seq_along(tags_to_process)) {
  
  current_games <- sort(unique(
    tag_dt[Tags == tags_to_process[i], AppID]
  ))
  
  if (length(current_games) >= 2) {
    
    pairs <- t(combn(current_games, 2))
    
    pair_list[[i]] <- data.table(
      Game1 = pairs[, 1],
      Game2 = pairs[, 2]
    )
  }
  
  if (i %% 25 == 0 || i == length(tags_to_process)) {
    cat("Processed", i, "of",
        length(tags_to_process), "tags\n")
  }
}

pair_dt <- rbindlist(pair_list)

rm(pair_list)
gc()

# Count shared tags for every unique game pair
shared_counts <- pair_dt[
  ,
  .(Shared.Tags = .N),
  by = .(Game1, Game2)
]

rm(pair_dt)
gc()

cat("Unique candidate pairs:", nrow(shared_counts), "\n")

# Count retained tags per game
game_tag_counts <- tag_dt[
  ,
  .(Tag.Count = uniqueN(Tags)),
  by = AppID
]

# Add tag counts for both games
shared_counts <- merge(
  shared_counts,
  game_tag_counts,
  by.x = "Game1",
  by.y = "AppID"
)

setnames(shared_counts, "Tag.Count", "Game1.Tags")

shared_counts <- merge(
  shared_counts,
  game_tag_counts,
  by.x = "Game2",
  by.y = "AppID"
)

setnames(shared_counts, "Tag.Count", "Game2.Tags")

# Calculate Jaccard similarity
shared_counts[
  ,
  Jaccard := Shared.Tags /
    (Game1.Tags + Game2.Tags - Shared.Tags)
]

# Apply original network thresholds
final_edges <- shared_counts[
  Shared.Tags >= minimum_shared_tags &
    Jaccard >= jaccard_threshold
]

cat("Final edges:", nrow(final_edges), "\n")

# Construct graph vertices from games with surviving edges
network_game_ids <- unique(c(
  final_edges$Game1,
  final_edges$Game2
))

game_lookup <- games %>%
  filter(AppID %in% network_game_ids) %>%
  transmute(
    name = AppID,
    AppID,
    GameName = Name,
    Genres,
    Tags,
    Peak.CCU,
    Positive,
    Negative
  )

network_edges <- final_edges %>%
  transmute(
    from = Game1,
    to = Game2,
    weight = Jaccard,
    Shared.Tags
  )

game_net <- graph_from_data_frame(
  network_edges,
  directed = FALSE,
  vertices = game_lookup
)

cat("\nNetwork summary\n")
cat("Nodes:", vcount(game_net), "\n")
cat("Edges:", ecount(game_net), "\n")
cat("Components:", components(game_net)$no, "\n")

# ==================================================
# 3. COMMUNITY DETECTION
# ==================================================

set.seed(42)

communities <- cluster_louvain(
  game_net,
  weights = E(game_net)$weight
)

V(game_net)$community <- membership(communities)

cat("Communities:", length(communities), "\n")
cat("Modularity:", modularity(communities), "\n")

# Identify largest connected component
component_result <- components(game_net)

largest_component_id <- which.max(component_result$csize)

largest_component <- induced_subgraph(
  game_net,
  vids = V(game_net)[
    component_result$membership == largest_component_id
  ]
)

cat("Largest component nodes:",
    vcount(largest_component), "\n")

# ==================================================
# 4. CENTRALITY MEASURES
# ==================================================

# Degree: number of direct connections
game_degree <- degree(game_net)

# Strength: sum of Jaccard similarity weights
game_strength <- strength(
  game_net,
  weights = E(game_net)$weight
)

# PageRank: importance based on connections
game_pagerank <- page_rank(
  game_net,
  weights = E(game_net)$weight
)$vector

# Betweenness: calculated in the largest component
# Convert similarity into distance
E(largest_component)$distance <-
  1 / E(largest_component)$weight

game_betweenness <- betweenness(
  largest_component,
  weights = E(largest_component)$distance,
  normalized = TRUE
)

# Combine centrality measures
centrality_results <- tibble(
  AppID = V(game_net)$AppID,
  GameName = V(game_net)$GameName,
  Genres = V(game_net)$Genres,
  Tags = V(game_net)$Tags,
  Community = membership(communities),
  Degree = as.numeric(game_degree),
  Strength = as.numeric(game_strength),
  PageRank = as.numeric(game_pagerank)
)

# Add betweenness values for games in largest component
betweenness_results <- tibble(
  AppID = V(largest_component)$AppID,
  Betweenness = as.numeric(game_betweenness)
)

centrality_results <- centrality_results %>%
  left_join(betweenness_results, by = "AppID") %>%
  arrange(desc(Degree))

write.csv(
  centrality_results,
  file.path(output_dir, "centrality_results.csv"),
  row.names = FALSE
)

# ==================================================
# 5. IDENTIFY IMPORTANT GAMES
# ==================================================

# Top games by each centrality measure
top_degree <- centrality_results %>%
  arrange(desc(Degree)) %>%
  slice_head(n = 20)

top_strength <- centrality_results %>%
  arrange(desc(Strength)) %>%
  slice_head(n = 20)

top_betweenness <- centrality_results %>%
  filter(!is.na(Betweenness)) %>%
  arrange(desc(Betweenness)) %>%
  slice_head(n = 20)

top_pagerank <- centrality_results %>%
  arrange(desc(PageRank)) %>%
  slice_head(n = 20)

write.csv(
  top_degree,
  file.path(output_dir, "top_degree_games.csv"),
  row.names = FALSE
)

write.csv(
  top_strength,
  file.path(output_dir, "top_strength_games.csv"),
  row.names = FALSE
)

write.csv(
  top_betweenness,
  file.path(output_dir, "top_betweenness_games.csv"),
  row.names = FALSE
)

write.csv(
  top_pagerank,
  file.path(output_dir, "top_pagerank_games.csv"),
  row.names = FALSE
)

cat("\nTop games by degree:\n")
print(top_degree)

cat("\nTop games by betweenness:\n")
print(top_betweenness)

# ==================================================
# 6. VISUALIZE CENTRALITY
# ==================================================

# Degree distribution
degree_data <- centrality_results

ggplot(degree_data, aes(x = Degree)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Degree Distribution of Steam Games",
    x = "Degree",
    y = "Number of games"
  ) +
  theme_minimal()

ggsave(
  file.path(output_dir, "degree_distribution.png"),
  width = 9,
  height = 6
)

# Top 20 games by degree
ggplot(
  top_degree,
  aes(
    x = reorder(GameName, Degree),
    y = Degree
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "20 Most Connected Steam Games",
    x = "Game",
    y = "Degree"
  ) +
  theme_minimal()

ggsave(
  file.path(output_dir, "top_degree_games.png"),
  width = 10,
  height = 8
)

# Top 20 games by betweenness
ggplot(
  top_betweenness,
  aes(
    x = reorder(GameName, Betweenness),
    y = Betweenness
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "20 Highest-Betweenness Steam Games",
    x = "Game",
    y = "Betweenness centrality"
  ) +
  theme_minimal()

ggsave(
  file.path(output_dir, "top_betweenness_games.png"),
  width = 10,
  height = 8
)


# ==================================================
# 7. GENRE OVERLAP OF CENTRAL GAMES
# ==================================================

# Select the 10 highest-betweenness games
bridge_games <- centrality_results %>%
  filter(!is.na(Betweenness)) %>%
  arrange(desc(Betweenness)) %>%
  slice_head(n = 10)

# Extract genres of each selected game's neighbours
neighbor_genres <- map_dfr(
  bridge_games$AppID,
  function(game_id) {
    
    game_vertex <- which(V(game_net)$AppID == game_id)
    
    neighbor_ids <- as_ids(
      neighbors(game_net, game_vertex)
    )
    
    neighbor_data <- tibble(
      AppID = V(game_net)$AppID[
        match(neighbor_ids, V(game_net)$name)
      ],
      Genres = V(game_net)$Genres[
        match(neighbor_ids, V(game_net)$name)
      ]
    )
    
    neighbor_data %>%
      separate_rows(Genres, sep = ",") %>%
      mutate(
        Genres = str_trim(Genres),
        Central.Game = bridge_games$GameName[
          match(game_id, bridge_games$AppID)
        ]
      ) %>%
      filter(!is.na(Genres), Genres != "")
  }
)

# Count the genres represented among each game's neighbours
genre_overlap_summary <- neighbor_genres %>%
  count(Central.Game, Genres, name = "Neighbor.Count") %>%
  group_by(Central.Game) %>%
  mutate(
    Genre.Proportion =
      Neighbor.Count / sum(Neighbor.Count)
  ) %>%
  arrange(Central.Game, desc(Neighbor.Count))

write.csv(
  genre_overlap_summary,
  file.path(output_dir, "central_game_neighbor_genres.csv"),
  row.names = FALSE
)

# Visualize the most common neighbour genres
ggplot(
  genre_overlap_summary,
  aes(
    x = reorder(Genres, Neighbor.Count),
    y = Neighbor.Count
  )
) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ Central.Game, scales = "free_y") +
  labs(
    title = "Genre Composition of Central Games' Neighbours",
    x = "Genre",
    y = "Number of neighbouring games"
  ) +
  theme_minimal()

ggsave(
  file.path(output_dir, "central_game_neighbor_genres.png"),
  width = 12,
  height = 10
)

centrality_results %>%
  select(Degree, Strength, PageRank, Betweenness) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3)



# ==================================================
# 8. INVESTIGATE BRIDGING BETWEEN COMMUNITIES
# ==================================================

# Select the 20 highest-betweenness games
bridge_games <- centrality_results %>%
  filter(!is.na(Betweenness)) %>%
  arrange(desc(Betweenness)) %>%
  slice_head(n = 20)

# Get the community memberships of each game's neighbours
neighbor_communities <- map_dfr(
  bridge_games$AppID,
  function(game_id) {
    
    vertex_id <- which(V(game_net)$AppID == game_id)
    
    neighbor_ids <- as.integer(
      neighbors(game_net, vertex_id)
    )
    
    tibble(
      GameName = centrality_results$GameName[
        match(game_id, centrality_results$AppID)
      ],
      Neighbor.Community = V(game_net)$community[
        neighbor_ids
      ],
      Game.Community = V(game_net)$community[vertex_id]
    )
  }
)

# Summarize the community composition of each game's neighbours
bridge_summary <- neighbor_communities %>%
  group_by(GameName, Game.Community) %>%
  summarise(
    Neighbor.Count = n(),
    Distinct.Neighbor.Communities =
      n_distinct(Neighbor.Community),
    Cross.Community.Neighbors =
      sum(Neighbor.Community != Game.Community),
    Cross.Community.Proportion =
      Cross.Community.Neighbors / Neighbor.Count,
    .groups = "drop"
  ) %>%
  left_join(
    centrality_results %>%
      select(GameName, Degree, Betweenness),
    by = "GameName"
  ) %>%
  arrange(desc(Betweenness))

print(bridge_summary, n = 20)

write.csv(
  bridge_summary,
  file.path(output_dir, "bridge_game_analysis.csv"),
  row.names = FALSE
)

# Plot cross-community neighbour proportions
ggplot(
  bridge_summary,
  aes(
    x = reorder(GameName, Cross.Community.Proportion),
    y = Cross.Community.Proportion
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Cross-Community Connections of High-Betweenness Games",
    x = "Game",
    y = "Proportion of Neighbours Outside Own Community"
  ) +
  theme_minimal()

ggsave(
  file.path(output_dir, "bridge_game_communities.png"),
  width = 10,
  height = 8
)


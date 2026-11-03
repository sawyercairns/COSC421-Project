
library(tidyverse)
library(data.table)
library(igraph)

# --------------------------------------------------
# 1. Load and prepare data
# --------------------------------------------------

games <- read.csv(
  "C:/Users/tacti/OneDrive/Documents/GitHub/COSC421-Project/pre-processing/games_preprocessed.csv",
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

cat("Games with retained tags:",
    n_distinct(game_tags$AppID), "\n")
cat("Retained tags:",
    n_distinct(game_tags$Tags), "\n")
cat("Game-tag relationships:",
    nrow(game_tags), "\n")

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

rm(pair_list)
gc()

shared_counts <- pair_dt[
  ,
  .(Shared.Tags = .N),
  by = .(Game1, Game2)
]

rm(pair_dt)
gc()

cat("Unique candidate game pairs:",
    nrow(shared_counts), "\n")

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
# CONNECTED COMPONENT ANALYSIS
# --------------------------------------------------

components_result <- components(game_net)

component_sizes <- tibble(
  Game = names(components_result$membership),
  Component = components_result$membership
) %>%
  count(Component, name = "Game.Count") %>%
  arrange(desc(Game.Count)) %>%
  mutate(
    Percentage = Game.Count / vcount(game_net) * 100,
    Cumulative.Percentage = cumsum(Percentage)
  )

cat("Number of connected components:",
    components_result$no, "\n")

cat("Largest component:",
    max(component_sizes$Game.Count), "games\n")

cat("Games in largest component:",
    round(component_sizes$Percentage[1], 2), "%\n")

cat("Isolated games:",
    sum(component_sizes$Game.Count == 1), "\n")

print(head(component_sizes, 20))

write.csv(
  component_sizes,
  "../output/component_sizes.csv",
  row.names = FALSE
)

# Component size distribution
ggplot(
  component_sizes,
  aes(x = Game.Count)
) +
  geom_histogram(bins = 30) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Connected Component Size Distribution",
    x = "Games per component (log scale)",
    y = "Number of components (log scale)"
  ) +
  theme_minimal()

ggsave(
  "../output/component_size_distribution.png",
  width = 9,
  height = 6
)


cat("\nFinal network\n")
cat("Nodes:", vcount(game_net), "\n")
cat("Edges:", ecount(game_net), "\n")
cat("Density:", edge_density(game_net), "\n")
cat("Components:", components(game_net)$no, "\n")

# --------------------------------------------------
# 7. Save network summary
# --------------------------------------------------

network_summary <- data.frame(
  Nodes = vcount(game_net),
  Edges = ecount(game_net),
  Density = edge_density(game_net),
  Components = components(game_net)$no,
  Average.Degree = mean(degree(game_net))
)

write.csv(
  network_summary,
  "../output/network_summary.csv",
  row.names = FALSE
)


# --------------------------------------------------
# 8. Network centrality and community detection
# --------------------------------------------------

# Degree: number of connected games
# Strength: sum of similarity weights
# PageRank: importance based on connected neighbours

centrality <- tibble(
  AppID = V(game_net)$AppID,
  GameName = V(game_net)$GameName,
  Degree = degree(game_net),
  Strength = strength(game_net, weights = E(game_net)$weight),
  PageRank = page_rank(
    game_net,
    weights = E(game_net)$weight
  )$vector
) %>%
  arrange(desc(Degree))

write.csv(
  centrality,
  "../output/game_centrality.csv",
  row.names = FALSE
)

# Louvain community detection
communities <- cluster_louvain(
  game_net,
  weights = E(game_net)$weight
)

V(game_net)$community <- membership(communities)

cat("Communities:", length(communities), "\n")
cat("Modularity:", modularity(communities), "\n")

community_sizes <- tibble(
  Community = membership(communities)
) %>%
  count(Community, name = "Game.Count") %>%
  arrange(desc(Game.Count))

write.csv(
  community_sizes,
  "../output/community_sizes.csv",
  row.names = FALSE
)

# --------------------------------------------------
# 9. Create a smaller representative graph
# --------------------------------------------------

# Select the largest community
largest_community <- community_sizes$Community[1]

community_vertices <- V(game_net)[
  membership(communities) == largest_community
]

# Get the 100 highest-degree games in that community
community_degrees <- degree(game_net, v = community_vertices)

selected_vertices <- names(
  sort(community_degrees, decreasing = TRUE)
)[1:min(100, length(community_degrees))]

# Induced subgraph: only connections between selected games
small_net <- induced_subgraph(
  game_net,
  vids = selected_vertices
)

cat("Visualization nodes:", vcount(small_net), "\n")
cat("Visualization edges:", ecount(small_net), "\n")

# Plot
set.seed(42)

png(
  "../output/game_network_largest_community.png",
  width = 1600,
  height = 1200,
  res = 150
)

plot.new()
plot(
  small_net,
  layout = layout_with_fr(small_net),
  vertex.size = 5,
  vertex.label = V(small_net)$GameName,
  vertex.label.cex = 0.55,
  vertex.label.dist = 0.5,
  edge.width = 1,
  edge.color = "grey70",
  main = "Most Connected Games in the Largest Community"
)

# --------------------------------------------------
# 10. Plot community sizes
# --------------------------------------------------
plot.new()
ggplot(
  community_sizes,
  aes(
    x = reorder(factor(Community), Game.Count),
    y = Game.Count
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Steam Game Similarity Network Communities",
    x = "Community",
    y = "Number of Games"
  ) +
  theme_minimal()

ggsave(
  "../output/community_sizes.png",
  width = 9,
  height = 6
)

# Show top 20 games
print(head(centrality, 20))


# --------------------------------------------------
# COMMUNITY-LEVEL NETWORK VISUALIZATION
# --------------------------------------------------

# Assign each game to its detected community
community_membership <- membership(communities)

# Extract original game-game edges and their community IDs
edge_data <- as_data_frame(game_net, what = "edges")

edge_data$Community1 <- community_membership[
  match(edge_data$from, V(game_net)$name)
]

edge_data$Community2 <- community_membership[
  match(edge_data$to, V(game_net)$name)
]

# Remove edges within the same community
between_community_edges <- edge_data %>%
  filter(Community1 != Community2) %>%
  group_by(Community1, Community2) %>%
  summarise(
    Weight = sum(weight),
    Connection.Count = n(),
    .groups = "drop"
  )

# Create community vertices
community_nodes <- tibble(
  name = as.character(seq_along(communities)),
  Game.Count = as.numeric(sizes(communities))
)

# Build community graph
community_graph <- graph_from_data_frame(
  between_community_edges %>%
    transmute(
      from = as.character(Community1),
      to = as.character(Community2),
      weight = Weight,
      connections = Connection.Count
    ),
  directed = FALSE,
  vertices = community_nodes
)

# Remove self-loops and combine duplicate edges
community_graph <- simplify(
  community_graph,
  remove.multiple = TRUE,
  remove.loops = TRUE,
  edge.attr.comb = list(
    weight = "sum",
    connections = "sum"
  )
)

# Plot
set.seed(42)

png(
  "../output/community_network.png",
  width = 1600,
  height = 1200,
  res = 150
)

plot.new()
plot(
  community_graph,
  layout = layout_with_fr(community_graph),
  vertex.size = 5 + sqrt(V(community_graph)$Game.Count) * 1.5,
  vertex.label = paste0(
    "C", V(community_graph)$name
  ),
  vertex.label.cex = 0.8,
  vertex.color = "skyblue",
  vertex.frame.color = "grey30",
  edge.width = 1 + E(community_graph)$weight /
    max(E(community_graph)$weight) * 5,
  edge.color = "grey65",
  main = "Connections Between Steam Game Communities"
)


# --------------------------------------------------
# LARGEST COMMUNITY GAME NETWORK
# --------------------------------------------------

# Identify largest community
community_sizes <- sizes(communities)

largest_community <- as.numeric(
  names(which.max(community_sizes))
)

# Get games in largest community
largest_members <- V(game_net)[
  membership(communities) == largest_community
]

# Calculate degree within the original network
community_degree <- degree(
  game_net,
  v = largest_members
)

# Select top 100 games
selected_names <- names(
  sort(community_degree, decreasing = TRUE)
)[1:min(100, length(community_degree))]

# Create induced subgraph
focused_graph <- induced_subgraph(
  game_net,
  vids = selected_names
)

# Calculate degree within the displayed subgraph
display_degree <- degree(focused_graph)

# Label only the 15 most connected games
top_labels <- names(
  sort(display_degree, decreasing = TRUE)
)[1:min(15, vcount(focused_graph))]

V(focused_graph)$display_label <- ifelse(
  V(focused_graph)$name %in% top_labels,
  V(focused_graph)$GameName,
  NA
)

# Scale node size based on degree
node_sizes <- 4 + 2 * sqrt(display_degree)

set.seed(42)

png(
  "../output/largest_community_network.png",
  width = 1800,
  height = 1400,
  res = 160
)

plot.new()
plot(
  focused_graph,
  layout = layout_with_fr(focused_graph),
  vertex.size = node_sizes,
  vertex.label = V(focused_graph)$display_label,
  vertex.label.cex = 0.65,
  vertex.label.color = "black",
  vertex.color = "skyblue",
  vertex.frame.color = "grey30",
  edge.width = 0.5 + E(focused_graph)$weight * 4,
  edge.color = "grey70",
  main = paste(
    "Most Connected Games in Community",
    largest_community
  )
)

plot.new()
# Component size distribution
ggplot(
  component_sizes,
  aes(x = reorder(
    factor(Component),
    Game.Count
  ), y = Game.Count)
) +
  geom_col() +
  scale_y_log10() +
  labs(
    title = "Connected Component Sizes",
    subtitle = "Steam game similarity network",
    x = "Connected component",
    y = "Number of games (log scale)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

ggsave(
  "../output/component_size_distribution.png",
  width = 10,
  height = 6
)

# Extract largest connected component
largest_component_id <- component_sizes$Component[1]

largest_component <- induced_subgraph(
  game_net,
  vids = V(game_net)[
    components_result$membership == largest_component_id
  ]
)

# Detect communities within largest component
largest_louvain <- cluster_louvain(
  largest_component,
  weights = E(largest_component)$weight
)

cat("Games:", vcount(largest_component), "\n")
cat("Edges:", ecount(largest_component), "\n")
cat("Communities:", length(largest_louvain), "\n")
cat("Modularity:", modularity(largest_louvain), "\n")

# Community sizes
largest_community_sizes <- sort(
  sizes(largest_louvain),
  decreasing = TRUE
)

print(largest_community_sizes)


# --------------------------------------------------
# VISUALIZE LARGEST COMPONENT BY COMMUNITY
# --------------------------------------------------

# Assign community membership to vertices
V(largest_component)$community <- membership(largest_louvain)

# Number of communities
num_communities <- length(largest_louvain)

# Generate distinct colours for communities
community_colours <- rainbow(num_communities)

# Assign colours to nodes
node_colours <- community_colours[
  V(largest_component)$community
]

# Scale node size by degree
node_sizes <- 2 + sqrt(degree(largest_component)) * 0.5

# Generate graph
set.seed(42)

png(
  "../output/largest_component_communities.png",
  width = 1800,
  height = 1400,
  res = 160
)
plot.new()
plot(
  largest_component,
  layout = layout_with_fr(largest_component),
  vertex.size = node_sizes,
  vertex.label = NA,
  vertex.color = node_colours,
  vertex.frame.color = NA,
  edge.color = adjustcolor("grey60", alpha.f = 0.15),
  edge.width = 0.3,
  main = paste(
    "Largest Connected Component:",
    num_communities,
    "Louvain Communities"
  )
)

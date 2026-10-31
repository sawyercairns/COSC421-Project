#The purpose of this script is to perform some basic data analytics on the pre-processed data
#in order to build some intuition about relationships in the data relevant to our research
#questions. Actual network analysis will be informed by these results, and occur within a
#separate R script.


library(tidyverse)
library(lubridate)

games <- read.csv(
  "./pre-processing/games_preprocessed.csv",
  stringsAsFactors = FALSE
)

# Convert dates
games$Release.date <- mdy(games$Release.date)

# Convert review counts
games$Positive <- as.numeric(games$Positive)
games$Negative <- as.numeric(games$Negative)

# Calculate review score and genre/tag counts
games <- games %>%
  mutate(
    Total.reviews = Positive + Negative,
    
    Review.score = ifelse(
      Total.reviews > 0,
      Positive / Total.reviews * 100,
      NA
    ),
    
    Genre.count = ifelse(
      is.na(Genres) | Genres == "",
      0,
      lengths(strsplit(Genres, ","))
    ),
    
    Tag.count = ifelse(
      is.na(Tags) | Tags == "",
      0,
      lengths(strsplit(Tags, ","))
    )
  )


# General dataset statistics
cat("\n--- DATASET OVERVIEW ---\n")

cat("Total games:", nrow(games), "\n")

cat("Unique genres:",
    n_distinct(trimws(unlist(strsplit(
      na.omit(games$Genres), ","
    )))), "\n")

cat("Unique tags:",
    n_distinct(trimws(unlist(strsplit(
      na.omit(games$Tags), ","
    )))), "\n")

cat("Average genres per game:",
    round(mean(games$Genre.count), 2), "\n")

cat("Average tags per game:",
    round(mean(games$Tag.count), 2), "\n")

cat("Average review score:",
    round(mean(games$Review.score, na.rm = TRUE), 2), "%\n")

cat("Median peak CCU:",
    median(games$Peak.CCU, na.rm = TRUE), "\n")

cat("Median total reviews:",
    median(games$Total.reviews, na.rm = TRUE), "\n")

# Genre frequency
genre_counts <- games %>%
  filter(!is.na(Genres), Genres != "") %>%
  separate_rows(Genres, sep = ",") %>%
  mutate(Genres = trimws(Genres)) %>%
  count(Genres, sort = TRUE)

# Tag frequency
tag_counts <- games %>%
  filter(!is.na(Tags), Tags != "") %>%
  separate_rows(Tags, sep = ",") %>%
  mutate(Tags = trimws(Tags)) %>%
  count(Tags, sort = TRUE)

ggplot(head(genre_counts, 20),
       aes(x = reorder(Genres, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "20 Most Common Steam Genres",
    x = "Genre",
    y = "Number of Games"
  ) +
  theme_minimal()

ggplot(head(tag_counts, 25),
       aes(x = reorder(Tags, n), y = n)) +
  geom_col(fill = "darkseagreen4") +
  coord_flip() +
  labs(
    title = "25 Most Common Steam Tags",
    x = "Tag",
    y = "Number of Games"
  ) +
  theme_minimal()

# Tags per game
ggplot(games, aes(x = Tag.count)) +
  geom_histogram(
    binwidth = 2,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribution of Tags per Game",
    x = "Number of Tags",
    y = "Number of Games"
  ) +
  theme_minimal()

# Genres per game
ggplot(games, aes(x = Genre.count)) +
  geom_histogram(
    binwidth = 1,
    fill = "darkseagreen4",
    color = "white"
  ) +
  labs(
    title = "Distribution of Genres per Game",
    x = "Number of Genres",
    y = "Number of Games"
  ) +
  theme_minimal()


# Peak concurrent players
ggplot(
  games %>% filter(Peak.CCU > 0),
  aes(x = Peak.CCU)
) +
  geom_histogram(
    bins = 50,
    fill = "steelblue",
    color = "white"
  ) +
  scale_x_log10() +
  labs(
    title = "Distribution of Peak Concurrent Players",
    x = "Peak CCU (log scale)",
    y = "Number of Games"
  ) +
  theme_minimal()

# Review score distribution
ggplot(
  games %>% filter(!is.na(Review.score)),
  aes(x = Review.score)
) +
  geom_histogram(
    bins = 40,
    fill = "darkseagreen4",
    color = "white"
  ) +
  labs(
    title = "Distribution of Steam Review Scores",
    x = "Positive Reviews (%)",
    y = "Number of Games"
  ) +
  theme_minimal()

# Tag count vs popularity
ggplot(
  games %>% filter(Peak.CCU > 0),
  aes(x = Tag.count, y = Peak.CCU)
) +
  geom_point(alpha = 0.2) +
  scale_y_log10() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Tag Diversity vs Game Popularity",
    x = "Number of Tags",
    y = "Peak CCU (log scale)"
  ) +
  theme_minimal()

# Review score vs popularity
ggplot(
  games %>%
    filter(Peak.CCU > 0, !is.na(Review.score)),
  aes(x = Review.score, y = Peak.CCU)
) +
  geom_point(alpha = 0.2) +
  scale_y_log10() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Review Score vs Game Popularity",
    x = "Positive Reviews (%)",
    y = "Peak CCU (log scale)"
  ) +
  theme_minimal()

tag_prevalence <- tag_counts %>%
  mutate(
    Percentage = n / nrow(games) * 100
  )

print(head(tag_prevalence, 30))


# Calculate average popularity and number of games per genre
genre_performance <- games %>%
  filter(
    !is.na(Genres),
    Genres != "",
    !is.na(Peak.CCU)
  ) %>%
  separate_rows(Genres, sep = ",") %>%
  mutate(Genres = trimws(Genres)) %>%
  group_by(Genres) %>%
  summarise(
    Game.count = n(),
    Average.CCU = mean(Peak.CCU, na.rm = TRUE),
    Median.CCU = median(Peak.CCU, na.rm = TRUE),
    .groups = "drop"
  )

print(genre_performance)

ggplot(
  genre_performance,
  aes(x = Game.count, y = Average.CCU)
) +
  geom_point(size = 3, alpha = 0.7, color = "steelblue") +
  geom_text(
    aes(label = Genres),
    vjust = -0.8,
    size = 3,
    check_overlap = TRUE
  ) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Genre Saturation vs Average Player Interest",
    subtitle = "Relationship between genre size and average peak CCU",
    x = "Number of Games in Genre",
    y = "Average Peak Concurrent Players"
  ) +
  theme_minimal()

ggplot(
  genre_performance,
  aes(x = Game.count, y = Median.CCU)
) +
  geom_point(size = 3, alpha = 0.7, color = "darkseagreen4") +
  geom_text(
    aes(label = Genres),
    vjust = -0.8,
    size = 3,
    check_overlap = TRUE
  ) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Genre Saturation vs Median Player Interest",
    subtitle = "Median peak CCU reduces the influence of blockbuster games",
    x = "Number of Games in Genre",
    y = "Median Peak Concurrent Players"
  ) +
  theme_minimal()

# Pearson correlation (linear relationship)
cor(
  genre_performance$Game.count,
  genre_performance$Average.CCU,
  method = "pearson"
)

# Spearman correlation (monotonic relationship)
cor(
  genre_performance$Game.count,
  genre_performance$Average.CCU,
  method = "spearman"
)

cor(
  genre_performance$Game.count,
  genre_performance$Median.CCU,
  method = "pearson"
)

cor(
  genre_performance$Game.count,
  genre_performance$Median.CCU,
  method = "spearman"
)

cor.test(
  genre_performance$Game.count,
  genre_performance$Median.CCU,
  method = "spearman",
  exact = FALSE
)

genre_filtered <- genre_performance %>%
  filter(Game.count >= 30)

cor.test(
  genre_filtered$Game.count,
  genre_filtered$Median.CCU,
  method = "spearman",
  exact = FALSE
)

library(ggrepel)
ggplot(
  genre_filtered,
  aes(x = Game.count, y = Median.CCU)
) +
  geom_point(size = 3, alpha = 0.7, color = "steelblue") +
  geom_text_repel(
    aes(label = Genres),
    size = 3,
    max.overlaps = 25
  ) +
  geom_smooth(
    method = "loess",
    se = TRUE,
    color = "red"
  ) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Genre Saturation vs Median Player Interest",
    subtitle = "Genres containing at least 30 games",
    x = "Number of Games (log scale)",
    y = "Median Peak CCU (log scale)"
  ) +
  theme_minimal()

thresholds <- c(10, 30, 50, 100)

for (threshold in thresholds) {
  
  filtered <- genre_performance %>%
    filter(Game.count >= threshold)
  
  result <- cor.test(
    filtered$Game.count,
    filtered$Median.CCU,
    method = "spearman",
    exact = FALSE
  )
  
  cat(
    "\nMinimum games:", threshold,
    "\nGenres:", nrow(filtered),
    "\nCorrelation:", round(result$estimate, 3),
    "\nP-value:", round(result$p.value, 5),
    "\n"
  )
}

threshold_results <- data.frame(
  Threshold = c(10, 30, 50, 100),
  Genres = c(26, 22, 17, 13),
  Correlation = c(-0.381, -0.583, -0.421, -0.402),
  P.value = c(0.05481, 0.004419, 0.09219, 0.17286)
)

ggplot(threshold_results,
       aes(x = Threshold, y = Correlation)) +
  geom_line(color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_text(
    aes(label = paste0("p = ", P.value)),
    vjust = -1
  ) +
  scale_x_continuous(breaks = c(10, 30, 50, 100)) +
  labs(
    title = "Genre Size Threshold vs Correlation",
    x = "Minimum Games per Genre",
    y = "Spearman Correlation with Median CCU"
  ) +
  theme_minimal()

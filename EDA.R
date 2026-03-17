# Load libraries and data
install.packages(c("dplyr", "ggplot2", "readr", "readxl", "gridExtra", "grid"))
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(gridExtra)
library(grid)

getwd()
setwd("C:/Users/huuti/Downloads")
data <- read.csv("cookie_cats.csv", stringsAsFactors = FALSE)

# Data Exploration: Check missing value, summary stats and groupby
## Convert data
data$retention_1 <- as.numeric(as.logical(data$retention_1))
data$retention_7 <- as.numeric(as.logical(data$retention_7))
## Check missing value
missing_total <- sum(is.na(data))
cat("Total missing values:", missing_total, "\n")
if (missing_total > 0) {
  cat("Column-wise missing:\n")
  print(colSums(is.na(data)))
  
  # Fill missing: numeric columns with median, others with mode
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      median_val <- median(data[[col]], na.rm = TRUE)
      data[[col]][is.na(data[[col]])] <- median_val
    } else {
      mode_val <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
      data[[col]][is.na(data[[col]])] <- mode_val
    }
  }
  cat("Missing values filled!\n")
} else {
  cat("No missing values to fill.\n")
}
## Summary stats
cat("Number of Unique Users:", length(unique(data$userid)) == nrow(data), "\n")

quants <- quantile(data$sum_gamerounds, probs = c(0.01, 0.05, 0.10, 0.20, 0.80, 0.90, 0.95, 0.99), na.rm = TRUE)
summary_stats <- data %>%
  summarise(
    count = n(),
    mean = mean(sum_gamerounds, na.rm = TRUE),
    std = sd(sum_gamerounds, na.rm = TRUE),
    min = min(sum_gamerounds, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  bind_cols(as.data.frame(t(c(quants, max = max(data$sum_gamerounds, na.rm = TRUE))))) %>%
  t() %>%
  as.data.frame()
rownames(summary_stats) <- c("count", "mean", "std", "min", names(quants), "max")
colnames(summary_stats) <- "sum_gamerounds"
print(summary_stats)

##Groupy
data_summary <- data %>%
  group_by(version) %>%
  summarise(
    count = n(),
    median = median(sum_gamerounds, na.rm = TRUE),
    mean = mean(sum_gamerounds, na.rm = TRUE),
    std = sd(sum_gamerounds, na.rm = TRUE),
    max = max(sum_gamerounds, na.rm = TRUE),
    .groups = 'drop'
  )
print(data_summary)

data_counts <- data %>%
  count(sum_gamerounds, sort = FALSE) %>%
  head(20)
print(data_counts)

data_reach <- data %>%
  group_by(sum_gamerounds) %>%
  summarise(user_count = n(), .groups = 'drop') %>%
  filter(sum_gamerounds %in% c(30, 40))
print(data_reach)

ret1_count <- rev(table(data$retention_1))
ret7_count <- rev(table(data$retention_7))
n_total <- nrow(data)
ret_df <- data.frame(
  RET1_COUNT = as.numeric(ret1_count),
  RET7_COUNT = as.numeric(ret7_count),
  RET1_RATIO = as.numeric(ret1_count) / n_total,
  RET7_RATIO = as.numeric(ret7_count) / n_total,
  row.names = names(ret1_count)
)
print(ret_df)

data_ret1_summary <- data %>%
  group_by(version, retention_1) %>%
  summarise(
    count = n(),
    median = median(sum_gamerounds, na.rm = TRUE),
    mean = mean(sum_gamerounds, na.rm = TRUE),
    std = sd(sum_gamerounds, na.rm = TRUE),
    max = max(sum_gamerounds, na.rm = TRUE),
    .groups = 'drop'
  )
print(data_ret1_summary)

data_ret7_summary <- data %>%
  group_by(version, retention_7) %>%
  summarise(
    count = n(),
    median = median(sum_gamerounds, na.rm = TRUE),
    mean = mean(sum_gamerounds, na.rm = TRUE),
    std = sd(sum_gamerounds, na.rm = TRUE),
    max = max(sum_gamerounds, na.rm = TRUE),
    .groups = 'drop'
  )
print(data_ret7_summary)

# Data Cleaning: Remove extreme values and create new columns
##Remove
data <- data[data$sum_gamerounds < max(data$sum_gamerounds, na.rm = TRUE), ]
##Create
data <- data %>%
  mutate(
    Retention = ifelse(retention_1 == TRUE & retention_7 == TRUE, 1, 0),
    NewRetention = paste0(as.character(retention_1), "-", as.character(retention_7))
  )

data_ret_summary <- data %>%
  group_by(version, Retention) %>%
  summarise(
    count = n(),
    median = median(sum_gamerounds, na.rm = TRUE),
    mean = mean(sum_gamerounds, na.rm = TRUE),
    std = sd(sum_gamerounds, na.rm = TRUE),
    max = max(sum_gamerounds, na.rm = TRUE),
    .groups = 'drop'
  )
print(data_ret_summary)

data_newret_summary <- data %>%
  group_by(version, NewRetention) %>%
  summarise(
    count = n(),
    median = median(sum_gamerounds, na.rm = TRUE),
    mean = mean(sum_gamerounds, na.rm = TRUE),
    std = sd(sum_gamerounds, na.rm = TRUE),
    max = max(sum_gamerounds, na.rm = TRUE),
    .groups = 'drop'
  )
print(data_newret_summary)

quants_clean <- quantile(data$sum_gamerounds, probs = c(0.01, 0.05, 0.10, 0.20, 0.80, 0.90, 0.95, 0.99), na.rm = TRUE)
summary_stats_clean <- data %>%
  summarise(
    count = n(),
    mean = mean(sum_gamerounds, na.rm = TRUE),
    std = sd(sum_gamerounds, na.rm = TRUE),
    min = min(sum_gamerounds, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  bind_cols(as.data.frame(t(c(quants_clean, max = max(data$sum_gamerounds, na.rm = TRUE))))) %>%
  t() %>%
  as.data.frame()
rownames(summary_stats_clean) <- c("count", "mean", "std", "min", names(quants_clean), "max")
colnames(summary_stats_clean) <- "sum_gamerounds"
print(summary_stats_clean)

data_summary_clean <- data %>%
  group_by(version) %>%
  summarise(
    count = n(),
    median = median(sum_gamerounds, na.rm = TRUE),
    mean = mean(sum_gamerounds, na.rm = TRUE),
    std = sd(sum_gamerounds, na.rm = TRUE),
    max = max(sum_gamerounds, na.rm = TRUE),
    .groups = 'drop'
  )
print(data_summary_clean)

# Visualization: Histograms, boxplots, line plots, and user counts

# Filter groups for histograms
gate_30 <- data[data$version == "gate_30", ]
gate_40 <- data[data$version == "gate_40", ]

# Plot 1: Histogram for Gate 30 (A) group
p1 <- ggplot(gate_30, aes(x = sum_gamerounds)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
  labs(title = "Distribution of Gate 30 (A)", x = "Sum Game Rounds", y = "Frequency") +
  theme_minimal() + theme(plot.title = element_text(size = 15, hjust = 0.5))
print(p1)
# Plot 2: Histogram for Gate 40 (B) group
p2 <- ggplot(gate_40, aes(x = sum_gamerounds)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
  labs(title = "Distribution of Gate 40 (B)", x = "Sum Game Rounds", y = "Frequency") +
  theme_minimal() + theme(plot.title = element_text(size = 15, hjust = 0.5))
print(p2)
# Plot 3: Boxplot comparing both groups
p3 <- ggplot(data, aes(x = version, y = sum_gamerounds, fill = version)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("gate_30" = "steelblue", "gate_40" = "steelblue")) +
  labs(title = "Distribution of Two Groups", x = "Version", y = "Sum Game Rounds") +
  theme_minimal() + theme(plot.title = element_text(size = 15, hjust = 0.5), legend.position = "none")
print(p3)
# Combine Plots 1-3 horizontally (hist A, hist B, boxplot)
grid.arrange(p1, p2, p3, nrow = 1, top = textGrob("After Removing The Extreme Value", gp = gpar(fontsize = 20, fontface = "bold")))

# Plot 4: Line plot of sum_gamerounds vs index by version (after cleaning)
data_with_index <- data %>%
  group_by(version) %>%
  mutate(
    index = row_number(),
    alpha_val = ifelse(version == "gate_30", 1, 0.8)
  ) %>%
  ungroup()

p_line_after <- ggplot(data_with_index, aes(x = index, y = sum_gamerounds, color = version, alpha = alpha_val)) + 
  geom_line(linewidth = 0.5) +  # Thay size bằng linewidth để fix warning
  scale_color_manual(values = c("gate_30" = "steelblue", "gate_40" = "yellow"), 
                     labels = c("gate_30" = "Gate 30", "gate_40" = "Gate 40")) +
  scale_alpha_identity() +  # Giữ nguyên giá trị alpha từ cột (không scale)
  labs(title = "After Removing The Extreme Value", x = "Index", y = "Sum Game Rounds", color = "Version") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), legend.position = "top")
print(p_line_after)
# Plot 5: User counts per sum_gamerounds (full range)
counts_full <- data %>%
  count(sum_gamerounds, sort = TRUE) %>%
  arrange(sum_gamerounds)
p1_full <- ggplot(counts_full, aes(x = sum_gamerounds, y = n)) +
  geom_line(color = "steelblue", size = 0.8) + geom_point(color = "orange", size = 0.5) +
  labs(title = "How many users are there all game rounds?", x = "Sum Game Rounds", y = "Number of Users") +
  theme_minimal() + theme(plot.title = element_text(size = 15, hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
print(p1_full)
# Plot 6: User counts per sum_gamerounds (first 200 rounds only)
counts_200 <- counts_full %>% filter(sum_gamerounds <= 200)
p2_200 <- ggplot(counts_200, aes(x = sum_gamerounds, y = n)) +
  geom_line(color = "steelblue", size = 0.8) + geom_point(color = "red", size = 0.5) +
  labs(title = "How many users are there first 200 game rounds?", x = "Sum Game Rounds", y = "Number of Users") +
  theme_minimal() + theme(plot.title = element_text(size = 15, hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
print(p2_200)
# Combine Plots 5-6 vertically (full vs first 200 user counts)
grid.arrange(p1_full, p2_200, ncol = 1, top = textGrob("The number of users in the game rounds played", gp = gpar(fontsize = 25, fontface = "bold")))
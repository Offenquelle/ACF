#### Number of rating changes by each CRA ####
# Separate data for Moody's and Fitch
MOODYS_DESCRIPTIVES <- ALL_RATINGS %>%
  filter(`Rating Source/Description` == "Moody's Long-term Issuer Rating")
FITCH_DESCRIPTIVES <- ALL_RATINGS %>%
  filter(`Rating Source/Description` == "Fitch Long-term Issuer Default Rating")

# Separate action types and calculate counts
MOODYS_DESCRIPTIVES <- MOODYS_DESCRIPTIVES %>%
  group_by(`Action Type`) %>%
  summarise(Action_Count = n())
FITCH_DESCRIPTIVES <- FITCH_DESCRIPTIVES %>%
  group_by(`Action Type`) %>%
  summarise(Action_Count = n())

# Combine descriptive statistics into one table
RATING_DESCRIPTIVES <- MOODYS_DESCRIPTIVES %>%
  rename(Moodys_Action_Count = Action_Count) %>%
  full_join(FITCH_DESCRIPTIVES %>% rename(Fitch_Action_Count = Action_Count), by = "Action Type")
RATING_DESCRIPTIVES$ALL_Action_Count <- rowSums(RATING_DESCRIPTIVES[, c("Moodys_Action_Count", "Fitch_Action_Count")], na.rm = TRUE)
colnames(RATING_DESCRIPTIVES) <- c("Action Type", "Moody's", "Fitch", "Total")

# Save descriptive statistics to CSV
write.csv(RATING_DESCRIPTIVES, "Rating_Descriptive_Statistics.csv", row.names = FALSE)

#### Distribution of rating change grades ####
# Create histogram of rating changes
ggplot(ALL_RATINGS, aes(x = as.numeric(Rating_Change))) +
  geom_histogram(binwidth = 1, 
                 fill = "lightblue", 
                 color = "black", 
                 boundary = 0) +
  labs(title = "Distribution of Rating Changes",
       x = "Rating Change (Numeric Scale)",
       y = "Frequency") +
  theme_minimal()
ggsave("Rating_Change_Distribution.png", width = 8, height = 6)

#### Time series of rating changes ####
# Convert 'Effective Date' to Date type
ALL_RATINGS$`Effective Date` <- as.Date(ALL_RATINGS$`Effective Date`, format = "%Y-%m-%d")

# Create time series plot of rating changes over time
ggplot(ALL_RATINGS, aes(x = `Effective Date`)) +
  geom_histogram(binwidth = 30, 
                 fill = "lightgreen", 
                 color = "black", 
                 boundary = 0) +
  labs(title = "Time Series of Rating Changes",
       x = "Date",
       y = "Number of Rating Changes") +
  theme_minimal()
ggsave("Rating_Changes_Time_Series.png", width = 8, height = 6)

#### Plot average cumulative abnormal returns ####
# Calculate average CARs for upgrades and downgrades
ALL_RESULTS <- ALL_ABNORMAL_RETURNS %>%
  group_by(Rating_Direction, date_numeric) %>%
  summarise(Avg_CAR = mean(CAR, na.rm = TRUE))

MOODYS_RESULTS <- MOODYS_ABNORMAL_RETURNS %>%
  group_by(Rating_Direction, date_numeric) %>%
  summarise(Avg_CAR = mean(CAR, na.rm = TRUE))

FITCH_RESULTS <- FITCH_ABNORMAL_RETURNS %>%
  group_by(Rating_Direction, date_numeric) %>%
  summarise(Avg_CAR = mean(CAR, na.rm = TRUE))

# Calculate average t-statistics and p-values for completeness in one dateframe
ALL_STATS <- ALL_ABNORMAL_RETURNS %>%
  group_by(Rating_Direction, date_numeric) %>%
  summarise(Avg_t_stat = mean(t_stat, na.rm = TRUE),
            Avg_p_value = mean(p_value, na.rm = TRUE))

MOODYS_STATS <- MOODYS_ABNORMAL_RETURNS %>%
  group_by(Rating_Direction, date_numeric) %>%
  summarise(Avg_t_stat = mean(t_stat, na.rm = TRUE),
            Avg_p_value = mean(p_value, na.rm = TRUE))

FITCH_STATS <- FITCH_ABNORMAL_RETURNS %>%
  group_by(Rating_Direction, date_numeric) %>%
  summarise(Avg_t_stat = mean(t_stat, na.rm = TRUE),
            Avg_p_value = mean(p_value, na.rm = TRUE))

# Merge average CARs with average statistics
ALL_RESULTS <- ALL_RESULTS %>%
  left_join(ALL_STATS, by = c("Rating_Direction", "date_numeric"))

MOODYS_RESULTS <- MOODYS_RESULTS %>%
  left_join(MOODYS_STATS, by = c("Rating_Direction", "date_numeric"))

FITCH_RESULTS <- FITCH_RESULTS %>%
  left_join(FITCH_STATS, by = c("Rating_Direction", "date_numeric"))

# Plot the average CARs
ggplot(ALL_RESULTS, aes(x = date_numeric, y = Avg_CAR, color = Rating_Direction)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Average Cumulative Abnormal Returns Around Rating Changes",
       x = "Days Relative to Rating Change",
       y = "Average Cumulative Abnormal Return (CAR)",
       color = "Rating Direction") +
  theme_minimal() +
  scale_x_continuous(breaks = c(-1, 0, 1), labels = c("-1", "0", "1")) +
  theme(legend.position = "top")
ggsave("Average_CARs_Rating_Changes.png", width = 8, height = 6)

ggplot(MOODYS_RESULTS, aes(x = date_numeric, y = Avg_CAR, color = Rating_Direction)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Average Cumulative Abnormal Returns Around Moody's Rating Changes",
       x = "Days Relative to Rating Change",
       y = "Average Cumulative Abnormal Return (CAR)",
       color = "Rating Direction") +
  theme_minimal() +
  scale_x_continuous(breaks = c(-1, 0, 1), labels = c("-1", "0", "1")) +
  theme(legend.position = "top")
ggsave("Average_CARs_Moodys_Rating_Changes.png", width = 8, height = 6)

ggplot(FITCH_RESULTS, aes(x = date_numeric, y = Avg_CAR, color = Rating_Direction)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Average Cumulative Abnormal Returns Around Fitch Rating Changes",
       x = "Days Relative to Rating Change",
       y = "Average Cumulative Abnormal Return (CAR)",
       color = "Rating Direction") +
  theme_minimal() +
  scale_x_continuous(breaks = c(-1, 0, 1), labels = c("-1", "0", "1")) +
  theme(legend.position = "top")
ggsave("Average_CARs_Fitch_Rating_Changes.png", width = 8, height = 6)


#### Result table for average CARs  at t = 1 with t-statistics and p-values ####
RESULT_TABLE <- Avg_CARs %>%
  filter(date_numeric == 1) %>%
  select(Rating_Direction, Avg_CAR, Avg_t_stat, Avg_p_value) %>%
  rename(
    `Rating Direction` = Rating_Direction,
    `Average CAR at t=1` = Avg_CAR,
    `Average t-statistic` = Avg_t_stat,
    `Average p-value` = Avg_p_value
  )

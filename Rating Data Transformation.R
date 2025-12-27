#### Setting the working directory and loading packages ####
# Set your working directory before running the script
setwd("~/R/ACF")

# Make sure to install the following packages
library(quantmod)
library(readxl)
library(dplyr)
library(data.table)
library(writexl)
library(ggplot2)
library(moments)
library(gganimate)

#### Merge and transform monthly rating change data from Refinitiv ####
# Merge multiple excel files into one dataframe
RATINGS_LIST <- list.files(pattern = "*.xlsx")

for (i in 1:length(RATINGS_LIST)) {
  temp <- read_excel(RATINGS_LIST[i], 
                     col_types = c("date", "text", "text", "text", "text", "text", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "text"))
  if (i == 1) {
    RATING_DATA <- temp
  } else {
    RATING_DATA <- rbind(RATING_DATA, temp)
  }
}
rm(temp, RATINGS_LIST, i)

# Exclude NAs, private companies, domestic currency ratings and keep actual rating changes
RATING_DATA <- na.omit(RATING_DATA)
RATING_DATA <- RATING_DATA %>%
  filter(`Private Company` == "No")
RATING_DATA <- RATING_DATA %>%
  filter(`Action Type` %in% c("Rating Upgraded", "Rating Downgraded"))
RATING_DATA <- RATING_DATA %>%
  filter(`Rating Scope` == "Foreign")

# Add or RICs that are wrong or not included
RATING_DATA$`Equity RIC`[RATING_DATA$`Equity RIC` == "BRKa"] <- "BRK-A"
RATING_DATA$`Equity RIC`[RATING_DATA$`Equity RIC` == "CMS_pb"] <- "CMS-PB"
RATING_DATA$`Equity RIC`[RATING_DATA$`Equity RIC` == "ATH_pa"] <- "ATH-PA"
RATING_DATA$`Equity RIC`[RATING_DATA$`Equity RIC` == "IPG^K25"] <- "IPG"
RATING_DATA$`Equity RIC`[RATING_DATA$`Equity RIC` == "CTA_pa"] <- "CTA-PA"
RATING_DATA$`Equity RIC`[RATING_DATA$`Equity RIC` == "OAK_pa"] <- "OAK-PA"
RATING_DATA$`Equity RIC`[RATING_DATA$`Equity RIC` == "PCG_pa"] <- "PCG-PA"
RATING_DATA$`Equity RIC`[RATING_DATA$`Equity RIC` == "K^L25"] <- "K"

RATING_DATA <- RATING_DATA %>%
  filter(`Equity RIC` != "WWOK.PK")
RATING_DATA <- RATING_DATA %>%
  filter(`Equity RIC` != "WNDHU.PK")
RATING_DATA <- RATING_DATA %>%
  filter(`Equity RIC` != "COOP.O^J25")
RATING_DATA <- RATING_DATA %>%
  filter(`Equity RIC` != "STRZ.O")
RATING_DATA <- RATING_DATA %>%
  filter(`Equity RIC` != "ANG_pb^J25")

# Extract unique tickers from equity RIC codes
get_ticker <- function(ric) {
  sub("\\..*$", "", ric)
}
RATING_DATA$ticker <- get_ticker(RATING_DATA$`Equity RIC`)

#### Adding numeric scales and calculating rating changes ####
# Numeric scale for ratings
MOODYS_SCALE <- data.frame(
  Rating = c("Aaa", "Aa1", "Aa2", "Aa3", "A1", "A2", "A3", "Baa1", "Baa2", "Baa3",
             "Ba1", "Ba2", "Ba3", "B1", "B2", "B3", "Caa1", "Caa2", "Caa3", "Ca", "C"),
  Numeric_Score = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
)

FITCH_SCALE <- data.frame(
  Rating = c("AAA", "AA+", "AA", "AA-", "A+", "A", "A-", "BBB+", "BBB", "BBB-",
             "BB+", "BB", "BB-", "B+", "B", "B-", "CCC+", "CCC", "CCC-", "CC", "C", "RD", "D"),
  Numeric_Score = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
)

# Create subsamples of Moody's and Fitch ratings
MOODYS_RATINGS <- RATING_DATA %>%
  filter(`Rating Source/Description` == "Moody's Long-term Issuer Rating")
FITCH_RATINGS <- RATING_DATA %>%
  filter(`Rating Source/Description` == "Fitch Long-term Issuer Default Rating")

# Map ratings to numeric scores
MOODYS_RATINGS <- MOODYS_RATINGS %>%
  left_join(MOODYS_SCALE, by = c("Current Rating" = "Rating")) %>%
  left_join(MOODYS_SCALE, by = c("Previous Rating" = "Rating"), suffix = c("_Current", "_Previous"))

FITCH_RATINGS <- FITCH_RATINGS %>%
  left_join(FITCH_SCALE, by = c("Current Rating" = "Rating")) %>%
  left_join(FITCH_SCALE, by = c("Previous Rating" = "Rating"), suffix = c("_Current", "_Previous"))

# Calculate rating changes
MOODYS_RATINGS <- MOODYS_RATINGS %>%
  mutate(Rating_Change = Numeric_Score_Previous - Numeric_Score_Current)

FITCH_RATINGS <- FITCH_RATINGS %>%
  mutate(Rating_Change = Numeric_Score_Previous - Numeric_Score_Current)

# Merge both CRAs data frames into one data frame and remove temporary data frames
ALL_RATINGS <- rbind(MOODYS_RATINGS, FITCH_RATINGS)
rm(RATING_DATA, MOODYS_RATINGS, FITCH_RATINGS)



#### Loading data and libraries ####
setwd("~/R/ACF")
library(quantmod)
library(readxl)
library(dplyr)

#### Data Transformation ####
# Merge multiple excel files into one dataframe
RATING_LIST <- list.files(pattern = "*.xlsx")

for (i in 1:length(RATING_LIST)) {
  temp <- read_excel(RATING_LIST[i], col_types = c("date", "text", "text", "text", "text", "text", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "text"))
  if (i == 1) {
    RATING_DATA <- temp
  } else {
    RATING_DATA <- rbind(RATING_DATA, temp)
  }
}

# Exclude NAs & private companies
RATING_DATA <- na.omit(RATING_DATA)
RATING_DATA <- RATING_DATA %>%
  filter(`Private Company` == "No")

# Create subsamples of Moody's and Fitch ratings
MOODYS_DATA <- RATING_DATA %>%
  filter(`Rating Source/Description` == "Moody's Long-term Issuer Rating")
FITCH_DATA <- RATING_DATA %>%
  filter(`Rating Source/Description` == "Fitch Long-term Issuer Default Rating")

# Create descriptive statistics for both CRAs
MOODYS_Descriptives <- MOODYS_DATA %>%
  group_by(`Action Type`) %>%
  summarise(Action_Count = n())

FITCH_Descriptives <- FITCH_DATA %>%
  group_by(`Action Type`) %>%
  summarise(Action_Count = n())




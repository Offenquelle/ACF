#### Calculate log returns ####
# Load stock prices, convert date column to Date type, remove x column
STOCK_PRICES <- read.csv("Stock_Prices.csv",
                         sep = ",",
                         header = TRUE)
STOCK_PRICES$date <- as.Date(STOCK_PRICES$date, format("%Y-%m-%d"))

STOCK_PRICES <- STOCK_PRICES %>%
  select(-X)

# Load Russel 3000 prices, convert date column to Date type, remove x column
RUSSEL3000_PRICES <- read.csv("Russel3000_Prices.csv",
                              sep = ",",
                              header = TRUE)
RUSSEL3000_PRICES$date <- as.Date(RUSSEL3000_PRICES$date, format("%Y-%m-%d"))
RUSSEL3000_PRICES <- RUSSEL3000_PRICES %>%
  select(-X)

# Delete ".Adjusted" in column names for easier handling
colnames(STOCK_PRICES) <- gsub("\\.Adjusted", "", colnames(STOCK_PRICES))
colnames(RUSSEL3000_PRICES) <- gsub("\\.Adjusted", "", colnames(RUSSEL3000_PRICES))

# Calculate daily log returns for all stocks
STOCK_RETURNS <- data.frame(Date = STOCK_PRICES$date)
for (col in colnames(STOCK_PRICES)[-1]) {
  STOCK_RETURNS[[col]] <- c(NA, diff(log(STOCK_PRICES[[col]])))
}
rm(col)

# Delete first row and delete all stock observations with NAs
STOCK_RETURNS <- STOCK_RETURNS[-1, ]
STOCK_RETURNS <- STOCK_RETURNS %>% select_if(~ !any(is.na(.)))

# Calculate daily log returns for Russel 3000
RUSSEL3000_RETURNS <- data.frame(Date = RUSSEL3000_PRICES$date)
RUSSEL3000_RETURNS$R_RUA <- c(NA, diff(log(RUSSEL3000_PRICES$RUA)))
RUSSEL3000_RETURNS <- RUSSEL3000_RETURNS[-1, ]

# Exclude tickers that have no stock return data
ALL_RATINGS <- ALL_RATINGS %>%
  filter(ticker %in% colnames(STOCK_RETURNS))

#### Abnormal return calculation function ####
calculate_abnormal_returns <- function(ticker, event_date_0) {
  
  # Calculate dates and windows
  event_window <- 1
  event_start <- event_date_0 - event_window
  event_end <- event_date_0 + event_window
  event_estimation_gap <- 10
  estimation_window <- 180
  estimation_end <- event_start - event_estimation_gap
  estimation_start <- estimation_end - estimation_window
  
  # Extract returns for the stock and Russel 3000 during estimation window
  est_returns_stock <- subset(STOCK_RETURNS, 
                              Date >= estimation_start & Date <= estimation_end, 
                              select = c("Date", ticker))
  est_returns_RUA <- subset(RUSSEL3000_RETURNS, 
                            Date >= estimation_start & Date <= estimation_end, 
                            select = c("Date", "R_RUA"))
  
  # Run market model regression
  est_reg <- lm(est_returns_stock[[2]] ~ est_returns_RUA$R_RUA)
  
  # Extract returns for the stock and Russel 3000 during event window
  event_returns_stock <- subset(STOCK_RETURNS, 
                                Date >= event_start & Date <= event_end, 
                                select = c("Date", ticker))
  event_returns_RUA <- subset(RUSSEL3000_RETURNS, 
                              Date >= event_start & Date <= event_end, 
                              select = c("Date", "R_RUA"))
  
  # Calculate abnormal returns
  AR <- event_returns_stock[[2]] - (est_reg$coefficients[1] + est_reg$coefficients[2] * event_returns_RUA$R_RUA)
  AR_df <- data.frame(Date = event_returns_stock$Date, Abnormal_Return = AR)
  AR_df$CAR <- cumsum(AR_df$Abnormal_Return)
  
  # Calculate t-statistics and p-values for abnormal returns
  if (nrow(event_returns_stock) == 0) {
    return(data.frame())  # Return empty dataframe if insufficient data
  }
  else
    AR_df$se <- summary(est_reg)$sigma
    AR_df$t_stat <- AR_df$Abnormal_Return / AR_df$se
    AR_df$p_value <- 2 * pt(-abs(AR_df$t_stat), df = summary(est_reg)$df[2])
    
  # Standardized times for all observations
  AR_df
  date_matching <- data.frame(
    date = as.Date(c(event_date_0 - 1, event_date_0, event_date_0 + 1)),
    date_numeric = c(-1,0,1)
  )
  AR_df <- AR_df %>%
    left_join(date_matching, by = c("Date" = "date"))
  
  return(AR_df)
}

#### Calculate abnormal returns for all stocks ####
ALL_ABNORMAL_RETURNS <- data.frame()
ALL_RATINGS <- as.matrix(ALL_RATINGS)

for (i in 1:nrow(ALL_RATINGS)){
  t <- ALL_RATINGS[i,14]
  d <- ALL_RATINGS[i,1]
  nam <- paste0("result", i)
  assign(nam, calculate_abnormal_returns(t, as.Date(d)))
  if(nrow(get(nam)) == 0){
    print(paste("No data for index:", i))
    rm(list = nam)
    next
  }
  else
    assign(nam, calculate_abnormal_returns(t, as.Date(d)))
    temp <- get(nam)
    temp$Ticker <- ALL_RATINGS[i,14]
    temp$Rating_Direction <- ALL_RATINGS[i,8]
    temp$CRA <- ALL_RATINGS[i,6]
    ALL_ABNORMAL_RETURNS <- rbind(ALL_ABNORMAL_RETURNS, temp)
    rm(list = nam)
}
rm(i, nam, temp, t, d)
ALL_RATINGS <- as.data.frame(ALL_RATINGS)

# Save Moodys and Fitch abnormal returns
MOODYS_ABNORMAL_RETURNS <- ALL_ABNORMAL_RETURNS %>%
  filter(CRA == "Moody's Long-term Issuer Rating")
FITCH_ABNORMAL_RETURNS <- ALL_ABNORMAL_RETURNS %>%
  filter(CRA == "Fitch Long-term Issuer Default Rating")
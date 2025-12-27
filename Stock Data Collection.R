#### Obtain stock price data ####
# Load unique tickers into a data frame
Stock_Data <- data.frame(ticker = unique(c(MOODYS_RATINGS$ticker, FITCH_DATA$ticker)))
Stock_Data <- transpose(Stock_Data)
colnames(Stock_Data) <- Stock_Data[1, ]
Stock_Data <- Stock_Data[-1, , drop = FALSE]

# Obtain stock price data for each ticker and import in stock data frame
start <- as.Date("2020-01-01")
end <- as.Date("2024-12-31")
tickers <- colnames(Stock_Data)
Stock_Prices <- c()

for (i in 1:length(tickers)) {
  if (i == 1){
    temp <- getSymbols(tickers[i],
                       src = "yahoo",
                       from = start,
                       to = end,
                       auto.assign = FALSE)[1, 6]
  } 
  else {
    temp <- getSymbols(tickers[i],
                       src = "yahoo",
                       from = start,
                       to = end,
                       auto.assign = FALSE)[, 6]
  }
  Stock_Prices <- merge.xts(Stock_Prices, temp)
  if (i %% 20 == 0) {
    Sys.sleep(15)  # Pause for 5 seconds after every 10 requests
  }
}
rm(temp, i, tickers)
write.csv(data.frame(date = index(Stock_Prices), coredata(Stock_Prices)), "Stock_Prices.csv")

#### Obtain price data for Russel 3000 index ####
Russel3000_Prices <- getSymbols("^RUA",
                               src = "yahoo",
                               from = start,
                               to = end,
                               auto.assign = FALSE)[, 6]
write.csv(data.frame(date = index(Russel3000_Prices), coredata(Russel3000_Prices)), "Russel3000_Prices.csv")
library(quantmod)
library(TTR)
library(tidyverse)

# Function to load stock data
load_stock_data <- function(file_name) {
  
  symbols <- readLines(file_name)
  
  stock_data <- list()
  
  for(symbol in symbols) {
    
    stock_df <- getSymbols(
      symbol,
      src = "yahoo",
      auto.assign = FALSE
    )
    
    stock_data[[symbol]] <- stock_df
  }
  
  return(stock_data)
}

# Load stock data
stocks <- load_stock_data("portfolio.txt")

# Show stock names
names(stocks)

# Function to calculate statistics
calculate_statistics <- function(stock_df) {
  
  close_prices <- Cl(stock_df)
  
  stats <- list(
    Mean = mean(close_prices, na.rm = TRUE),
    Median = median(close_prices, na.rm = TRUE),
    SD = sd(close_prices, na.rm = TRUE),
    MovingAverage = SMA(close_prices, n = 20)
  )
  
  return(stats)
}
# Calculate statistics for Apple
apple_stats <- calculate_statistics(stocks[["AAPL"]])

apple_stats

# Display first few rows of Apple stock data
head(stocks[["AAPL"]])

# Plot Apple stock closing prices
chartSeries(stocks[["AAPL"]],
            theme = chartTheme("white"),
            name = "Apple Stock Price")

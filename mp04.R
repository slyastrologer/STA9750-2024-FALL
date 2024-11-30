library(dplyr)
library(httr)
library(knitr)
library(readr)
library(stringr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(sf)





### TASK 1: Register for Alpha Vantage API Key ###



### TASK 2: Register for FRED API Key ###



### TASK 3: Data Acquisition ###
# Load required libraries
library(httr)
library(jsonlite)

## WAGE GROWTH ##
# Define the FRED API endpoint and the FRED API key
api_key <- "ffc31969aa3d7177ca28636e5b65c963"
base_url <- "https://api.stlouisfed.org/fred/"

# Define the FRED series ID for the data you want to download
series_id <- "NYWTOT"

# Build the URL to get the data for a specific series
url <- paste0(base_url, "series/observations")

# Define the query parameters
params <- list(
  series_id = series_id,        # The series ID
  api_key = api_key,            # The API key
  file_type = "json",           # Specify the file type as JSON
  frequency = "q",              # Data frequency
  units = "lin",                # Linear units
  start_date = "2014-04-01",    # Start date
  end_date = "2024-04-01"       # End date
)

# Make the GET request to the FRED API
response <- GET(url, query = params)

# Check if the request was successful (status code 200 means OK)
if (status_code(response) == 200) {
  # Parse the JSON response
  data <- fromJSON(content(response, "text"))
  # Extract the observations
  observations <- data$observations
  # Convert to a data frame
  data_wagegrowth <- as.data.frame(observations)
  # Display the first few rows of the data
  head(data_wagegrowth)
  # Save the data to a CSV file
  write.csv(data_wagegrowth, "fred_data_wagegrowth.csv", row.names = FALSE)
  # If the request succeeds, print a download message
  cat("Data has been successfully downloaded and saved as 'fred_data_wagegrowth.csv'.\n")
} else {
  # If the request fails, print an error message
  cat("Failed to retrieve data. Status code:", status_code(response), "\n")
}


## INFLATION ##
# Define the FRED API endpoint and the FRED API key
api_key <- "ffc31969aa3d7177ca28636e5b65c963"
base_url <- "https://api.stlouisfed.org/fred/"

# Define the FRED series ID for the data you want to download
series_id <- "CPALTT01USQ657N"

# Build the URL to get the data for a specific series
url <- paste0(base_url, "series/observations")

# Define the query parameters
params <- list(
  series_id = series_id,        # The series ID
  api_key = api_key,            # The API key
  file_type = "json",           # Specify the file type as JSON
  frequency = "q",              # Data frequency
  units = "lin",                # Linear units
  start_date = "2014-04-01",    # Start date
  end_date = "2024-04-01"       # End date
)

# Make the GET request to the FRED API
response <- GET(url, query = params)

# Check if the request was successful (status code 200 means OK)
if (status_code(response) == 200) {
  # Parse the JSON response
  data <- fromJSON(content(response, "text"))
  # Extract the observations
  observations <- data$observations
  # Convert to a data frame
  data_inflation <- as.data.frame(observations)
  # Display the first few rows of the data
  head(data_inflation)
  # Save the data to a CSV file
  write.csv(data_inflation, "fred_data_inflation.csv", row.names = FALSE)
  # If the request succeeds, print a download message
  cat("Data has been successfully downloaded and saved as 'fred_data_inflation.csv'.\n")
} else {
  # If the request fails, print an error message
  cat("Failed to retrieve data. Status code:", status_code(response), "\n")
}


## US EQUITY MARKET TOTAL RETURNS ##
# Define the Alpha Vantage API key and base URL
api_key <- "13OR5QPKL3DGT9U6"  # Alpha Vantage API key
symbol <- "SPY"  # S&P 500 ETF
function_type <- "TIME_SERIES_MONTHLY"  # Monthly historical data
output_size <- "full"  # Full time series

# Construct the API request URL
url <- paste0("https://www.alphavantage.co/query?function=", function_type,
              "&symbol=", symbol, 
              "&apikey=", api_key, 
              "&outputsize=", output_size)

# Make the API request
response <- GET(url)

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the JSON content of the response
  data <- content(response, "parsed", type = "application/json")
  # Extract the historical time series data (if present)
  if (function_type == "TIME_SERIES_MONTHLY") {
    time_series <- data[["Monthly Time Series"]]
    # Convert the time series into a data frame
    if (!is.null(time_series)) {
      # Convert the list to a data frame where each date is a row
      data_sap500 <- as.data.frame(do.call(rbind, lapply(time_series, unlist)))
      # Set column names for open, high, low, close, volume
      colnames(data_sap500) <- c("open", "high", "low", "close", "volume")
      # Convert the row names to a date column
      data_sap500$date <- rownames(data_sap500)
      rownames(data_sap500) <- NULL  # Remove row names
      # Ensure the date column is in Date format (using the first of each month)
      data_sap500$date <- as.Date(paste0(data_sap500$date, "-01"))
      # Print out the first few rows of the data frame
      print(head(data_sap500))
    } else {
      print("Error: No time series data found.")
    }
  }
} else {
  # If the request was unsuccessful, print the error message
  print(paste("Request failed with status:", status_code(response)))
  print(content(response, "text"))
}


## INTL EQUITY MARKET TOTAL RETURNS ##







## BOND MARKET TOTAL RETURNS ##
# Define the Alpha Vantage API key and base URL
api_key <- "13OR5QPKL3DGT9U6"  # Alpha Vantage API key
symbol <- "AGG"  # Bond Markets ETF
function_type <- "TIME_SERIES_MONTHLY"  # Monthly historical data
output_size <- "full"  # Full time series

# Construct the API request URL
url <- paste0("https://www.alphavantage.co/query?function=", function_type,
              "&symbol=", symbol, 
              "&apikey=", api_key, 
              "&outputsize=", output_size)

# Make the API request
response <- GET(url)

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the JSON content of the response
  data <- content(response, "parsed", type = "application/json")
  # Extract the historical time series data (if present)
  if (function_type == "TIME_SERIES_MONTHLY") {
    time_series <- data[["Monthly Time Series"]]
    # Convert the time series into a data frame
    if (!is.null(time_series)) {
      # Convert the list to a data frame where each date is a row
      data_bondreturns <- as.data.frame(do.call(rbind, lapply(time_series, unlist)))
      # Set column names for open, high, low, close, volume
      colnames(data_bondreturns) <- c("open", "high", "low", "close", "volume")
      # Convert the row names to a date column
      data_bondreturns$date <- rownames(data_bondreturns)
      rownames(data_bondreturns) <- NULL  # Remove row names
      # Ensure the date column is in Date format (using the first of each month)
      data_bondreturns$date <- as.Date(paste0(data_bondreturns$date, "-01"))
      # Print out the first few rows of the data frame
      print(head(data_bondreturns))
    } else {
      print("Error: No time series data found.")
    }
  }
} else {
  # If the request was unsuccessful, print the error message
  print(paste("Request failed with status:", status_code(response)))
  print(content(response, "text"))
}


## SHORT-TERM DEBT RETURNS ##
## 
## 
## 
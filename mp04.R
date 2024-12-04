library(dplyr)
library(httr)
library(knitr)
library(readr)
library(stringr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(lubridate)





### TASK 1: Register for Alpha Vantage API Key ###





### TASK 2: Register for FRED API Key ###





### TASK 3: Data Acquisition ###
## LOAD REQUIRED LIBRARIES ##
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
# Define the Alpha Vantage API key and base URL
api_key <- "13OR5QPKL3DGT9U6"  # Alpha Vantage API key
symbol <- "EFA"  # MSCI Developed Markets ETF
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
      data_msci <- as.data.frame(do.call(rbind, lapply(time_series, unlist)))
      # Set column names for open, high, low, close, volume
      colnames(data_msci) <- c("open", "high", "low", "close", "volume")
      # Convert the row names to a date column
      data_msci$date <- rownames(data_msci)
      rownames(data_msci) <- NULL  # Remove row names
      # Ensure the date column is in Date format (using the first of each month)
      data_msci$date <- as.Date(paste0(data_msci$date, "-01"))
      # Print out the first few rows of the data frame
      print(head(data_msci))
    } else {
      print("Error: No time series data found.")
    }
  }
} else {
  # If the request was unsuccessful, print the error message
  print(paste("Request failed with status:", status_code(response)))
  print(content(response, "text"))
}



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
# Define the FRED API endpoint and the FRED API key
api_key <- "ffc31969aa3d7177ca28636e5b65c963"
base_url <- "https://api.stlouisfed.org/fred/"

# Define the FRED series ID for the data you want to download
series_id <- "QFRD304INFUSNO"

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
  data_shortdebts <- as.data.frame(observations)
  # Display the first few rows of the data
  head(data_shortdebts)
  # Save the data to a CSV file
  write.csv(data_shortdebts, "fred_data_shortdebts.csv", row.names = FALSE)
  # If the request succeeds, print a download message
  cat("Data has been successfully downloaded and saved as 'fred_data_shortdebts.csv'.\n")
} else {
  # If the request fails, print an error message
  cat("Failed to retrieve data. Status code:", status_code(response), "\n")
}





### TASK 4: Initial Analysis ###
# Make sure that the datatypes are in correct format
data_inflation$date <- as.Date(data_inflation$date)
data_inflation$value <- as.numeric(data_inflation$value)
data_wagegrowth$date <- as.Date(data_wagegrowth$date)
data_wagegrowth$value <- as.numeric(data_wagegrowth$value)

# Merge both datasets by the 'date' column, rescale the wage growth data, omit n/a values
data_inflationandwages <- merge(data_inflation, data_wagegrowth, by = "date", suffixes = c("_inflation", "_wage_growth"))
data_inflationandwages$scaled_wage_growth <- data_inflationandwages$value_wage_growth / 100000000
data_inflationandwages <- na.omit(data_inflationandwages)

# Plot the data using ggplot
ggplot(data_inflationandwages, aes(x = date)) +
  geom_line(aes(y = value_inflation, color = "Inflation"), size = 1) +
  geom_line(aes(y = scaled_wage_growth, color = "Wage Growth"), size = 1) +
  scale_y_continuous(
    name = "Inflation (percent)",
    sec.axis = sec_axis(~ ., name = "Wage Growth (hundreds of millions)")
  ) +
  labs(
    title = "Inflation vs Wage Growth",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Inflation" = "red", "Wage Growth" = "green")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )



# Convert SAP500 columns to numeric datatypes, create a year_quarter column, group "year_quarter" to calculate average open and close prices
data_sap500quarterlyavg <- data_sap500 |> 
  mutate(
    open = as.numeric(open),
    close = as.numeric(close),
    high = as.numeric(high),
    low = as.numeric(low),
    volume = as.numeric(volume),
    date = as.Date(date),
    year_quarter = paste(year(date), "-Q", quarter(date), sep = "")
  ) |> 
  group_by(year_quarter) |> 
  summarise(
    avg_open = mean(open, na.rm = TRUE),
    avg_close = mean(close, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  rename(
    `Year and Quarter` = year_quarter,
    `Average Open` = avg_open,
    `Average Close` = avg_close
  )

# Display the SAP500 results as a table
data_sap500quarterlyavg |> kable()

# Convert MSCI columns to numeric datatypes, create a year_quarter column, group "year_quarter" to calculate average open and close prices
data_msciquarterlyavg <- data_msci |> 
  mutate(
    open = as.numeric(open),
    close = as.numeric(close),
    high = as.numeric(high),
    low = as.numeric(low),
    volume = as.numeric(volume),
    date = as.Date(date),
    year_quarter = paste(year(date), "-Q", quarter(date), sep = "")
  ) |> 
  group_by(year_quarter) |> 
  summarise(
    avg_open = mean(open, na.rm = TRUE),
    avg_close = mean(close, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  rename(
    `Year and Quarter` = year_quarter,
    `Average Open` = avg_open,
    `Average Close` = avg_close
  )

# Display the MSCI results as a table
data_msciquarterlyavg |> kable()





### TASK 5: Historical Comparison ###
# Change the datatypes in the stock market data
data_sap500$open <- as.numeric(data_sap500$open)
data_sap500$close <- as.numeric(data_sap500$close)
data_msci$open <- as.numeric(data_msci$open)
data_msci$close <- as.numeric(data_msci$close)
data_shortdebts$date <- as.Date(data_shortdebts$date)
data_shortdebts$value <- as.numeric(data_shortdebts$value)



# TRS Retirement Calculation
calculate_trs_retirement_benefit <- function(salary_data, years_served, data_inflation, retirement_date) {
  # salary_data: A numeric vector of the employee's salary for the last 3 years
  # years_served: The number of years the employee has worked
  # data_inflation: A data frame containing 'date' and 'value' columns with CPI data
  # retirement_date: The employee's retirement date (as a Date object)
  
  # 1. Calculate Final Average Salary (FAS) based on the last 3 years' salary
  FAS <- mean(salary_data)
  
  # 2. Calculate the base retirement benefit
  if (years_served <= 20) {
    base_benefit <- 0.0167 * FAS * years_served
  } else if (years_served == 20) {
    base_benefit <- 0.0175 * FAS * years_served
  } else {
    base_benefit <- (0.35 + 0.02 * years_served) * FAS
  }
  
  # 3. Convert 'retirement_date' to Date class if not already
  retirement_date <- as.Date(retirement_date)
  
  # 4. Get CPI data for the period from January 2010 to the month of retirement
  data_inflation$date <- as.Date(data_inflation$date)  # Ensure 'date' is Date class
  
  # Filter CPI data from January 2010 to the month of retirement
  cpi_period <- data_inflation[data_inflation$date >= "2010-01-01" & data_inflation$date <= retirement_date, ]
  
  # 5. Initialize inflation adjustment
  inflation_adjustment <- 0  # Start with no inflation adjustment
  
  # Loop through each month in the CPI period and calculate inflation adjustment
  for (i in 1:nrow(cpi_period)) {
    avg_cpi <- cpi_period$value[i]  # CPI for the current month
    
    # Calculate the inflation adjustment for the current month
    monthly_inflation <- round(0.5 * avg_cpi, 1)  # 50% of the CPI, rounded to nearest 0.1%
    monthly_inflation <- pmin(pmax(monthly_inflation, 1), 3)  # cap between 1% and 3%
    
    # Apply the monthly inflation adjustment to the base benefit
    base_benefit <- base_benefit * (1 + monthly_inflation / 100)
  }
  
  # 6. Return the final adjusted retirement benefit
  return(base_benefit)
}



# ORP Retirement Calculation
calculate_orp <- function(age, salary, start_date, end_date, data_sap500, data_msci, data_shortdebts) {
  # Set asset allocation based on age range
  if (age >= 25 && age <= 49) {
    allocation <- c(US = 0.54, International = 0.36, Bonds = 0.10, ShortDebt = 0)
  } else if (age >= 50 && age <= 59) {
    allocation <- c(US = 0.47, International = 0.32, Bonds = 0.21, ShortDebt = 0)
  } else if (age >= 60 && age <= 74) {
    allocation <- c(US = 0.34, International = 0.23, Bonds = 0.43, ShortDebt = 0)
  } else if (age >= 75) {
    allocation <- c(US = 0.19, International = 0.13, Bonds = 0.62, ShortDebt = 0.06)
  }
  
  # Calculate market returns for each asset class
  us_returns <- (data_sap500$close - data_sap500$open) / data_sap500$open
  international_returns <- (data_msci$close - data_msci$open) / data_msci$open
  shortdebt_returns <- diff(log(data_shortdebts$value)) # assuming logarithmic return
  bond_returns <- rep(0, length(data_sap500$date)) # assuming bonds return 0 if not specified
  
  # Salary-based contribution percentages
  if (salary <= 45000) {
    emp_contrib_rate <- 0.03
  } else if (salary <= 55000) {
    emp_contrib_rate <- 0.035
  } else if (salary <= 75000) {
    emp_contrib_rate <- 0.045
  } else if (salary <= 100000) {
    emp_contrib_rate <- 0.0575
  } else {
    emp_contrib_rate <- 0.06
  }
  
  # Employer contribution rate (8% for first 7 years, 10% thereafter)
  employer_contrib_rate <- ifelse(age <= 31, 0.08, 0.10)  # assuming employment starts at age 25
  
  # Initial account balance
  account_balance <- 0  # Starting with no balance
  
  # Monthly contributions (employee + employer)
  emp_monthly_contrib <- (emp_contrib_rate * salary) / 12
  employer_monthly_contrib <- (employer_contrib_rate * salary) / 12
  
  # Withdrawals (4% annually, divided monthly)
  annual_withdrawal_rate <- 0.04
  monthly_withdrawal <- (annual_withdrawal_rate * salary) / 12
  
  # Date range for simulation
  date_range <- seq.Date(as.Date(start_date), as.Date(end_date), by = "month")
  
  # Loop over each month
  for (i in 1:length(date_range)) {
    # Monthly contributions
    account_balance <- account_balance + emp_monthly_contrib + employer_monthly_contrib
    
    # Apply asset allocation returns
    us_growth <- allocation["US"] * us_returns[i %% length(us_returns) + 1]  # Wrap around if less than the number of months
    international_growth <- allocation["International"] * international_returns[i %% length(international_returns) + 1]
    bond_growth <- allocation["Bonds"] * bond_returns[i %% length(bond_returns) + 1]
    shortdebt_growth <- allocation["ShortDebt"] * shortdebt_returns[i %% length(shortdebt_returns) + 1]
    
    # Update account balance with growth
    account_balance <- account_balance * (1 + us_growth + international_growth + bond_growth + shortdebt_growth)
    
    # Monthly withdrawal
    account_balance <- account_balance - monthly_withdrawal
  }
  
  return(account_balance)
}



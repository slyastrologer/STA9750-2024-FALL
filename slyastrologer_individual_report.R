# Load the necessary packages
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)





# Link the URL to the CSV file
url <- "https://data.ny.gov/api/views/yg77-3tkj/rows.csv?accessType=DOWNLOAD"

# Read the CSV file into R as a dataframe from the URL
mtafinancials2019to2024 <- read_csv(url)

# Filter the dataframe for transit data from the NYC Transit division and financial data that is confirmed
mtafinancials2019to2024 <- mtafinancials2019to2024 |>
  filter(Agency == "NYCT", Scenario == "Actual")

# Filter the dataframe for revenue data, then sum the revenue based on month
mtafarerevenue2019to2024 <- mtafinancials2019to2024 |>
  filter(Type == "Total Revenue", `General Ledger` == "Farebox Revenue") |>
  group_by(Month) |>
  summarise(Fare_Revenue = sum(Amount, na.rm = TRUE))

# Create a plot of the revenue
ggplot(mtafarerevenue2019to2024, aes(x = Month, y = Fare_Revenue)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 1) +
  labs(title = "MTA Monthly Fare Revenue",
       x = "Month",
       y = "Fare Revenue") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Link the URL to the CSV file for Uber Ridership Data
url <- "https://data.cityofnewyork.us/resource/2v9c-2k7f.csv?$query=SELECT%20base_license_number%2C%20base_name%2C%20dba%2C%20year%2C%20month%2C%20month_name%2C%20total_dispatched_trips%2C%20total_dispatched_shared_trips%2C%20unique_dispatched_vehicles%20SEARCH%20%22uber%22"

# Read the CSV file into R as a dataframe from the URL
uber_ride_stats <- read_csv(url)

# Link the URL to the CSV file for Lyft Ridership Data
url <- "https://data.cityofnewyork.us/resource/2v9c-2k7f.csv?$query=SELECT%20base_license_number%2C%20base_name%2C%20dba%2C%20year%2C%20month%2C%20month_name%2C%20total_dispatched_trips%2C%20total_dispatched_shared_trips%2C%20unique_dispatched_vehicles%20SEARCH%20%22lyft%22"

# Read the CSV file into R as a dataframe from the URL
lyft_ride_stats <- read_csv(url)

# Cleaning the Uber Data for Data from 2017 Onward
uber_ride_stats2017to2024 <- uber_ride_stats |>
  filter(year >= 2017) |>
  select(base_name, year, month, total_dispatched_trips) |>
  mutate(total_estimated_revenue = total_dispatched_trips * 14.48) |>
  arrange(year, month)

# Cleaning the Lyft Data for Data from 2017 Onward
lyft_ride_stats2017to2024 <- lyft_ride_stats |>
  filter(year >= 2017) |>
  select(base_name, year, month, total_dispatched_trips) |>
  mutate(total_estimated_revenue = total_dispatched_trips * 14.56) |>
  arrange(year, month)

# Combine the Uber and Lyft data
rideshare_stats2017to2024 <- bind_rows(uber_ride_stats2017to2024, lyft_ride_stats2017to2024)
# Creating a date column combining the year and month columns
rideshare_stats2017to2024$date <- make_date(rideshare_stats2017to2024$year, rideshare_stats2017to2024$month, 1)

# Create a plot of the total car trips for Uber and Lyft
suppressWarnings(
  ggplot(rideshare_stats2017to2024, aes(x = date, y = total_estimated_revenue, color = base_name, group = base_name)) +
    geom_line(size = 1) +
    geom_point(size = 1) +
    labs(title = "Uber and Lyft Monthly Estimated Revenue",
         x = "Month",
         y = "Estimated Revenue",
         color = "Company") +
    theme_minimal() +
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
    scale_color_manual(values = c("UBER" = "black", "LYFT" = "pink")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)





# Combine, clean, and plot the MTA, Uber, and Lyft Data
# Step 1: Filter the data for 2019 onward
mtafarerevenue2019to2024 <- mtafarerevenue2019to2024 %>%
  filter(Month >= "2019-01-01")
rideshare_stats2017to2024 <- rideshare_stats2017to2024 %>%
  filter(date >= "2019-01-01")

# Step 2: Separate the rideshare data into Uber and Lyft
rideshare_uber <- rideshare_stats2017to2024 %>%
  filter(base_name == "UBER")
rideshare_lyft <- rideshare_stats2017to2024 %>%
  filter(base_name == "LYFT")

# Step 3: Merge the datasets
# Ensure the "Month" and "date" columns are of Date class for proper merging
mtafarerevenue2019to2024$Month <- as.Date(mtafarerevenue2019to2024$Month)
rideshare_uber$date <- as.Date(rideshare_uber$date)
rideshare_lyft$date <- as.Date(rideshare_lyft$date)
# Merge the MTA dataset with the Uber and Lyft datasets by month
merged_data_uber <- left_join(mtafarerevenue2019to2024, rideshare_uber, by = c("Month" = "date"))
merged_data_lyft <- left_join(mtafarerevenue2019to2024, rideshare_lyft, by = c("Month" = "date"))

# Step 4: Combine the data for plotting
combined_data <- bind_rows(
  merged_data_uber %>% 
    select(Month, Fare_Revenue, total_estimated_revenue) %>%
    mutate(Rideshare = "UBER", Revenue = total_estimated_revenue),
  
  merged_data_lyft %>% 
    select(Month, Fare_Revenue, total_estimated_revenue) %>%
    mutate(Rideshare = "LYFT", Revenue = total_estimated_revenue)
) %>%
  mutate(Rideshare = factor(Rideshare, levels = c("UBER", "LYFT")))  # Ensure UBER and LYFT are factors for proper coloring

# Step 5: Plot the data
ggplot(combined_data, aes(x = Month)) +
  geom_line(aes(y = Fare_Revenue, color = "MTA Fare Revenue"), size = 1) +
  geom_line(aes(y = Revenue, color = Rideshare), size = 1) +
  labs(
    title = "Revenue Comparison: MTA vs Rideshare (UBER & LYFT)",
    x = "Month",
    y = "Revenue",
    color = "Legend"
  ) +
  scale_color_manual(values = c("MTA Fare Revenue" = "blue", "UBER" = "black", "LYFT" = "pink")) +
  theme_minimal() +
  theme(legend.position = "top")



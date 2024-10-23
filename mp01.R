if(!require("tidyverse")) install.packages("tidyverse")

# Let's start with Fare Revenue
library(tidyverse)
if(!file.exists("2022_fare_revenue.xlsx")){
  # This should work _in theory_ but in practice it's still a bit finicky
  # If it doesn't work for you, download this file 'by hand' in your
  # browser and save it as "2022_fare_revenue.xlsx" in your project
  # directory.
  download.file("http://www.transit.dot.gov/sites/fta.dot.gov/files/2024-04/2022%20Fare%20Revenue.xlsx", 
                destfile="2022_fare_revenue.xlsx", 
                quiet=FALSE, 
                method="wget")
}
FARES <- readxl::read_xlsx("2022_fare_revenue.xlsx") |>
  select(-`State/Parent NTD ID`, 
         -`Reporter Type`,
         -`Reporting Module`,
         -`TOS`,
         -`Passenger Paid Fares`,
         -`Organization Paid Fares`) |>
  filter(`Expense Type` == "Funds Earned During Period") |>
  select(-`Expense Type`) |>
  group_by(`NTD ID`,       # Sum over different `TOS` for the same `Mode`
           `Agency Name`,  # These are direct operated and sub-contracted 
           `Mode`) |>      # of the same transit modality
  # Not a big effect in most munis (significant DO
  # tends to get rid of sub-contractors), but we'll sum
  # to unify different passenger experiences
  summarize(`Total Fares` = sum(`Total Fares`)) |>
  ungroup()

# Next, expenses
if(!file.exists("2022_expenses.csv")){
  # This should work _in theory_ but in practice it's still a bit finicky
  # If it doesn't work for you, download this file 'by hand' in your
  # browser and save it as "2022_expenses.csv" in your project
  # directory.
  download.file("https://data.transportation.gov/api/views/dkxx-zjd6/rows.csv?date=20231102&accessType=DOWNLOAD&bom=true&format=true", 
                destfile="2022_expenses.csv", 
                quiet=FALSE, 
                method="wget")
}
EXPENSES <- readr::read_csv("2022_expenses.csv") |>
  select(`NTD ID`, 
         `Agency`,
         `Total`, 
         `Mode`) |>
  mutate(`NTD ID` = as.integer(`NTD ID`)) |>
  rename(Expenses = Total) |>
  group_by(`NTD ID`, `Mode`) |>
  summarize(Expenses = sum(Expenses)) |>
  ungroup()

FINANCIALS <- inner_join(FARES, EXPENSES, join_by(`NTD ID`, `Mode`))



# Monthly Transit Numbers
library(tidyverse)
if(!file.exists("ridership.xlsx")){
  # This should work _in theory_ but in practice it's still a bit finicky
  # If it doesn't work for you, download this file 'by hand' in your
  # browser and save it as "ridership.xlsx" in your project
  # directory.
  download.file("https://www.transit.dot.gov/sites/fta.dot.gov/files/2024-09/July%202024%20Complete%20Monthly%20Ridership%20%28with%20adjustments%20and%20estimates%29_240903.xlsx", 
                destfile="ridership.xlsx", 
                quiet=FALSE, 
                method="wget")
}
TRIPS <- readxl::read_xlsx("ridership.xlsx", sheet="UPT") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(-`Legacy NTD ID`, 
         -`Reporter Type`, 
         -`Mode/Type of Service Status`, 
         -`UACE CD`, 
         -`TOS`) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`), 
               names_to="month", 
               values_to="UPT") |>
  drop_na() |>
  mutate(month=my(month)) # Parse _m_onth _y_ear date specs
MILES <- readxl::read_xlsx("ridership.xlsx", sheet="VRM") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(-`Legacy NTD ID`, 
         -`Reporter Type`, 
         -`Mode/Type of Service Status`, 
         -`UACE CD`, 
         -`TOS`) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`), 
               names_to="month", 
               values_to="VRM") |>
  drop_na() |>
  group_by(`NTD ID`, `Agency`, `UZA Name`, 
           `Mode`, `3 Mode`, month) |>
  summarize(VRM = sum(VRM)) |>
  ungroup() |>
  mutate(month=my(month)) # Parse _m_onth _y_ear date specs

USAGE <- inner_join(TRIPS, MILES) |>
  mutate(`NTD ID` = as.integer(`NTD ID`))



if(!require("DT")) install.packages("DT")
library(DT)

sample_n(USAGE, 1000) |> 
  mutate(month=as.character(month)) |> 
  DT::datatable()



#Task 1 - Creating Syntatic Names
USAGE <- USAGE %>%
  rename(metro_area = "UZA Name")



#Task 2 - Recoding the Mode Column
USAGE <- USAGE |>
  mutate(Mode=case_when(
    Mode == "DR" ~ "Demand Response",   
    Mode == "FB" ~ "Ferryboat",   
    Mode == "MB" ~ "Bus",   
    Mode == "SR" ~ "Streetcar Rail",   
    Mode == "TB" ~ "Trolleybus",   
    Mode == "VP" ~ "Vanpool",   
    Mode == "CB" ~ "Commuter Bus",   
    Mode == "RB" ~ "Bus Rapid Transit",   
    Mode == "LR" ~ "Light Rail",   
    Mode == "YR" ~ "Hybrid Rail",   
    Mode == "MG" ~ "Monorail Automated Guideway",   
    Mode == "CR" ~ "Commuter Rail",   
    Mode == "AR" ~ "Alaska Railroad",   
    Mode == "TR" ~ "Aerial Tramway",   
    Mode == "HR" ~ "Heavy Rail",   
    Mode == "IP" ~ "Inclined Plane",   
    Mode == "PB" ~ "Publico",   
    Mode == "CC" ~ "Cable Car",   
    TRUE ~ "Unknown"))

if(!require("DT")) install.packages("DT")
library(DT)

sample_n(USAGE, 1000) |> 
  mutate(month=as.character(month)) |> 
  DT::datatable()



#Task 3 - Answering Specified Questions With DPLYR
#Remove the NTD ID and 3 Mode Columns
USAGE <- USAGE %>%
  select(-"NTD ID", -"3 Mode") 


#What transit agency had the most VRM?
USAGE %>% group_by(Agency) %>%
  summarize(max(VRM)) %>%
  arrange(desc(`max(VRM)`))
#After grouping the data by agency, calculating the max VRM values, and arranging
#the VRM values in descending order, the transit agency with the most VRM is MTA
#New York City Transit. 


#What transit mode had the most total VRM in our data set?
USAGE %>% group_by(Mode) %>%
  summarize(max(VRM)) %>%
  arrange(desc(`max(VRM)`))
#After grouping the data by the mode of transportation, calculating the max VRM values of each mode, 
#and arranging the VRM values in descending order, the transit mode with the most VRM in the dataset 
#is Heavy Rail; or more colloquially known as mass rapid transit. 


#How many trips were taken on the NYC Subway (Heavy Rail) in May 2024?
USAGE %>% filter(Agency == "MTA New York City Transit", month == "2024-05-01", Mode == "Heavy Rail")
#To determine how many Heavy Rail trips were taken on the NYC subway in May 2024, the filter function
#with three parameters is used: the first parameter ensures the data displayed is solely from the MTA NYC Transit,
#the second parameter ensures that of the MTA NYC Transit data, only datapoints from the month of May are displayed,
#the third parameter filters out only the transit trips from May that are completed via heavy rail.  


#What mode of transport had the longest average trip in May 2024?
#N/A


#How much did NYC subway ridership fall between April 2019 and April 2020?
Ridership_April19 <- USAGE %>%
  filter(month == "2019-04-01", Agency == "MTA New York City Transit", Mode == "Heavy Rail") %>%
  summarize(Ridership_April19 = sum(UPT, na.rm = TRUE))
Ridership_April20 <- USAGE %>%
  filter(month == "2020-04-01", Agency == "MTA New York City Transit", Mode == "Heavy Rail") %>%
  summarize(Ridership_May24 = sum(UPT, na.rm = TRUE))
Ridership_Change <- (Ridership_April19 - Ridership_April20)
Ridership_Change
#To determine how much subway ridership in NYC fell between April 2019 and April 2020, we first use the
#filter function to find the requested values and store them in two variables. We then subtract those two variables and
#store their result in a new variable, which we then run to receive a value of 211,969,660.



#Task 4 - Explore and Analyze
#
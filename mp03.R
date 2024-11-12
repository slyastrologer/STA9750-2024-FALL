library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(httr)





### TASK 1: Download Congressional Shapefiles 1976-2012 ###

# Base URL for downloading congressional shapefiles
base_url <- "https://github.com/JeffreyBLewis/congressional-district-boundaries"

# Define the range of congresses (from 1976 to 2012)
congresses <- 94:113

# Define the directory where the shapefiles will be saved
download_dir <- "congress_shapefiles"
if (!dir.exists(download_dir)) dir.create(download_dir)

# Function to download the shapefile if not already downloaded
download_shapefile <- function(congress, base_url, download_dir) {
  # Format the Congress number ("94th", "95th", etc.)
  congress_str <- paste0(congress, "th")
  # Construct the URL for the specific shapefile (adjust naming convention as needed)
  file_url <- paste0(base_url, congress_str, "/shapefile.zip")
  # Define the local filename for the zip file
  zip_file <- file.path(download_dir, paste0(congress_str, "_shapefile.zip"))
  # Check if the file already exists
  if (!file.exists(zip_file)) {
    message(paste("Downloading:", congress_str))
    GET(file_url, write_disk(zip_file, overwrite = TRUE))
  } else {
    message(paste("File for Congress", congress_str, "already exists. Skipping download."))
  }
}

# Loop through all Congresses from 94th to 113th
for (congress in congresses) {
  download_shapefile(congress, base_url, download_dir)
}

# Optional: After download, unzip all shapefiles if necessary
unzip_shapefiles <- function(download_dir) {
  zip_files <- list.files(download_dir, pattern = "\\.zip$", full.names = TRUE)
  for (zip_file in zip_files) {
    # Define the target folder for extraction
    unzip_dir <- gsub("\\.zip$", "", zip_file)
    if (!dir.exists(unzip_dir)) dir.create(unzip_dir)
    # Unzip the file
    unzip(zip_file, exdir = unzip_dir)
  }
}

# Optional: After downloading, uncomment the following line to unzip files
# unzip_shapefiles(download_dir)

message("Download process completed!")





### TASK 2: Download Congressional Shapefiles 2014-2022 ###

# Base URL for the U.S. Census Bureau Congressional Shapefiles
base_url <- "https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html"

# Define the range of congresses (113th to 117th Congresses correspond to years 2014-2022)
congresses <- 113:117  # From 113th Congress (2013-2015) to 117th Congress (2021-2023)

# Define the directory where the shapefiles will be saved
download_dir <- "congress_shapefiles"
if (!dir.exists(download_dir)) dir.create(download_dir)

# Function to download the shapefile if not already downloaded
download_shapefile <- function(congress, base_url, download_dir) {
  # Format the Congress number (e.g., "113th", "114th", etc.)
  congress_str <- paste0(congress, "th")
  # Construct the URL for the specific shapefile (adjust naming convention as needed)
  file_url <- paste0(base_url, "congressional", congress_str, "/shapefile.zip")
  # Define the local filename for the zip file
  zip_file <- file.path(download_dir, paste0("congress_", congress_str, "_shapefile.zip"))
  # Check if the file already exists
  if (!file.exists(zip_file)) {
    message(paste("Downloading:", congress_str))
    GET(file_url, write_disk(zip_file, overwrite = TRUE))
  } else {
    message(paste("File for Congress", congress_str, "already exists. Skipping download."))
  }
  return(zip_file)
}

# Function to unzip the downloaded shapefiles with error handling
unzip_shapefiles <- function(zip_file) {
  if (file.exists(zip_file)) {
    unzip_dir <- gsub("\\.zip$", "", zip_file)  # Remove .zip extension for folder name
    # Check if the directory already exists
    if (!dir.exists(unzip_dir)) {
      dir.create(unzip_dir)  # Create the directory for extraction
    }
    # Try unzipping the file, and catch any errors
    tryCatch({
      message(paste("Unzipping:", zip_file))
      unzip(zip_file, exdir = unzip_dir)
      message(paste("Unzipped successfully:", zip_file))
    }, error = function(e) {
      warning(paste("Failed to unzip:", zip_file, "\nError message:", e$message))
    })
  } else {
    warning(paste("The file does not exist:", zip_file))
  }
}

# Loop through all Congresses from 113th to 117th
for (congress in congresses) {
  zip_file <- download_shapefile(congress, base_url, download_dir)
  unzip_shapefiles(zip_file)
}

message("Download and unzip process completed!")





### TASK 3: Exploration of Vote Count Data ###

library(readr)
housevotes1976to2022 <- read_csv("dataverse_files/1976-2022-house.csv")
View(housevotes1976to2022)
library(readr)
presidentvotes1976to2020 <- read_csv("dataverse_files2/1976-2020-president.csv")
View(presidentvotes1976to2020)

### States that have gained and lost the most seats in the US House of Representatives between 1976 and 2022. ###

statedistricts_1976to2022 <- housevotes1976to2022 %>%
  group_by(year, state) %>%
  summarise(num_districts = n_distinct(district))
statedistricts_change <- statedistricts_1976to2022 %>%
  spread(key = year, value = num_districts) %>%
  rename(districts_1976 = `1976`, districts_2022 = `2022`) %>%
  mutate(districts_change = districts_2022 - districts_1976) %>%
  arrange(desc(districts_change))
view(statedistricts_change)

### Elections in our data where the election would have had a different outcome if the “fusion” system was not used and candidates only received the votes from their “major party line” (Democrat or Republican). ###

newyorkvotes_1976to2022 <- housevotes1976to2022 %>%
  group_by(year, state) %>%
  filter(state == "NEW YORK")

nycandidatevotes_total <- newyorkvotes_1976to2022 %>%
  group_by(candidate, year) %>%
  mutate(candidatetotal = sum(candidatevotes, na.rm = TRUE)) %>%
  ungroup()
nycandidatevotes_winner <- nycandidatevotes_total %>%
  group_by(year, district) %>%
  slice_max(candidatetotal, n = 1) %>%
  select(year, candidate, candidatetotal) %>%
  distinct(candidate, .keep_all = TRUE)
altnycandidatevotes_total <- newyorkvotes_1976to2022 %>%
  group_by(candidate, year) %>%
  mutate(candidatetotal = sum(candidatevotes, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(party == "DEMOCRAT" | party == "REPUBLICAN")
altnycandidatevotes_winner <- nycandidatevotes_total %>%
  group_by(year, district) %>%
  slice_max(candidatevotes, n = 1) %>%
  select(year, candidate, candidatevotes)

if (any(nycandidatevotes_winner$candidate != altnycandidatevotes_winner$candidate)) {
  print("There are elections that would have had a different outcome.")
} else {
  print("There are not elections that would have had a different outcome.")
}

### Do presidential candidates run ahead or run behind congressional candidates? ###

congressionalpartyvotes <- housevotes1976to2022 %>%
  filter(party == "DEMOCRAT" | party == "REPUBLICAN") %>%
  group_by(year, party) %>%
  summarize(total_votes_congressional = sum(candidatevotes), .groups = 'drop')
presidentialpartyvotes <- presidentvotes1976to2020 %>%
  rename("party" = "party_detailed") %>%
  filter(party == "DEMOCRAT" | party == "REPUBLICAN") %>%
  group_by(year, party) %>%
  summarize(total_votes_presidential = sum(candidatevotes), .groups = 'drop')
congressionalandpresidential <- merge(congressionalpartyvotes, presidentialpartyvotes, by = c("year", "party"), all.x = TRUE)
congressionalandpresidential$iscongresshigher <- congressionalandpresidential$total_votes_congressional > congressionalandpresidential$total_votes_presidential
view(congressionalandpresidential)





### TASK 4: Automate Zip File Extraction ###
library(ggplot2)
library(sf)

if(!file.exists("nyc_borough_boundaries.zip")){
  download.file("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=Shapefile", 
                destfile="nyc_borough_boundaries.zip")
}

##-
td <- tempdir(); 
zip_contents <- unzip("nyc_borough_boundaries.zip", 
                      exdir = td)

fname_shp <- zip_contents[grepl("shp$", zip_contents)]
nyc_sf <- read_sf(fname_shp)
nyc_sf

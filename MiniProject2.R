library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)

get_imdb_file <- function(fname){
  BASE_URL <- "https://datasets.imdbws.com/"
  fname_ext <- paste0(fname, ".tsv.gz")
  if(!file.exists(fname_ext)){
    FILE_URL <- paste0(BASE_URL, fname_ext)
    download.file(FILE_URL, 
                  destfile = fname_ext)
  }
  as.data.frame(readr::read_tsv(fname_ext, lazy=FALSE))
}

NAME_BASICS      <- get_imdb_file("name.basics")
TITLE_BASICS     <- get_imdb_file("title.basics")
TITLE_EPISODES   <- get_imdb_file("title.episode")
TITLE_RATINGS    <- get_imdb_file("title.ratings")
TITLE_CREW       <- get_imdb_file("title.crew")
TITLE_PRINCIPALS <- get_imdb_file("title.principals")



NAME_BASICS <- NAME_BASICS |> 
  filter(str_count(knownForTitles, ",") > 1)



TITLE_RATINGS |>
  ggplot(aes(x=numVotes)) + 
  geom_histogram(bins=30) +
  xlab("Number of IMDB Ratings") + 
  ylab("Number of Titles") + 
  ggtitle("Majority of IMDB Titles Have Less than 100 Ratings") + 
  theme_bw() + 
  scale_x_log10(label=scales::comma) + 
  scale_y_continuous(label=scales::comma)



TITLE_RATINGS |>
  pull(numVotes) |>
  quantile()



TITLE_RATINGS <- TITLE_RATINGS |>
  filter(numVotes >= 100)



TITLE_BASICS <- TITLE_BASICS |>
  semi_join(TITLE_RATINGS, 
            join_by(tconst == tconst))

TITLE_CREW <- TITLE_CREW |>
  semi_join(TITLE_RATINGS, 
            join_by(tconst == tconst))

TITLE_EPISODES_1 <- TITLE_EPISODES |>
  semi_join(TITLE_RATINGS, 
            join_by(tconst == tconst))
TITLE_EPISODES_2 <- TITLE_EPISODES |>
  semi_join(TITLE_RATINGS, 
            join_by(parentTconst == tconst))

TITLE_EPISODES <- bind_rows(TITLE_EPISODES_1,
                            TITLE_EPISODES_2) |>
  distinct()

TITLE_PRINCIPALS <- TITLE_PRINCIPALS |>
  semi_join(TITLE_RATINGS, join_by(tconst == tconst))


rm(TITLE_EPISODES_1)
rm(TITLE_EPISODES_2)



NAME_BASICS <- NAME_BASICS |>
  mutate(birthYear = as.numeric(birthYear),
         deathYear = as.numeric(deathYear))



#Task 1: Column Type Correction
TITLE_BASICS <- TITLE_BASICS |>
  mutate(startYear = as.numeric(startYear),
         endYear = as.numeric(endYear),
         runtimeMinutes = as.numeric(runtimeMinutes),
         isAdult = as.logical(isAdult))
TITLE_EPISODES <- TITLE_EPISODES |>
  mutate(seasonNumber = as.numeric(seasonNumber),
         episodeNumber = as.numeric(episodeNumber))
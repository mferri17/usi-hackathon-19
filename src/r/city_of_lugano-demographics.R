setwd('/Users/giovannikraushaar/Library/Mobile Documents/com~apple~CloudDocs/DataScience/Hackathon/usi_hackathon_data/data/city_of_lugano/demographics/')

library(fs)
library(jsonlite)
library(tibble)
library(dplyr)
library(stringr)

apply_quarter <- function(year, quarter, filename){
  d <- fromJSON(paste(year, quarter, filename, sep = '/'))
  d <- mutate(d, quarter = quarter)
  return(d)
}

apply_year <- function(year, filename){
  d <- lapply(1:4, function(x) apply_quarter(year = year, quarter = x, filename = filename))
  d <- bind_rows(d)
  d <- mutate(d, year = year)
  return(d)
}

apply_filename <- function(filename){
  d <- lapply(2008:2018, function(x) apply_year(year = x, filename = filename))
  d <- bind_rows(d)
  return(d)
}

filenames <- system('ls 2008/1/', intern = TRUE)

d <- lapply(filenames, apply_filename)
names(d) <- filenames

for (k in filenames) {
  write.csv(d[[k]], file = str_replace(k, '.json', '.csv'), col.names = FALSE)
}



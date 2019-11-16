# giovanni.kraushaar@usi.ch

homewd <- getwd()

library(readr)
library(tibble)
library(dplyr)
library(stringr)
library(lubridate)
library(geosphere)

d <- read_csv('datasets/publibike/rides_by_day.csv') %>%
  mutate(Ende = dmy_hm(Ende)) %>%
  mutate(
    date = date(Ende),
    time = hms::as_hms(Ende),
    hour = hour(Ende),
    minute = minute(Ende),
    day = wday(Ende, label = TRUE)
    ) %>%
  mutate(time = hms::round_hms(time, 600))  # round at 10 minutes

# ----- Merge Meteo

meteo <- read_csv('datasets/meteo_suisse/Dati-meteo_Lugano.csv') %>%
  mutate(
    Data = dmy(Data),
    Ora = hms::as_hms(Ora)
    ) %>%
  select(-matches('Data e ora'))

d <- left_join(d, meteo, by = c("date" = "Data", "time" = "Ora")) %>%
  rename(id = 'Fahrt: Name')

rm(meteo)

# ----- Merge Stations

loans <- read_csv('datasets/publibike/station_number_of_loans.csv') %>%
  rename(id = 'Fahrt: Name', from = "Station - VON") %>%
  distinct() %>% # remove duplicate rows
  mutate(from = str_replace(.$from, 'Piazza Mercato/Contrada di Verla', 'Contrada di Verla'))

returns <- read_csv('datasets/publibike/station_number_of_returns.csv') %>%
  rename(id = "Fahrt: Name", to = "Station - BIS") %>%
  distinct() %>%  # some ids were repeated
  mutate(to = str_replace(.$to, 'Piazza Mercato/Contrada di Verla', 'Contrada di Verla'))

trips <- inner_join(loans, returns, by = 'id') 
rm(loans, returns)

station <- read_csv('datasets/publibike/stations.csv') %>%
  select(-matches('id'), -matches('NetworkName')) %>%
  select(-matches('address'), -matches('zip'), -matches('city')) %>%
  .[,c(1,3,2)]  # first longitude then latitude

trips <- left_join(trips, station, by = c('from' = 'name')) %>%
  rename(from_long = 'longitude', from_lat = 'latitude') %>%
  left_join(station, by = c('to' = 'name')) %>%
  rename(to_long = 'longitude', to_lat = 'latitude') %>%
  add_column(distance = distGeo(  # distances are in meters!
    p1 = .[,c('from_long','from_lat')],
    p2 = .[,c('to_long','to_lat')]))

rm(station)

# NAs in
# "Stazione di riserva Lugano 2", "Stazione di riserva Lugano 1", 
# "Atelier Lugano"
# because they do not have a matching in stations

# ---- Merge everthing

d <- left_join(d, trips, by = 'id')

# ---- Save csv

write_csv(d, 'datasets/bikes.csv')


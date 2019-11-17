# giovanni.kraushaar@usi.ch

library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)

d <- read.csv('datasets/bikes.csv') %>%
  drop_na(to) %>%
  mutate(from = as.factor(to), to = as.factor(to), hour = as.factor(hour)) %>%
  mutate(date = ymd(date))

gather_bike <- function(d, start, end){
  d <- d %>%
    filter(date >= ymd(start), date <= ymd(end))
  
  stations <- read_csv('datasets/publibike/stations.csv') %>%
    select('name', 'longitude', 'latitude')
  
  # FROM
  t <- table(d$hour,d$from)
  hour <- attr(t,'dimnames')[[1]]
  from <- attr(t,'dimnames')[[2]]
  dfrom <- lapply(from, function(x) tibble(
    from = rep(x,length(hour)), 
    hour = hour)) %>% 
    bind_rows()
  
  t <- as.numeric(t) %>% tibble()
  colnames(t) <- 'from_count'
  dfrom <- bind_cols(dfrom, t)
  rm(hour,from,t)
  
  x <- dfrom %>% group_by(hour) %>% summarise(tot = sum(from_count))
  dfrom <- inner_join(dfrom, x, by = 'hour') %>%
    mutate(from_freq_rel = from_count / tot)
  rm(x)
  
  avg_dist <- tapply(d$distance, list(d$hour,d$from), mean) %>% 
    as.data.frame() %>%
    rownames_to_column('hour') %>%
    gather(-matches('hour'), key = 'from', value = 'from_avg_dist')
  
  dfrom <- inner_join(dfrom, avg_dist, by = c('hour','from')) %>%
    select(-'tot')
  
  # TO
  t <- table(d$hour,d$to)
  hour <- attr(t,'dimnames')[[1]]
  to <- attr(t,'dimnames')[[2]]
  dto <- lapply(to, function(x) tibble(
    to = rep(x,length(hour)), 
    hour = hour)) %>% 
    bind_rows()
  
  t <- as.numeric(t) %>% tibble()
  colnames(t) <- 'to_count'
  dto <- bind_cols(dto, t)
  rm(hour,to,t)
  
  x <- dto %>% group_by(hour) %>% summarise(tot = sum(to_count))
  dto <- inner_join(dto, x, by = 'hour') %>%
    mutate(to_freq_rel = to_count / tot)
  rm(x)
  
  avg_dist <- tapply(d$distance, list(d$hour,d$to), mean) %>% 
    as.data.frame() %>%
    rownames_to_column('hour') %>%
    gather(-matches('hour'), key = 'to', value = 'to_avg_dist')
  
  dto <- inner_join(dto, avg_dist, by = c('hour','to')) %>%
    select(-'tot')
  
  out <- inner_join(dfrom,dto, by = c('from' = 'to', 'hour')) %>%
    left_join(stations, by = c('from' = 'name')) %>%
    rename(station = from)
  
  return(out)
}

df <- filter(d, day == 'Sat' | day == 'Sun')

# DOW Day of the Week encoding
#  A = all
#  W = weekend
#  D = workday

# Period encoding
#  S = summer
#  F = fall

dsa <- gather_bike(d,'2019-07-20','2019-08-19') %>% 
  add_column( DOW = 'A', period = 'S')
dsw <- d %>%
  filter(day == 'Sat' | day == 'Sun') %>%
  gather_bike('2019-07-20','2019-08-19') %>% 
  add_column( DOW = 'W', period = 'S')
dsd <- d %>%
  filter(day != 'Sat' | day != 'Sun') %>%
  gather_bike('2019-07-20','2019-08-19') %>% 
  add_column( DOW = 'D', period = 'S')

dfa <- gather_bike(d,'2019-09-16','2019-10-15') %>% 
  add_column( DOW = 'A', period = 'F')
dfw <- d %>%
  filter(day == 'Sat' | day == 'Sun') %>%
  gather_bike('2019-09-16','2019-10-15') %>% 
  add_column( DOW = 'W', period = 'F')
dfd <- d %>%
  filter(day != 'Sat' | day != 'Sun') %>%
  gather_bike('2019-09-16','2019-10-15') %>% 
  add_column( DOW = 'D', period = 'F')

df <- bind_rows(dsa, dsw, dsd, dfa, dfw, dfd) %>%
  drop_na(from_avg_dist, to_avg_dist)

write_csv(
  df,
  path = 'datasets/gather_bike.csv'
)

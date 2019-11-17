# giovanni.kraushaar@usi.ch

library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)

d <- read.csv('datasets/bikes.csv') %>%
  drop_na(to) %>%
  mutate(from = as.factor(from), to = as.factor(to), 
         hour = as.factor(hour)) %>%
  mutate(date = ymd(date))

gather_bike <- function(d, start, end){
  d <- d %>%
    filter(date >= ymd(start), date <= ymd(end))
  
  counting <- function(hour, station, distance, prefix){
    # prefix either 'from' or 'to'
    
    
    
    t <- table(hour, station)
    h <- attr(t,'dimnames')[[1]]  # hour
    s <- attr(t,'dimnames')[[2]]  # station
    df <- lapply(s, function(x) tibble(
      s = rep(x,length(h)), 
      hour = h)) %>% 
      bind_rows()
    t <- as.numeric(t) %>% tibble()
    count_name <- paste(prefix, 'count', sep = '_')
    colnames(t) <- count_name
    df <- bind_cols(df, t) %>%
      rename(station = s)
    rm(h,s,t)
    
    hour_sum <- function(count, hour_vec, h){
      idx <- hour_vec == h
      s <- sum(count[idx])
      return(s)
    }
    x <- lapply(unique(df$hour), 
                function(k){
                  l <- list(hour = k, 
                            tot = hour_sum(df[[count_name]], df$hour, k))
                  return(l)
                }) %>%
      bind_rows()
    df <- left_join(df, x, by = 'hour')
    rm(x)
    # Relative frequency
    df[paste(prefix,'freq',sep = '_')] <- df[[count_name]] / df$tot
    
    avg_dist <- tapply(distance, list(hour,station), mean) %>% 
      as.data.frame() %>%
      rownames_to_column('hour') %>%
      gather(-matches('hour'), key = 'station', 
             value = value)
    avg_dist[paste(prefix, 'avg_dist', sep = '_')] <- avg_dist$value
    
    df <- inner_join(df, avg_dist, by = c('hour','station')) %>%
      select(-'tot', -'value')
    
    return(df)
  }
  
  dfrom <- counting(d$hour, d$from, d$distance, 'from')
  dto <- counting(d$hour, d$to, d$distance, 'to')
  
  stations <- read_csv('datasets/publibike/stations.csv') %>%
    select('name', 'longitude', 'latitude')
  
  df <- full_join(dfrom, dto, by = c('station', 'hour')) %>%
    left_join(stations, by = c('station' = 'name'))
  
  return(df)
}


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
  drop_na(from_avg_dist, to_avg_dist) %>%
  rename(
    from_avgDistance = from_avg_dist, 
    to_avgDistance = to_avg_dist,
    lon = longitude,
    lat = latitude)

write_csv(
  df,
  path = 'datasets/gather_bike.csv'
)

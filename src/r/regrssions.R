# giovanni.kraushaar@usi.ch

library(readr)
library(tibble)
library(dplyr)
library(lubridate)
library(stringr)
library(stargazer)

# Regression 1

d <- read_csv('datasets/bikes.csv') %>% 
  # select(matches('date'), matches('hour')) %>%
  mutate(time = paste(.$date, .$hour, sep = '_'))

s <- read_csv('datasets/swisscom_csv/tripcount_hourly_withinarea.csv') %>%
  mutate(date = ymd_hms(.$Date)) %>%
  transmute(time = paste(date(.$date), hour(.$date), sep = '_'), 
            commutes = `Reason: Commute`) %>%
  filter(commutes != '<20') %>%
  mutate(commutes = as.numeric(commutes))

df <- table(d$time) %>%
  tibble(time = names(.), rides = .)

df <- inner_join(df, s, by = 'time')

lm(rides ~ commutes, df) %>% summary()


# Regression 2 - Pick hours

dfp <- df %>% 
  filter(str_detect(.$time, '(?<=_)[78]|17|18|12|13$'))

lm(rides ~ commutes, dfp) %>% summary()

df %>%  ## non pick hours
  filter(!str_detect(.$time, '(?<=_)[78]|17|18|12|13$')) %>%
  lm(rides ~ commutes, .) %>%
  summary()
  


# Regression 3 - drunk hours (also more busses, substitution effect)
dfd <- df %>% 
  filter(str_detect(.$time, '(?<=_)(2[1-3])$|(?<=_)[0-2]$'))
lm(rides ~ commutes, dfd) %>% summary()

df %>%
  filter(str_detect(.$time, '(?<=_)([0-3]|23)$')) %>%
  lm(rides ~ commutes, .) %>%
  summary()


# Regression 4 - control dummies
dff <- df
df <- dff

split_day <- function(x){
  if (length(x) > 1) return(sapply(x, split_day))
  out <- if (x > 20 | x == 5 | x == 0) 'E' else if (x < 5 & x > 0) 'N' else 'D'
  return(out)
}

wday_dummy_gen <- function(x){
  if (length(x) > 1) return(sapply(x, wday_dummy_gen))
  out <- if (x %in% c('Sat','Sun')) TRUE else FALSE
  return(out)
}

df <- df %>% 
  add_column(hour = as.numeric(str_extract(.$time, '(?<=_)\\d+$'))) %>%
  add_column(day_time = split_day(.$hour)) %>%
  add_column(weekend = .$time %>%
               str_extract('^.+(?=_)') %>%
               ymd() %>%
               wday(label = TRUE) %>% 
               wday_dummy_gen())

## Factor Encoding 
#  D - day (6-20)
#  E - evening and early morning (21-24 + 5) -> low density
#  N - night (1-4) 


dummyfy <- function(x){
  x <- as.factor(x)
  d <- lapply(levels(x), function(y) if_else(x == y, TRUE, FALSE)) %>%
    bind_cols()
  colnames(d) <- levels(x)
  return(d)
}

df <- df %>%
  bind_cols(dummyfy(df$day_time))

lm(rides ~ commutes + D + E + weekend, df) %>%
  summary()

lm(rides ~ commutes + low_transport + weekend, 
   df %>% mutate(low_transport = D+E)) %>%
  summary()


# Regrssions on paper

# (1) 
lm(rides ~ commutes, filter(df, !weekend)) %>% 
  stargazer(out = 'reg1.tex')

reg2 <- lm(rides ~ commutes, filter(df, !weekend, D))
reg3 <- lm(rides ~ commutes, filter(df, !weekend, E))
reg4 <- lm(rides ~ commutes, filter(df, !weekend, N))
stargazer(reg2,reg3,reg4, out = 'multireg.tex')

rm(d,dfd,dff,dfp,reg2,reg3,reg4,s)


# Regressions with tpl
tpl <- read_csv('datasets/tpl_density.csv')

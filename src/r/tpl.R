# giovanni.kraushaar@usi.ch

library(dplyr)
library(readr)
library(lubridate)
library(fs)
library(ggplot2)

files <- fs::dir_ls('datasets/tpl/lines_events/')

d <- lapply(files, function(x) read_csv(x)[,c(1,7,9)]) %>%
  bind_rows()
d[!is.na(d$X1),1] <- d[!is.na(d$X1),4] 

d <- d %>% 
  mutate(date = dmy(DATA)) %>%
  mutate(hour = hour(TRANS.PROGRAMMATO)) %>%
  select(-X1, -DATA, -TRANS.PROGRAMMATO) %>%
  mutate(dh = paste(date, hour, sep = '_'))

d <- tapply(d$VEICOLO, as.factor(d$dh), function(x) length(unique(x))) %>%
  tibble(
    dh = names(.),
    count = .) %>%
  mutate(dh = ymd_h(dh))

write_csv(d, 'datasets/tpl_density.csv')

g <- d %>% 
  transmute(hour = hour(dh), count = count) %>%
  group_by(hour) %>%
  summarise(avg = mean(count))

ggplot(g, aes(hour,avg)) + geom_col()

ggsave('tpl_density.pdf', device = 'pdf')

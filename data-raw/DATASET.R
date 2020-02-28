## code to prepare `DATASET` dataset goes here

usethis::use_data("DATASET")

library(expsmooth)
library(tidyverse)
library(tsibble)

data("carparts")

# 1. remove series with NAs
missing <- as_tsibble(carparts) %>%
  mutate(a = ifelse(is.na(value), 1, 0)) %>%
  filter(a != 0) %>%
  distinct(key) %>%
  pull()

# 2. ten or more months with positive demands (remove if less than 10 positives)
less_than_10 <- as_tsibble(carparts) %>%
  as_tibble() %>%
  mutate(positive = ifelse(value > 0 , 1, 0)) %>%
  group_by(key) %>%
  summarise(tot_positive = sum(positive)) %>%
  filter(tot_positive < 10) %>%
  distinct(key) %>%
  pull()

# at least some positive demands in the first 15 months
zero_front <- as_tsibble(carparts) %>%
  as_tibble() %>%
  mutate(positive = ifelse(value > 0 , 1, 0)) %>%
  group_by(key) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 15) %>%
  summarise(tot_positive = sum(positive)) %>%
  filter(tot_positive == 0) %>%
  distinct(key) %>%
  pull()

# at least some positive demands in the last 15 months
zero_back <- as_tsibble(carparts) %>%
  as_tibble() %>%
  mutate(positive = ifelse(value > 0 , 1, 0)) %>%
  group_by(key) %>%
  mutate(rank = row_number()) %>%
  filter(rank >= 37) %>%
  summarise(tot_positive = sum(positive)) %>%
  filter(tot_positive == 0) %>%
  distinct(key) %>%
  pull()

`%ni%` <- Negate(`%in%`)

df <- as_tsibble(carparts) %>%
  filter(key %ni% missing) %>%
  filter(key %ni% zero_front) %>%
  filter(key %ni% zero_back) %>%
  filter(key %ni% less_than_10) %>%
  mutate(date = as.Date(index)) %>%
  as_tibble() %>%
  select(date, key, value)

train <- df %>%
  filter(date < "2001-10-01") %>%
  as_tibble()

test <- df %>%
  filter(date >= "2001-10-01")%>%
  as_tibble()

write_csv(df, "data-raw/carparts.csv")
write_csv(train, "data/train_carparts.csv")
write_csv(test, "data/test_carparts.csv")

usethis::use_data(df)




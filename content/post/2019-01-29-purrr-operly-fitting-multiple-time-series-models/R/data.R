# prepare environment ----------------------------------------------------------

# clear environment
rm(list = ls())

# import libs
library(lubridate)
library(magrittr)
library(padr)
library(tidyverse)

# prepare raw data -------------------------------------------------------------

# get all file names
filenames <- list.files("data-raw")

# create a container tibble
pjm <- read_csv(paste0("data-raw/", filenames[1])) %>%
  rename(datetime = 1, cons = 2) %>%
  mutate(provider = str_replace_all(filenames[1], "_hourly.csv", "")) %>%
  select(provider, everything())

# import all data
for (provider in filenames[-1]) {
  
  pjm %<>% bind_rows(
    read_csv(paste0("data-raw/", provider)) %>%
    rename(datetime = 1, cons = 2) %>%
    mutate(provider = str_replace_all(provider, "_hourly.csv", ""))
  )
  
}

# data wrangling ---------------------------------------------------------------

# take the mean from duplicates
pjm %<>%
  group_by(provider, datetime) %>%
  summarise(cons = mean(cons)) %>%
  ungroup()

# # checking time span across providers
# pjm %>%
#   spread(provider, cons) %>%
#   drop_na() %>%
#   glimpse()

# balancing the date
pjm %<>%
  filter(provider != "NI") %>% # remove provider with shorter time span
  spread(key = provider, value = cons) %>%
  drop_na() %>%
  gather(key = provider, value = cons, -datetime)

# interpolate some missing values
pjm %<>%
  group_by(provider) %>%
  pad() %>%
  mutate(cons = case_when(
    is.na(cons) ~ lag(cons) + lead(cons) / 2,
    TRUE ~ cons
  )) %>%
  ungroup()
  
# readjust datetime since it is ceil-ed in the original data
pjm %<>%
  mutate(datetime = datetime - hours(1))

# save the dataset -------------------------------------------------------------

# write
write_csv(pjm, "data/pjm.csv")

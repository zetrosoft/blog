# prepare environment ----------------------------------------------------------

# clear environment
rm(list = ls())

# import libs
library(forecast)
library(lubridate)
library(magrittr)
library(tidymodels)
library(tidyverse)

# prepare dataset ------------------------------------------------------------

# import dataset
pjm <- read_csv("data/pjm.csv")

# quick check
glimpse(pjm)

# split time index -------------------------------------------------------------

# train-val-test size
test_size <- 24 * 7 * 4
train_size <- 24 * 7 * 4 * 3

# get the min-max of the time index for each sample
test_end <- max(pjm$datetime)
test_start <- test_end - hours(test_size) + hours(1)

train_end <- test_start - hours(1)
train_start <- train_end - hours(train_size) + hours(1)

# prepare recipes --------------------------------------------------------------

# convert to wide format
pjm %<>%
  spread(provider, cons)

# recipes: square root, center, scale
rec <- recipe(~ . , filter(pjm, datetime %within% intrain)) %>%
  step_sqrt(all_numeric()) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  prep()

# preview the bake results
pjm <- bake(rec, pjm)

# quick check
glimpse(pjm)

# revert back function
rec_revert <- function(vector, rec, varname) {

  # store recipe values
  rec_center <- rec$steps[[2]]$means[varname]
  rec_scale <- rec$steps[[3]]$sds[varname]

  # convert back based on the recipe
  results <- (vector * rec_scale + rec_center) ^ 2

  # add additional adjustment if necessary
  results <- round(results)

  # return the results
  results

}

# convert back to long format
pjm %<>%
  gather(provider, cons, -datetime)

# nesting ----------------------------------------------------------------------

# adjust by sample
pjm %<>%
  mutate(sample = case_when(
    datetime %within% intrain ~ "train",
    datetime %within% intest ~ "test"
  )) %>%
  drop_na()

# nest the train data
pjm %<>%
  group_by(provider, sample) %>%
  nest(.key = "data") %>%
  spread(sample, data)

# prepare data functions -------------------------------------------------------

# data funs list
data_funs <- list(
  ts = function(x) ts(pull(x, cons), frequency = 24),
  msts = function(x) msts(pull(x, cons), seasonal.periods = c(24, 24 * 7))
)

# convert to nested
data_funs %<>%
  rep(length(unique(pjm$provider))) %>%
  enframe(name = "data_fun_name", "data_fun") %>%
  mutate(provider =
    sort(rep(unique(pjm$provider), length(unique(.$data_fun_name))))
  )

# combine with models
pjm %<>%
  left_join(data_funs)

# prepare fitting functions ----------------------------------------------------

# models list
models <- list(
  auto.arima = function(x) auto.arima(x),
  ets = function(x) ets(x),
  stlm = function(x) stlm(x),
  tbats = function(x) tbats(x, use.box.cox = FALSE)
)

# convert to nested
models %<>%
  rep(length(unique(pjm$provider))) %>%
  enframe(name = "model_name", "model") %>%
  mutate(provider =
    sort(rep(unique(pjm$provider), length(unique(.$model_name))))
  )

# combine with models
pjm %<>%
  left_join(models) %>%
  filter(
    !(model_name == "ets" & data_fun_name == "msts"),
    !(model_name == "auto.arima" & data_fun_name == "msts"),
  )

# nested fitting ---------------------------------------------------------------

# invoke nested fitting
pjm %<>%
  mutate(
    params = map(train, ~ list(x = .x)),
    data = invoke_map(data_fun, params),
    params = map(data, ~ list(x = .x)),
    fitted = invoke_map(model, params)
  ) %>%
  select(-data, -params)

# calculate test errors
pjm %<>%
  mutate(error =
    map(fitted, ~ forecast(.x, h = 24 * 7 * 4)) %>%
    map2_dbl(test, ~ rmse_vec(truth = .y$cons, estimate = .x$mean))
  ) %>%
  arrange(provider, error)

# filter by lowest test error
pjm %<>%
  select(-fitted) %>%
  group_by(provider) %>%
  filter(error == first(error)) %>%
  ungroup()

# nested forecast --------------------------------------------------------------

# recombine samples
pjm %<>%
  mutate(fulldata = map2(train, test, ~ bind_rows(.x, .y))) %>%
  select(provider, fulldata, everything(), -train, -test)

# invoke nested fitting for full data
pjm %<>%
  mutate(
    params = map(fulldata, ~ list(x = .x)),
    data = invoke_map(data_fun, params),
    params = map(data, ~ list(x = .x)),
    fitted = invoke_map(model, params)
  ) %>%
  select(-data, -params)

# get forecast
pjm %<>%
  mutate(forecast =
    map(fitted, ~ forecast(.x, h = 24 * 7 * 4)) %>%
    map2(fulldata, ~ tibble(
      datetime = tk_make_future_timeseries(.y$datetime, 24 * 7 * 4),
      cons = as.vector(.x$mean)
    ))
  )

# unnest actual and forecast
pjm %<>%
  select(provider, actual = fulldata, forecast) %>%
  gather(key, value, -provider) %>%
  unnest(value) %>%
  mutate(cons = rec_revert(cons, rec, provider))
  
# plot the forecast
pjm %>%
  ggplot(aes(x = datetime, y = cons, colour = key)) +
    geom_line() +
    labs(x = NULL, y = NULL, colour = NULL) +
    facet_wrap(~ provider, scale = "free", ncol = 1) +
    tidyquant::theme_tq() +
    tidyquant::scale_colour_tq()

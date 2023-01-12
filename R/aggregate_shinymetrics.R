library(tidyverse)
library(lubridate)
library(shinyfind)
library(htmltools)
library(shinymetrics)
library(purrr)
library(glue)


metrics_df_weekly <-
  list.files(path = "data", pattern = "daily_metrics_*", full.names = TRUE) |>
  map_df(~read_csv(.)) |>
  unique() |>
  dplyr::group_by(date = cut(date, "week"), key) %>% 
  dplyr::summarise(value = sum(value)) |>
  mutate(date = lubridate::as_date(date)) |>
  rename(application = key,
         n_connections = value)



# Write latest file with weekly metrics  ------------------------------------------
write_csv(x = metrics_df_weekly, file = 'data/metrics_weekly_latest.csv')

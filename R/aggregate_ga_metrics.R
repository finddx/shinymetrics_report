library(tidyverse)
library(lubridate)
library(shinyfind)
library(purrr)
library(glue)


files <- fs::dir_ls("data/google_analytics/", glob="*.csv")
ga_metrics_df_weekly <- read_csv(files, id="path", skip = 10)


ga_metrics_df_weekly <-
  list.files(path = "data/google_analytics", pattern = "*.csv", full.names = TRUE) |>
  read_csv(id="application", skip = 10) |>
  mutate(application = stringr::str_replace(application, "data/google_analytics/", ""))|>
  mutate(application = stringr::str_replace(application, ".csv", "")) |>
  mutate(application = case_when(application == "2020-2022_covid-19_EQA" ~ "FIND EQA directory (COVID-19)",
                                 application == "2020-2022_covid-19_ngs_sequencing_mapping" ~ "FIND NGS Capacity (COVID-19)",
                                 application == "2020-2022_covid-19_policy_mapping" ~ "FIND Sars-CoV-2 Policy dashboard",
                                 application == "2020-2022_covid-19_test_dir_gardeners" ~ "FIND Test directory (COVID-19)",
                                 application == "2020-2022_covid-19_test_dir_shinny" ~ "FIND Test directory (COVID-19)",
                                 application == "2020-2022_covid-19_test_tracker" ~ "FIND Sars-CoV-2 Test tracker",
                                 application == "2020-2022_ebola_test_dir" ~ "FIND Test directory (Ebola)",
                                 application == "2020-2022_monkeypox_test_dir" ~ "FIND Test directory (Monkeypox)",
                                 application == "2020-2022_ntds_test_dir" ~ "FIND Test directory (NTDs)"
                                 
  )
  ) |>
  rename(date = `Day Index`,
         page_views = `Page Views`) |>
  filter(!is.na(date)) |>
  mutate(date = lubridate::as_date(date, format = "%d/%m/%Y")) |>
  group_by(application, date) |>
  summarise(page_views = sum(page_views)) |>
  group_by(date = cut(date, "week"), application) %>% 
  summarise(page_views = sum(page_views)) |>
  mutate(date = lubridate::as_date(date)) 




ga_metrics_df_weekly_wide <- ga_metrics_df_weekly |>
  pivot_wider(
    names_from =  application,
    values_from = page_views
  )


# Write latest file with weekly metrics  ------------------------------------------
write_csv(x = ga_metrics_df_weekly, file = 'data/ga_metrics_df_weekly_latest.csv')
write_csv(x = ga_metrics_df_weekly_wide, file = 'data/ga_metrics_df_weekly_wide_latest.csv')

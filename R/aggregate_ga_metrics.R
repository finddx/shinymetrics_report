library(tidyverse)
library(lubridate)
library(shinyfind)
library(purrr)
library(glue)


files_2020_2022 <- fs::dir_ls("data/google_analytics/", glob="*2020-2022*.csv")
ga_metrics_df_weekly_2020_2022 <- read_csv(files_2020_2022, id="path", skip = 10)|>
  rename(date = `Day Index`,
         page_views = `Page Views`) |>
  mutate(date = lubridate::as_date(date, format = "%d/%m/%Y")) |>
  mutate(application = stringr::str_replace(path, "data/google_analytics/", ""))
  
  


files_2023 <- fs::dir_ls("data/google_analytics/", glob="* - 2023*.csv")
ga_metrics_df_weekly_2023 <- read_csv(files_2023, id="path") |>
  select(path, date = Date, page_views = Views) |>
  mutate(date = lubridate::as_date(date, format = "%d %m %Y")) |>
  mutate(application = stringr::str_replace(path, "data/google_analytics/20231107 - 2023 Pages views for test directories_", ""))
  
  


ga_metrics_df_weekly <- bind_rows(ga_metrics_df_weekly_2020_2022, ga_metrics_df_weekly_2023) |>
  mutate(application = stringr::str_replace(application, ".csv", "")) |>
  mutate(application = case_when(application == "2020-2022_covid-19_EQA" ~ "FIND EQA directory (COVID-19)",
                                 application == "2020-2022_covid-19_ngs_sequencing_mapping" ~ "FIND NGS Capacity (COVID-19)",
                                 application == "2020-2022_covid-19_policy_mapping" ~ "FIND Sars-CoV-2 Policy dashboard",
                                 application == "2020-2022_covid-19_test_dir_gardeners" ~ "FIND Test directory (COVID-19)",
                                 application == "2020-2022_covid-19_test_dir_shinny" ~ "FIND Test directory (COVID-19)",
                                 application == "2020-2022_covid-19_test_tracker" ~ "FIND Sars-CoV-2 Test tracker",
                                 application == "2020-2022_ebola_test_dir" ~ "FIND Test directory (Ebola)",
                                 application == "2020-2022_monkeypox_test_dir" ~ "FIND Test directory (Monkeypox)",
                                 application == "2020-2022_ntds_test_dir" ~ "FIND Test directory (NTDs)",
                                 application == "AMR test directory_Table" ~ "FIND Test directory (AMR)",
                                 application == "COVID-19 test directory_Table" ~ "FIND Test directory (COVID-19)",
                                 application == "Outbreaks test directory_Table" ~ "FIND Test directory (Outbreaks)",
                                 application == "TB test directory_Table" ~ "FIND Test directory (TB)"
  )
  ) |>
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


ga_metrics_df_weekly_testdirs <- ga_metrics_df_weekly |>
  filter(application %in% c("FIND EQA directory (COVID-19)",
                            "FIND Test directory (AMR)",
                            "FIND Test directory (COVID-19)",
                            "FIND Test directory (Ebola)",
                            "FIND Test directory (Monkeypox)",
                            "FIND Test directory (NTDs)",
                            "FIND Test directory (Outbreaks)",
                            "FIND Test directory (TB)"))



ga_metrics_df_weekly_testdirs_wide <- ga_metrics_df_weekly_testdirs |>
  pivot_wider(
    names_from =  application,
    values_from = page_views
  )


# Write latest file with weekly metrics  ------------------------------------------
write_csv(x = ga_metrics_df_weekly, file = 'data/ga_metrics_df_weekly_latest.csv')
write_csv(x = ga_metrics_df_weekly_wide, file = 'data/ga_metrics_df_weekly_wide_latest.csv')

write_csv(x = ga_metrics_df_weekly_testdirs, file = 'data/ga_metrics_df_weekly_testdirs.csv')
write_csv(x = ga_metrics_df_weekly_testdirs_wide, file = 'data/ga_metrics_df_weekly_testdirs_wide.csv')


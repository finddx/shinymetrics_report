library(tidyverse)
library(lubridate)
library(shinyfind)
library(htmltools)
library(shinymetrics)
library(purrr)
library(glue)


# Connect to account on shinyapps.io --------------------------------------
rsconnect::setAccountInfo(name='finddx',
                          token=Sys.getenv("SHINYAPPS_TOKEN"),
                          secret=Sys.getenv("SHINYAPPS_SECRET"))


apps <- rsconnect::applications(account = "finddx")



# Specify dates for metrics function --------------------------------------
# today <- "2022-12-31 0:00:00"
# last_date <- "2022-09-01 0:00:00"

today <- format(Sys.time(), "%Y-%m-%d %X")
last_date <-  as.Date(today) - 7




# Produce metrics per day for all public shiny apps -----------------------
apps <- c("FINDCov19Tracker", "FINDCov19Policy", "testdir_tabs","ntds", 
          "ebola", "monkeypox", "covid_eqa", "ngscapacity")
app_names <- c("FIND Sars-CoV-2 Test tracker", "FIND Sars-CoV-2 Policy dashboard", "FIND Test directory (COVID-19)",
               "FIND Test directory (NTDs)", "FIND Test directory (Ebola)","FIND Test directory (Monkeypox)", 
               "FIND EQA directory (COVID-19)", "FIND NGS Capacity (COVID-19)")

server <- rep("shinyapps.io", length(app))
from <- rep(last_date, length(apps))
until <- rep(today, length(app_names))

args <- list(apps, app_names, server,from, until)


metrics_df_list <- pmap(args, metrics)
metrics_df <- bind_rows(metrics_df_list)


# Write file with daily metrics  ------------------------------------------
write_csv(x = metrics_df, file = paste0('data/daily_metrics_',as.Date(last_date),"_", as.Date(today), ".csv"))


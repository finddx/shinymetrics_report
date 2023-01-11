# download metrics from shinyapps.io

library(tidyverse)
library(lubridate)
library(shinyfind)
library(purrr)


apps <- rsconnect::applications("finddx") 

apps <- c("FINDCov19Tracker", "FINDCov19Policy", "testdir_tabs","ntds", 
              "ebola", "monkeypox", "covid_eqa")


app_names <- c("FIND Sars-CoV-2 Test tracker", "FIND Sars-CoV-2 Policy dashboard", "FIND Test directory (COVID-19)",
          "FIND Test directory (NTDs)", "FIND Test directory (Ebola)","FIND Test directory (Monkeypox)", 
          "FIND EQA directory (COVID-19)")

# Create function for getting metrics -------------------------------------

metrics <- function(app = "FINDCov19Policy", app_name = "FIND Sars-CoV-2 Policy dashboard", server="shinyapps.io", from = "2022-09-01 0:00:00", until = "2022-12-12 0:00:00"){
  df <- rsconnect::showMetrics("container_status",
                               c("connect_count", 
                                 "connect_procs"),
                               appName = app,
                               server = server,
                               from = as.numeric(as.POSIXct(from)),
                               until = as.numeric(as.POSIXct(until)),
                               interval="1m"
  ) %>%
    magrittr::set_colnames(c("dummy", "timestamp", "connect_count", "connect_procs")) %>%
    mutate(across(c("timestamp", "connect_count", "connect_procs"), as.numeric)) %>%
    mutate(date=as_datetime(timestamp)) %>% 
    select(-timestamp) %>% 
    arrange(date) %>% 
    mutate(date = date(date),
           new_connect=case_when(
             connect_count>lag(connect_count,1) ~ connect_count-lag(connect_count,1),
             TRUE ~ 0)) %>%
    group_by(date) %>%
    summarise(
      n_count=sum(connect_count),
      n_procs=sum(connect_procs),
      n_connect=sum(new_connect) # approximate) %>%
    ) %>%  
    select(n_connect, date) %>% 
    gather(key="key", value="value", -date) %>%
    mutate(key = app_name)
  
  return(df)
}


app <- c("FINDCov19Tracker", "FINDCov19Policy", "testdir_tabs","ntds", 
          "ebola", "monkeypox", "covid_eqa")


app_name <- c("FIND Sars-CoV-2 Test tracker", "FIND Sars-CoV-2 Policy dashboard", "FIND Test directory (COVID-19)",
               "FIND Test directory (NTDs)", "FIND Test directory (Ebola)","FIND Test directory (Monkeypox)", 
               "FIND EQA directory (COVID-19)")

server <- rep("shinyapps.io", length(app))

from <- rep("2022-09-01 0:00:00", length(app))

until <- rep("2022-12-12 0:00:00", length(app))

argList <- list(app, app_name, server, from, until)

metrics_all <- pmap_dfr(argList,  metrics)



# Create function for plotting --------------------------------------------

plot_metrics <- function(df, weekly = TRUE, app_name){
  df <- subset(df, key == app_name)
  
  if(isTRUE(weekly)){
    df <- df %>%
      group_by(date = cut(date, "week"), key) %>% 
      summarise(value = sum(value))
    df$date <- as_date(df$date)
    
    p <- ggplot(df) +
      labs(title="Number of connections for FIND shiny applications (weekly)", x="", y="") +
      geom_line(aes(x=date, y=value, colour=key)) +
      facet_wrap(~key) + 
      scale_color_manual(values = pal_find()) +
      theme_light()
    
  } else {
    df <- df
    p <- ggplot(df) +
      labs(title="Number of connections for FIND shiny applications (daily)", x="", y="") +
      geom_line(aes(x=date, y=value, colour=key)) +
      facet_wrap(~key) + 
      scale_color_manual(values = pal_find()) +
      theme_light()
  }
  
 return(p)
}


plot_metrics(df = metrics_all, weekly = TRUE, app_name[1])
plot_metrics(df = metrics_all, weekly = FALSE, app_name[1])

plot_metrics(df = metrics_all, weekly = TRUE, app_name[2])
plot_metrics(df = metrics_all, weekly = FALSE, app_name[2])

plot_metrics(df = metrics_all, weekly = TRUE, app_name[3])
plot_metrics(df = metrics_all, weekly = FALSE, app_name[3])

plot_metrics(df = metrics_all, weekly = TRUE, app_name[4])
plot_metrics(df = metrics_all, weekly = FALSE, app_name[4])

plot_metrics(df = metrics_all, weekly = TRUE, app_name[5])
plot_metrics(df = metrics_all, weekly = FALSE, app_name[5])

plot_metrics(df = metrics_all, weekly = TRUE, app_name[6])
plot_metrics(df = metrics_all, weekly = FALSE, app_name[6])

plot_metrics(df = metrics_all, weekly = TRUE, app_name[7])
plot_metrics(df = metrics_all, weekly = FALSE, app_name[7])






# 
# # Metrics for Test Directory ----------------------------------------------
# 
# # http://docs.rstudio.com/shinyapps.io/metrics.html#ApplicationMetrics
# testdir <- rsconnect::showMetrics("container_status",
#                                   c("connect_count", 
#                                     "connect_procs"),
#                                   appName="testdir_tabs",
#                                   server="shinyapps.io",
#                                   from = as.numeric(as.POSIXct("2022-09-01 0:00:00")),
#                                   until = as.numeric(as.POSIXct("2022-12-05 0:00:00")),
#                                   interval="1m"
# ) %>%
#   magrittr::set_colnames(c("dummy", "timestamp", "connect_count", "connect_procs")) %>%
#   mutate(across(c("timestamp", "connect_count", "connect_procs"), as.numeric)) %>%
#   mutate(date=as_datetime(timestamp)) %>% 
#   select(-timestamp) %>% 
#   arrange(date) %>% 
#   mutate(date = date(date),
#          new_connect=case_when(
#            connect_count>lag(connect_count,1) ~ connect_count-lag(connect_count,1),
#            TRUE ~ 0)) %>%
#   group_by(date) %>%
#   summarise(
#     n_count=sum(connect_count),
#     n_procs=sum(connect_procs),
#     n_connect=sum(new_connect) # approximate) %>%
#   ) %>%  
#   select(n_connect, date) %>% 
#   gather(key="key", value="value", -date) %>%
#   mutate(key = "Test directory")
# 
# p_testdir <- ggplot(testdir) +
#   labs(title="Daily number of connections for FIND Test directory (COVID-19)", x="", y="") +
#   geom_line(aes(x=date, y=value, colour=key)) +
#   facet_wrap(~key)
# 
# print(p_testdir)
# 
# 
# 
# # Metrics for Test tracker ----------------------------------------------
# 
# # http://docs.rstudio.com/shinyapps.io/metrics.html#ApplicationMetrics
# FINDCov19Tracker <- rsconnect::showMetrics("container_status",
#                                            c("connect_count", 
#                                              "connect_procs"),
#                                            appName="FINDCov19Tracker",
#                                            server="shinyapps.io",
#                                            from = as.numeric(as.POSIXct("2022-09-01 0:00:00")),
#                                            until = as.numeric(as.POSIXct("2022-12-05 0:00:00")),
#                                            interval="1m"
# ) %>%
#   magrittr::set_colnames(c("dummy", "timestamp", "connect_count", "connect_procs")) %>%
#   mutate(across(c("timestamp", "connect_count", "connect_procs"), as.numeric)) %>%
#   mutate(date=as_datetime(timestamp)) %>% 
#   select(-timestamp) %>% 
#   arrange(date) %>% 
#   mutate(date = date(date),
#          new_connect=case_when(
#            connect_count>lag(connect_count,1) ~ connect_count-lag(connect_count,1),
#            TRUE ~ 0)) %>%
#   group_by(date) %>%
#   summarise(
#     n_count=sum(connect_count),
#     n_procs=sum(connect_procs),
#     n_connect=sum(new_connect) # approximate) %>%
#   ) %>%  
#   select(n_connect, date) %>% 
#   gather(key="key", value="value", -date) %>%
#   mutate(key = "FIND Sars-CoV-2 Test tracker")
# 
# 
# 
# 
# 
# 
# # Metrics for Policy dashboard ----------------------------------------------
# 
# # http://docs.rstudio.com/shinyapps.io/metrics.html#ApplicationMetrics
# FINDCov19Policy <- rsconnect::showMetrics("container_status",
#                                           c("connect_count", 
#                                             "connect_procs"),
#                                           appName="FINDCov19Policy",
#                                           server="shinyapps.io",
#                                           from = as.numeric(as.POSIXct("2022-09-01 0:00:00")),
#                                           until = as.numeric(as.POSIXct("2022-12-05 0:00:00")),
#                                           interval="1m"
# ) %>%
#   magrittr::set_colnames(c("dummy", "timestamp", "connect_count", "connect_procs")) %>%
#   mutate(across(c("timestamp", "connect_count", "connect_procs"), as.numeric)) %>%
#   mutate(date=as_datetime(timestamp)) %>% 
#   select(-timestamp) %>% 
#   arrange(date) %>% 
#   mutate(date = date(date),
#          new_connect=case_when(
#            connect_count>lag(connect_count,1) ~ connect_count-lag(connect_count,1),
#            TRUE ~ 0)) %>%
#   group_by(date) %>%
#   summarise(
#     n_count=sum(connect_count),
#     n_procs=sum(connect_procs),
#     n_connect=sum(new_connect) # approximate) %>%
#   ) %>%  
#   select(n_connect, date) %>% 
#   gather(key="key", value="value", -date) %>%
#   mutate(key = "FIND Sars-CoV-2 Policy dashboard")

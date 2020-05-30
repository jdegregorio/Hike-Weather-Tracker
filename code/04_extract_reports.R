# The purpose of this script is to extract the the weather forecast for each
# hike.

# SETUP -----------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Load required package libraries
library(tidyverse)
library(lubridate)
library(rvest)
library(httr)
library(here)

# Load functions
source(here("code", "helpers.R"))

# Load backpack hikes
df_hikes <- read_rds(here("data", "df_hikes_weather.rds"))


# DOWNLOAD AND PARSE TRIP REPORTS ---------------------------------------------

# Query, download, and parse trip report html
df_hikes <- df_hikes %>%
  mutate(
    report_html = map(hike_id, query_reports),
    data_reports = map(report_html, parse_report_html)
  )

# GATHER REPORT STATISTICS ----------------------------------------------------

# Reports in last 10 days
df_hikes$report_cnt_10day <- map_int(
  df_hikes$data_reports,
  ~ .x %>%
    filter(date_report >= (Sys.Date() - 10)) %>%
    nrow()
)

# Reports in last 20 days
df_hikes$report_cnt_20day <- map_int(
  df_hikes$data_reports,
  ~ .x %>%
    filter(date_report >= (Sys.Date() - 20)) %>%
    nrow()
)

# Reports in last 30 days
df_hikes$report_cnt_30day <- map_int(
  df_hikes$data_reports,
  ~ .x %>%
    filter(date_report >= (Sys.Date() - 30)) %>%
    nrow()
)

# SAVE DATA -------------------------------------------------------------------

# Save hike data with reports
write_rds(df_hikes, here("data", "df_hikes_reports.rds"))
write_csv(df_hikes %>% select(-report_html, -data_reports), here("data", "hikes.csv"))

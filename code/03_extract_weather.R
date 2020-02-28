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
library(feather)

# Load functions
source(here("code", "helpers.R"))

# Load backpack hikes
df_hikes <- read_rds(here("data", "df_hikes_info.rds"))

# Future trip report query to get urls
# https://www.wta.org/go-hiking/hikes/mount-persis/@@related_tripreport_listing?b_start:int=0&b_size:int=1000


# DOWNLOAD HTML FOR BACKPACK HIKE PAGES ---------------------------------------

# Download HTML for hike pages
list_html <- map(
  df_hikes$weather_url,
  read_and_wait
)

# Temporary backup
write_rds(list_html, here("data", "list_weather_html.rds"))


# PARSE WEATHER FORCAST SUMMARY -----------------------------------------------

# Gather forecast data
list_forecasts <- map(
  list_html,
  ~ .x %>% html_nodes(".forecast-tombstone")
)

# Parse forecast day/title
list_title <- map(
  list_forecasts,
  ~ .x %>% 
    html_node(".period-name") %>%
    html_text() %>%
    str_trim()
)

# Parse forecast text/detail
list_text <- map(
  list_forecasts,
  ~ .x %>% 
    html_node(".short-desc") %>%
    html_text() %>%
    str_trim()
)

# Parse temp hi/lo
list_temp <- map(
  list_forecasts,
  ~ .x %>% 
    html_node(".temp") %>%
    html_text() %>%
    str_trim()
)

# Combine into a list of dataframes
df_forecast_summary <- 
  pmap(
    list(list_title, list_text,list_temp),
    ~ tibble(day = ..1, forecast = ..2, temp = ..3) %>%
      filter(day %in% c("Friday", "Saturday", "Sunday")) %>%
      pivot_wider(names_from = day, values_from = c(forecast, temp))
  ) %>% 
  bind_rows() %>%
  rename_all(str_to_lower)

# Join to hike data
df_hikes <- bind_cols(df_hikes, df_forecast_summary)

# PARSE DETAILED WEATHER FORECAST----------------------------------------------

# Gather forecast data
list_forecasts <- map(
  list_html,
  ~ .x %>% html_nodes(".row-forecast")
)

# Parse forecast day/title
list_title <- map(
  list_forecasts,
  ~ .x %>% 
    html_node("b") %>%
    html_text() %>%
    str_trim()
)

# Parse forecast text/detail
list_text <- map(
  list_forecasts,
  ~ .x %>% 
    html_node(".forecast-text") %>%
    html_text() %>%
    str_trim()
)

# Combine into a list of dataframes
df_forecast_detailed <- 
  map2(
    list_title,
    list_text,
    ~ tibble(day = .x, forecast = .y) %>%
      filter(day %in% c("Friday", "Saturday", "Sunday")) %>%
      pivot_wider(names_from = day, names_prefix = "forecast_detail_", values_from = forecast)
  ) %>% 
  bind_rows() %>%
  rename_all(str_to_lower)

# Join with hike data
df_hikes <- bind_cols(df_hikes, df_forecast_detailed)


# SAVE DATA -------------------------------------------------------------------

# Save hike data with info
write_rds(df_hikes, here("data", "df_hikes_weather.rds"))

# DEVELOPMENT -----------------------------------------------------------------

# # GENERATE WEATHER URL ------------------------------------------------------
# 
# df_hikes <- df_hikes %>%
#   mutate(
#     weather_url = paste0(
#       "https://forecast.weather.gov/MapClick.php?lat=", latitude,
#       "&lon=", longitude,
#       "&FcstType=digitalDWML"
#     )
#   )

# # PARSE HOURLY FORECAST -------------------------------------------------------
# 
# # Date/Time
# datetime_all <- map(
#   list_html,
#   ~ .x %>%
#     xml_nodes("start-valid-time") %>% 
#     xml_text() %>% 
#     as_datetime(tz = "US/Pacific")
# )
# 
# # Hourly Temp
# temp_all <- map(
#   list_html,
#   ~ .x %>%
#     xml_nodes('temperature[type="hourly"]') %>% 
#     xml_nodes("value") %>%
#     xml_text() %>%
#     as.numeric()
# )
# 
# # Percipitation %
# percip_pct_all <- map(
#   list_html,
#   ~ .x %>%
#     xml_nodes("probability-of-precipitation") %>% 
#     xml_nodes("value") %>%
#     xml_text() %>%
#     as.numeric()
# )
# 
# # Cloud Cover
# cloudcover_all <- map(
#   list_html,
#   ~ .x %>%
#     xml_nodes("cloud-amount") %>% 
#     xml_nodes("value") %>%
#     xml_text() %>%
#     as.numeric()
# )
# 
# # Weather
# list_html[[1]] %>%
#   xml_nodes("weather-conditions") %>%
#   map(~ .x %>% xml_nodes("value"))

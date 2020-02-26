# The purpose of this script is to extract the hike information including the
# following:
#   - Basic stats
#   - Location/Directions
#   - Trip Reports

# SETUP -----------------------------------------------------------------------

# Load required package libraries
library(tidyverse)
library(lubridate)
library(rvest)
library(httr)
library(here)

# Load functions
source(here("code", "helpers.R"))

# Load backpack hikes
df_hikes <- read_rds(here("data", "df_hikes.rds"))

# Future trip report query to get urls
# https://www.wta.org/go-hiking/hikes/mount-persis/@@related_tripreport_listing?b_start:int=0&b_size:int=1000


# DOWNLOAD HTML FOR BACKPACK HIKE PAGES ---------------------------------------

# Download HTML for hike pages
list_html <- map(
  df_hikes$hike_url,
  read_and_wait
)

# Temporary backup
write_rds(list_html, "list_html.rds")

# PARSE HIKE PAGE DETAILS -----------------------------------------------------

# Hike region
df_hikes$region <- map(
  list_html,
  ~ .x %>%
    html_node("[id~=hike-region]") %>%
    html_text() %>%
    str_trim()
)

# Gather all hike stats
stats_all <- map(
  list_html,
  ~ .x %>%
    html_nodes(".hike-stat") %>%
    html_text() %>%
    str_remove_all("\n") %>%
    str_remove_all("\t") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
)

# Hike Location
df_hikes$location <- stats_all %>%
  map_chr(1) %>%
  str_sub(10, -16)

# Hike Length
df_hikes$length <- stats_all %>%
  map_chr(~paste0(.x, collapse = " ")) %>%
  str_extract("(?<=Length )[:digit:]+.[:digit:](?= miles)") %>%
  as.numeric()

# Hike Length Type
df_hikes$length_type <- stats_all %>%
  map_chr(~paste0(.x, collapse = " ")) %>%
  str_extract("roundtrip|one-way")

# Elevation Gain
df_hikes$elevation_gain <- stats_all %>%
  map_chr(~paste0(.x, collapse = " ")) %>%
  str_extract("(?<=Elevation Gain: )[:digit:]+(?= ft.)") %>%
  as.numeric()

# Highest Point
df_hikes$highest_point <- stats_all %>%
  map_chr(~paste0(.x, collapse = " ")) %>%
  str_extract("(?<=Highest Point: )[:digit:]+(?= ft.)") %>%
  as.numeric()

# Rating
df_hikes$rating <- stats_all %>%
  map_chr(~paste0(.x, collapse = " ")) %>%
  str_extract("[:digit:]{1}.[:digit:]{2}(?= out of 5)") %>%
  as.numeric()

# Review Count
df_hikes$review_count <- stats_all %>%
  map_chr(~paste0(.x, collapse = " ")) %>%
  str_extract("[:digit:]+(?= votes)") %>%
  as.numeric()


# Gather all hike features
features_all <- map(
  list_html,
  ~ .x %>%
    html_nodes(".feature") %>%
    html_attr("data-title")
)

# Extract feature data
df_features <- df_hikes %>%
  select(hike_name) %>%
  mutate(features = features_all, status = TRUE) %>%
  unnest(features) %>%
  pivot_wider(
    id_cols = hike_name,
    names_from = features,
    names_prefix = "feature_",
    values_from = status,
    values_fill = list(status = FALSE)
  ) %>%
  rename_all(.funs = function(s) s %>% str_to_lower() %>% str_squish() %>% str_remove_all("[:space:]") %>% str_remove("/"))

# Join features to hike data
df_hikes <- df_hikes %>% left_join(df_features, by = "hike_name")

# Gather Latitude/Longitude
latlong_all <- map(
  list_html,
  ~ .x %>%
    html_nodes(".latlong") %>%
    html_nodes("span") %>%
    html_text()
)

# Extract latitude and longitude
df_hikes$latitude <- map(latlong_all, 1) %>% as.numeric()
df_hikes$longitude <- map(latlong_all, 2) %>% as.numeric()


# Weather URL
df_hikes$weather_url <- map(
  list_html,
  ~ .x %>%
    html_nodes('a[href^="http://forecast.weather.gov/"]') %>%
    html_attr("href")
)


# The purpose of this script is to extract the current hikes in the Washington
# Trails Association "Backpack" (i.e. saved hikes).

# SETUP -----------------------------------------------------------------------

# Load required package libraries
library(tidyverse)
library(lubridate)
library(rvest)
library(httr)
library(here)
library(keyring)

# Load functions
source(here("code", "helpers.R"))

# Set parameters
username <- key_get("wta_username")
password <- key_get("wta_password")


# WTA LOGIN -------------------------------------------------------------------

# Specify url to the login page
url_login <- "https://www.wta.org/login"


# Create new session, extract login form
session_wta <- html_session(url_login)
form_login <- html_form(session_wta)[[3]]

# Fill out form with email/password
form_filled <- set_values(
  form_login,
  "__ac_name" = username, 
  "__ac_password" = password
)

# Submit form
session_wta <- session_wta %>% 
  submit_form(form_filled)

# Open "Backpack Dashboard" page
session_wta <- session_wta %>% 
  jump_to("https://www.wta.org/backpack")

# Capture base url for backpack
url_backpack <- session_wta$url


# EXTRACT HIKES FROM WTA BACKPACK ---------------------------------------------

# Initialize item query
list_hikes_all <- list()
hike_start <- 0

# Repeat until page is empty
repeat {
  
  # Query items
  session_wta <- session_wta %>% 
    jump_to(paste0(url_backpack, "/hikes?b_start:int=", hike_start))
  
  # Capture the list of hikes, add to master list
  list_hikes_page <- session_wta %>% html_nodes(".item")
  list_hikes_all  <- list_hikes_all %>% append(list_hikes_page)
  
  # Check hike count on page
  if (length(list_hikes_page) > 0) {
    hike_start <- hike_start + 25
  } else{
    break
  }
  
  # Delay
  wait()
  
}

# EXTRACT BASIC BACKPACK HIKE INFORMATION -------------------------------------

# Gather name, url, and hiked status in dataframe
df_hikes <-
  tibble(html = list_hikes_all) %>%
  mutate(
    class = map_chr(html, ~ .x %>% html_attr("class")),
    hike_name = map_chr(html, ~ .x %>% html_attr("data-hikename")),
    hike_url = map_chr(html, ~ .x %>% html_node("a") %>% html_attr("href")),
    flag_hiked = class == "item hiked"
  ) %>%
  select(hike_name, hike_url, flag_hiked) %>%
  distinct()


# Save dataframe
write_rds(df_hikes, here("data", "df_hikes.rds"))

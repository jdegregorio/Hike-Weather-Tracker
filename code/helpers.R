
# Function to execute random sleep duration
wait <- function(min = 4, max = 6) {
  
  # Set delay based on normal distribution
  delay <- runif(1, min, max)
  
  return(Sys.sleep(delay))
}

# Function to read HTML and then wait 
read_and_wait <- function (url) {
  
  # Print URL
  print(url)
  
  # Read HTML
  html <- read_html(url)
  
  # Wait
  wait()

  return(html)
}

# Function to create hike report query url
create_report_query_url <- function(hike_id, int_start = 0, int_interval = 100) {

  # Construct URL
  url <- paste0(
    "https://www.wta.org/go-hiking/hikes/",
    hike_id,
    "/@@related_tripreport_listing?",
    "b_start:int=", int_start,
    "&b_size:int=", int_interval
  )
  
  return(url)
}


# Function to create trip report query
query_reports <- function(hike_id, int_start = 0, int_interval = 100, limit = 100) {
  
  # Print Hike ID
  print(hike_id)

  # Create url
  url <- create_report_query_url(hike_id, int_start, int_interval)
  
  # Load HTML
  html <- read_and_wait(url)
  
  # Check data count
  report_count <- 
    html_node(html, "#count-data") %>% 
    html_text() %>%
    as.numeric()
  
  # Limit report count
  if (report_count > limit) {
    report_count <- limit
  }
  
  # If neccessary, create grid to download remaining reports
  if (report_count > int_interval) {
    
    # Create query grid
    grid_html <-
      tibble(
        start = seq(int_interval, report_count, int_interval),
        url = map_chr(start, ~create_report_query_url(hike_id, .x, int_interval))
      )
    
    # Remove last query if limited
    if (report_count == limit) {
      grid_html <- grid_html %>% slice(-nrow(grid_html))
    }
    
    # Download HTML
    grid_html$html <- map(
      grid_html$url,
      read_and_wait
    )
    
    # Compile list
    list_html <- c(list(html), grid_html$html)
    
  } else {
    
    # Compile list
    list_html <- list(html)
    
  }
  
  # Parse html into individual reports
  list_report_html <- map(
    list_html,
    ~ html_nodes(.x, ".item-row")
  )
  
  # Flatten into long list
  list_report_html <- flatten(list_report_html)
  

  return(list_report_html)
}

# Function to parse reports
parse_report_html <- function(list_report_html) {
  
  # Date
  list_date <- 
    map_chr(
      list_report_html,
      ~ .x %>%
        html_node(".elapsed-time") %>% 
        html_attr("title")
    ) %>%
    mdy()
  
  # Trail issues
  list_issues <- 
    map_chr(
      list_report_html,
      ~ .x %>%
        html_node(".trail-issues") %>%
        html_text() %>%
        str_trim() %>%
        str_extract("(?<=Beware of:\n              )([:graph:]|[:space:])*")
    )
  
  # Compile dataframe
  df_reports <- tibble(date_report = list_date, issues = list_issues)
  
  return(df_reports)
}
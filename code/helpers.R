
# Function to execute random sleep duration
wait <- function(mean = 3, sd = 1, min = 1) {
  
  repeat {
    
    # Set delay based on normal distribution
    delay <- rnorm(1, mean, sd)
    
    # Check if it is above the min, if so, then break
    if (delay >= min) break

  }
  
  return(Sys.sleep(delay))
}

# Function to read HTML and then wait 
read_and_wait <- function (url) {
  
  # Read HTML
  html <- read_html(url)
  
  # Wait
  wait()

  return(html)
}

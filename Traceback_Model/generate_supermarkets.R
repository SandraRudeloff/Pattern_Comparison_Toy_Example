library(dplyr)

generate_shops <- function(no_of_cells, chain_data){
  cells_per_row <- sqrt(no_of_cells)
  field <- expand.grid(x = seq(0.05, 1, by = 0.1), y = seq(0.05, 1, by = 0.1))
  
  num_stores <- chain_data$num_stores
  spatial_distribution <- chain_data$spatial_distribution
  
  total_sales <- chain_data$total_sales
  sales_distribution <- chain_data$sales_distribution
  
  df_shops <- switch(
    spatial_distribution,
    "random" = generate_random_shops(field, num_stores),
    "uniform" = generate_uniform_shops(field, num_stores)
  )
  
  df_shops$sales <- switch(
    spatial_distribution,
    "random" = generate_random_sales(num_stores),
    "uniform" = generate_uniform_sales(num_stores)
  )
  
  return(df_shops)
}


generate_uniform_sales <- function(num_stores) {
  sales <- rep(1, num_stores)
  return(sales)
}

generate_random_sales <- function(num_stores) {
  sales <- rep(1, num_stores)
  return(sales)
}

# Single Chain random ----
generate_random_shops <- function(field, num_shops) {

  # Randomly select 'num_shops' number of cells from the field
  random_shops <- field %>% sample_n(num_shops) %>%
    rename(store_x = x, store_y = y)
  
  return(random_shops)
}


# Single Chain Uniform ----




# Usage
# set.seed(123) # Set seed for reproducibility
# num_shops <- 10 # Specify the number of shops to distribute
# shop_locations <- generate_random_shops(num_shops)
# print(shop_locations)


# single chain population based ----

# Two clustered ----

# Two opposing ----

# Two mixed ----

# Two random ----


# # Generate supermarket data
# supermarket_data <- generate_random_supermarket_data("Chain1", 2)
# 
# # Create a blank plot
# plot(1, 1, xlim = c(0, 1000), ylim = c(0, 1000), type = "n", xlab = "X", ylab = "Y", main = "Supermarket Locations")
# 
# # Add the supermarket locations
# points(supermarket_data$x, supermarket_data$y, pch = 19, col = "blue")
# 
# # Add grid lines to represent the cells
# abline(v = seq(0, 1000, by = 100), h = seq(0, 1000, by = 100), col = "lightgray", lty = "dotted")

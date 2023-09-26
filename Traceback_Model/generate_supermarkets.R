library(dplyr)
library(ggplot2)
set.seed(123) # Set seed for reproducibility

generate_shops <- function(no_of_cells, chain_data) {
  cells_per_row <- sqrt(no_of_cells)
  field <- expand.grid(x = seq(0.05, 1, by = 0.1), y = seq(0.05, 1, by = 0.1))

  num_stores <- chain_data$num_stores
  spatial_distribution <- chain_data$spatial_distribution

  total_sales <- chain_data$total_sales
  sales_distribution <- chain_data$sales_distribution

  df_shops <- switch(spatial_distribution,
    "random" = generate_random_shops(field, num_stores),
    "uniform" = generate_uniform_shops(field, num_stores)
  )

  df_shops$sales <- switch(sales_distribution,
    "random" = generate_random_sales(num_stores, total_sales),
    "uniform" = generate_uniform_sales(num_stores, total_sales)
  )

  return(df_shops)
}

# Sales ----
generate_uniform_sales <- function(num_stores, total_sales) {
  sales <- rep(total_sales / num_stores, num_stores)
  return(sales)
}

generate_random_sales <- function(num_stores, total_sales) {
  sales <- sample(1:100, num_stores, replace = TRUE)

  # Normalize the random numbers so they sum up to total_sales
  sales <- (sales / sum(sales)) * total_sales

  sales <- round(sales, 2)

  # Adjust the last element to make sure the sum is exactly total_sales
  sales[num_stores] <- total_sales - sum(sales[-num_stores])

  return(sales)
}

#  Spatial Distibution ----
plot_stores <- function(field, stores) {
  # Plotting
  ggplot() +
    geom_tile(data = field, aes(x = x, y = y), alpha = 0.1) +
    geom_point(data = stores, aes(x = x, y = y), color = "red", size = 4) +
    coord_fixed(ratio = 1) +
    ggtitle("Stores") +
    xlab("X Coordinate") +
    ylab("Y Coordinate") +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_minimal()
}

## Single Chain Random ----
generate_random_shops <- function(field, num_shops) {
  # Randomly select 'num_shops' number of cells from the field
  random_shops <- field %>% sample_n(num_shops)

  return(random_shops)
}


## Single Chain Uniform ----
served_cells <- function(field, shops) {
  # Assign each cell in the grid to the closest site
  field$closest_site <- apply(field, 1, function(point) {
    distances <- sqrt((point[1] - shops$x)^2 + (point[2] - shops$y)^2)
    closest_site <- which.min(distances)
    return(closest_site)
  })
  # Count the number of cells attracted by each site
  cell_count <- table(field$closest_site)
  return(cell_count)
}

generate_uniform_shops <- function(field, num_shops) {
  # Initialize random points
  initial_shops <- data.frame(
    x = runif(num_shops, min(field$x), max(field$x)),
    y = runif(num_shops, min(field$y), max(field$y))
  )

  # K-Means to find centroids
  result <- kmeans(field, centers = initial_shops, iter.max = 1000, nstart = 10)

  # Extract the centroids
  final_shops <- as.data.frame(result$centers)

  return(final_shops)
}

#
# field <- expand.grid(x = seq(0.05, 1, by = 0.1), y = seq(0.05, 1, by = 0.1))
# # Generate stores
# num_stores <- 15
# stores <- generate_uniform_shops( field, num_stores)
# plot_stores(field, stores)
# print(stores)
# print(served_cells(field, stores))


## Single Chain Population based ----
generate_populationBased_shops <- function(field, num_shops, df_population) {

}

# Two clustered ----

# Two opposing ----

# Two mixed ----

# Two random ----

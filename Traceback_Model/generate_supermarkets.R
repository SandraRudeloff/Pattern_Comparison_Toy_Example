library(dplyr)
library(ggplot2)
library(RColorBrewer) # visualization with color set
library(grid)
library(ggnewscale)
set.seed(123) # Set seed for reproducibility

generate_shops <- function(no_of_cells, chain_data, df_population, is_first_chain, df_shops_chain1 = NULL) {
  field <- expand.grid(x = seq(0.05, 1, by = 0.1), y = seq(0.05, 1, by = 0.1))
  num_stores <- chain_data$num_stores
  spatial_distribution <- chain_data$spatial_distribution

  total_sales <- chain_data$total_sales
  sales_distribution <- chain_data$sales_distribution

  if (spatial_distribution == "clustered") {
    radius <- chain_data$radius
  }

  if (!is_first_chain && !is.null(df_shops_chain1)) {
    df_shops <- switch(spatial_distribution,
      "clustered" = generate_clustered_shops(df_shops_chain1, num_stores, radius),
      "opposing" = generate_opposing_shops(df_shops_chain1, num_stores, field),
      "random" = generate_random_shops(field, num_stores),
      "uniform" = generate_uniform_shops(field, num_stores),
      "popBased" = generate_populationBased_shops(field, num_stores, df_population, inverse = FALSE),
      "invPopBased" = generate_populationBased_shops(field, num_stores, df_population, inverse = TRUE),
    )
  } else {
    df_shops <- switch(spatial_distribution,
      "random" = generate_random_shops(field, num_stores),
      "uniform" = generate_uniform_shops(field, num_stores),
      "popBased" = generate_populationBased_shops(field, num_stores, df_population, inverse = FALSE),
      "invPopBased" = generate_populationBased_shops(field, num_stores, df_population, inverse = TRUE),
    )
  }

  if (!is_first_chain && !is.null(df_shops_chain1)) {
    df_shops$sales <- switch(sales_distribution,
      "random" = generate_random_sales(num_stores, total_sales),
      "uniform" = generate_uniform_sales(num_stores, total_sales),
      "popBased" = generate_populationBased_sales(df_shops, df_population, total_sales, inverse = FALSE),
      "invPopBased" = generate_populationBased_sales(df_shops, df_population, total_sales, inverse = TRUE),
      "flagship" = generate_flagship_sales(df_shops, df_population, total_sales),
      "adjustedPopBased" = generate_adjusted_populationBased_sales(df_shops_chain1, df_shops, df_population, total_sales)
    )
  } else {
    df_shops$sales <- switch(sales_distribution,
      "random" = generate_random_sales(num_stores, total_sales),
      "uniform" = generate_uniform_sales(num_stores, total_sales),
      "popBased" = generate_populationBased_sales(df_shops, df_population, total_sales, inverse = FALSE),
      "invPopBased" = generate_populationBased_sales(df_shops, df_population, total_sales, inverse = TRUE),
      "flagship" = generate_flagship_sales(df_shops, df_population, total_sales)
    )
  }

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

get_sales_potential <- function(store_cell, df_population, inverse) {
  distances <- sqrt((df_population$x_centroid - store_cell[1])^2 + (df_population$y_centroid - store_cell[2])^2)

  if (inverse) {
    raw_sales_potential <- distances
  } else {
    raw_sales_potential <- 1 / (distances + 1e-9) # Adding a small constant to avoid division by zero
  }

  # Scale the raw sales potential values by the population in each cell
  scaled_sales_potential <- sum(raw_sales_potential * df_population$population)

  return(scaled_sales_potential)
}

generate_populationBased_sales <- function(df_stores, df_population, total_sales, inverse) {
  df_stores$sales_potential <- apply(df_stores, 1, function(row) get_sales_potential(row, df_population, inverse))

  # Normalize this to get sales distribution ratios
  df_stores$sales_ratio <- df_stores$sales_potential / sum(df_stores$sales_potential)

  # Distribute total sales
  df_stores$sales <- round(df_stores$sales_ratio * total_sales, 2)

  # Adjust the last element to make sure the sum is exactly total_sales
  df_stores$sales[nrow(df_stores)] <- total_sales - sum(df_stores$sales[-nrow(df_stores)])

  return(df_stores$sales)
}

### Single Chain Flagship
generate_flagship_sales <- function(df_shops, df_population, total_sales) {
  # Calculate sales potential for each store
  df_shops$sales_potential <- apply(df_shops, 1, function(row) get_sales_potential(row, df_population, inverse = FALSE))

  flagship_store <- which.max(df_shops$sales_potential)

  # Allocate a significant portion of total sales to the flagship store
  flagship_sales <- 0.5 * total_sales # 50% of total sales

  # Distribute the remaining sales among the other stores
  remaining_sales <- total_sales - flagship_sales
  df_shops$sales_ratio <- df_shops$sales_potential / sum(df_shops$sales_potential[-flagship_store])
  df_shops$sales <- round(df_shops$sales_ratio * remaining_sales, 2)

  # Assign sales to the flagship store
  df_shops$sales[flagship_store] <- flagship_sales

  return(df_shops$sales)
}

generate_adjusted_populationBased_sales <- function(df_shops_chain1, df_shops_chain2, df_population, total_sales_chain2) {
  # Calculate sales potential for each store in chain 2
  df_shops_chain2$sales_potential <- apply(df_shops_chain2, 1, function(row) get_sales_potential(row, df_population, inverse = FALSE))

  for (i in seq_len(nrow(df_shops_chain2))) {
    store2 <- df_shops_chain2[i, ]
    for (j in seq_len(nrow(df_shops_chain1))) {
      store1 <- df_shops_chain1[j, ]
      distance <- sqrt((store2$x - store1$x)^2 + (store2$y - store1$y)^2)

      # Reduce sales potential if another chain's store is within a certain distance (e.g., 0.1 units)
      if (distance < 0.1) {
        df_shops_chain2$sales_potential[i] <- df_shops_chain2$sales_potential[i] * 0.9 # reduce by 10%
      }
    }
  }

  # Normalize to get sales distribution ratios
  df_shops_chain2$sales_ratio <- df_shops_chain2$sales_potential / sum(df_shops_chain2$sales_potential)

  # Distribute total sales
  df_shops_chain2$sales <- round(df_shops_chain2$sales_ratio * total_sales_chain2, 2)

  # Adjust the last element to make sure the sum is exactly total_sales
  df_shops_chain2$sales[nrow(df_shops_chain2)] <- total_sales_chain2 - sum(df_shops_chain2$sales[-nrow(df_shops_chain2)])

  return(df_shops_chain2$sales)
}
#  Spatial Distibution ----
plot_stores <- function(field, df_shops, investigation_scenario) {
  unique_chains <- unique(df_shops$chain)
  all_colors <- brewer.pal(9, "Set1")
  chain_colors <- all_colors[1:length(unique_chains)]
  names(chain_colors) <- unique_chains
  
  p_main <- ggplot() +
    geom_point(
      data = df_shops,
      aes(x = x, y = y, fill = chain),
      size = 3,
      shape = 23,
      alpha = 0.8
    ) +
    scale_fill_manual(values = chain_colors, name = "Shop Chain") +

    # Adjust the x and y axis breaks to have lines every 100m
    scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) + 
    scale_y_continuous(breaks = seq(0, 1, by = 0.1),limits = c(0, 1)) + 

    # Add labels and theme
    labs(
      title = sprintf("Stores - Investigation Scenario: %s", investigation_scenario),
      x = "X Coordinate",
      y = "Y Coordinate",
      color = "Shop Chain"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      aspect.ratio = 1,
      panel.grid.minor = element_blank()
    )

  df_shops$sales <- format(round(df_shops$sales, 2), big.mark = ",")
  df_shops$x <- round(df_shops$x, 4)
  df_shops$y <- round(df_shops$y, 4)

  table_grob_shops <- tableGrob(df_shops[, c("chain", "x", "y", "sales")], rows = NULL)
  
  grid.arrange(
    p_main,
    table_grob_shops,
    ncol = 2
  )
  
}

# Function to adjust coordinates if they lie on the grid
# Function to adjust coordinates if they lie on the grid
adjust_coordinates <- function(coord) {
  # Define a small tolerance value for numerical comparisons
  tolerance <- 1e-9
  
  x <- coord[1]
  y <- coord[2]
  
  # Check if x is "close enough" to being divisible by 0.1
  if (((x *10)%% 1) < tolerance) {
    x <- x + runif(1, -1e-5, 1e-5)
  }
  
  # Check if y is "close enough" to being divisible by 0.1
  if (((y *10) %% 1) < tolerance) {
    y <- y + runif(1, -1e-5, 1e-5)
  }
  
  return(c(x, y))
}



## Single Chain ----
### Single Chain Random ----
generate_random_shops <- function(field, num_shops) {
  # Randomly select 'num_shops' number of cells from the field
  random_shops <- field %>% sample_n(num_shops)
  
  adjusted_coords <- t(apply(random_shops, 1, adjust_coordinates))
  random_shops$x <- adjusted_coords[, 1]
  random_shops$y <- adjusted_coords[, 2]
  
  return(random_shops)
}

### Single Chain Uniform ----
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

  adjusted_coords <- t(apply(final_shops, 1, adjust_coordinates))
  final_shops$x <- adjusted_coords[, 1]
  final_shops$y <- adjusted_coords[, 2]
  
  return(final_shops)
}

## Single Chain Population based ----
generate_populationBased_shops <- function(field, num_shops, df_population, inverse) {
  # Normalize the population to get weights
  total_population <- sum(df_population$population)
  weights <- df_population$population / total_population

  if (inverse) {
    weights <- 1 - weights
    weights <- weights / sum(weights) # Normalize the inverted weights
  }

  # Randomly sample cell_ids based on weights
  selected_cells <- sample(df_population$cell_id, size = num_shops, replace = TRUE, prob = weights)

  df_shops <- data.frame(x_centroid = numeric(), y_centroid = numeric())
  for (cell in selected_cells) {
    selected_row <- df_population[df_population$cell_id == cell, ]
    df_shops <- rbind(df_shops, selected_row[, c("x_centroid", "y_centroid")])
  }
  colnames(df_shops) <- c("x", "y")
  
  return(df_shops)
}

## Two Chains ----
### Two Chains clustered ----
generate_clustered_shops <- function(df_shops_chain1, num_stores_chain2, radius) {
  # Initialize an empty data frame to store the locations for chain 2
  df_chain2 <- data.frame(x = numeric(), y = numeric())

  for (i in 1:num_stores_chain2) {
    # Step 1: Randomly select a store from chain 1
    seed_row <- df_shops_chain1[sample(nrow(df_shops_chain1), 1), ]
    
    repeat {
      # Step 2: Generate nearby coordinates
      angle <- runif(1, 0, 2 * pi)
      distance <- runif(1, 0, radius)
      new_x <- seed_row$x + distance * cos(angle)
      new_y <- seed_row$y + distance * sin(angle)
      
      # Check if the new coordinates are within the field
      if (new_x >= 0 && new_x <= 1 && new_y >= 0 && new_y <= 1) {
        break
      }
    }
    df_chain2 <- rbind(df_chain2, data.frame(x = new_x, y = new_y))
  }
  
  adjusted_coords <- t(apply(df_chain2, 1, adjust_coordinates))
  
  df_chain2$x <- adjusted_coords[, 1]
  df_chain2$y <- adjusted_coords[, 2]

  return(df_chain2)
}

### Two Chains opposing ----
generate_opposing_shops <- function(df_shops_chain1, num_stores_chain2, field) {
  df_chain2 <- data.frame(x = numeric(), y = numeric())

  # Calculate the minimum distance from each point in the field to any chain 1 store
  field$min_distance <- apply(field, 1, function(row) {
    min_distance <- min(sqrt((df_shops_chain1$x - row["x"])^2 + (df_shops_chain1$y - row["y"])^2))
    return(min_distance)
  })

  # Sort the field by this minimum distance, in descending order
  sorted_field <- field[order(-field$min_distance), ]

  # Select the top num_stores_chain2 locations
  df_chain2 <- sorted_field[1:num_stores_chain2, c("x", "y")]

  adjusted_coords <- t(apply(df_chain2, 1, adjust_coordinates))
  
  df_chain2$x <- adjusted_coords[, 1]
  df_chain2$y <- adjusted_coords[, 2]
  
  return(df_chain2)
}

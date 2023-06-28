no_of_cells <- 100
total_population <- 5000
population_type <- "main_and_small_clusters"
desired_gradient <- 50 # high values mean a large spreading # used for all radial type populations

# df_population init ----
centroid_coords <- seq(50, by = 100, length.out = sqrt(no_of_cells)) # sqrt(no_of_cells): number of cells per row
df_population <- expand.grid(x_centroid = centroid_coords, y_centroid = centroid_coords)


# Random ---- 
if (population_type == "random") {

  # Generate random proportions
  proportions <- runif(nrow(df_population))
  
  # Scale the proportions so that they sum up to the total population
  population <- total_population * proportions / sum(proportions)
  
  # Round down the population counts to the nearest integer
  df_population$population <- floor(population)
  
  # Calculate the remaining population
  remaining_population <- total_population - sum(df_population$population)
  
  # Randomly assign the remaining population to some of the cells
  selected_cells <- sample(nrow(df_population), abs(remaining_population), replace = TRUE)
  if (remaining_population > 0) {
    df_population$population[selected_cells] <- df_population$population[selected_cells] + 1
  } else if (remaining_population < 0) {
    df_population$population[selected_cells] <- df_population$population[selected_cells] - 1
  }
}

# Radial ----
if (population_type == "radial") {
  # Randomly select a center cell
  center_cell <- sample(1:nrow(df_population), 1)
  center_x <- df_population$x_centroid[center_cell]
  center_y <- df_population$y_centroid[center_cell]
  
  # Calculate the distances from each cell to the center cell
  distances <- sqrt((df_population$x_centroid - center_x)^2 + (df_population$y_centroid - center_y)^2)
  
  # Calculate the raw population values (not yet scaled to the total population)
  raw_population <- 1 / (1 + distances / desired_gradient)
  
  # Scale the raw population values so that they sum up to the total population
  population <- round(total_population * raw_population / sum(raw_population))
  
  # Calculate the remaining population
  remaining_population <- abs(total_population - sum(population))
  
  # Randomly assign the remaining population to some of the cells
  selected_cells <- sample(nrow(df_population), remaining_population, replace = TRUE)
  population[selected_cells] <- population[selected_cells] + 1
  
  # Assign the calculated population values to the data frame
  df_population$population <- population
  
}

# Radial same size Clusters ----
if (population_type == "radial_clusters") {
  num_clusters <- 5 # or calculate based on desired entropy
  # Initialize the population distribution
  population <- rep(0, no_of_cells)
  
  # For each cluster...
  for (i in 1:num_clusters) {
    # Randomly select a center cell
    center_cell <- sample(1:nrow(df_population), 1)
    print(center_cell)
    center_x <- df_population$x_centroid[center_cell]
    center_y <- df_population$y_centroid[center_cell]
    
    # Calculate the distances from each cell to the center cell
    distances <- sqrt((df_population$x_centroid - center_x)^2 + (df_population$y_centroid - center_y)^2)
    
    # Calculate the raw population values (not yet scaled to the total population)
    raw_population <- 1 / (1 + distances / desired_gradient) # 1 in denominator avoid division by zero and to ensure that the raw population values are within the range of 0 to 1.
    
    # Scale the raw population values so that they sum up to the cluster's population
    cluster_population <- total_population / num_clusters # or vary between clusters
    population <- population + round(cluster_population * raw_population / sum(raw_population))
  }
  
  # Randomly assign the remaining population to some of the cells
  remaining_population <- total_population - sum(population)
  selected_cells <- sample(nrow(df_population), abs(remaining_population))
  
  if (remaining_population > 0) {
    population[selected_cells] <- population[selected_cells] + 1
  } else if (remaining_population < 0) {
    population[selected_cells] <- population[selected_cells] - 1
  }
  
  
  # Assign the calculated population values to the data frame
  df_population$population <- population
}

# Radial Main and small clusters ----
if (population_type == "main_and_small_clusters") {
  num_clusters <- 5 # or calculate based on desired entropy

  
  # Initialize the population distribution
  population <- rep(0, no_of_cells)
  
  # Randomly select a center cell for the main cluster
  main_cluster_cell <- sample(1:nrow(df_population), 1)
  main_cluster_x <- df_population$x_centroid[main_cluster_cell]
  main_cluster_y <- df_population$y_centroid[main_cluster_cell]
  
  # Calculate the distances from each cell to the main cluster
  main_cluster_distances <- sqrt((df_population$x_centroid - main_cluster_x)^2 + (df_population$y_centroid - main_cluster_y)^2)
  
  # Calculate the raw population values for the main cluster (not yet scaled to the total population)
  main_cluster_raw_population <- 1 / (1 + main_cluster_distances / desired_gradient) # 1 in denominator to avoid division by zero and to ensure that the raw population values are within the range of 0 to 1.
  
  # Scale the raw population values for the main cluster so that they sum up to the main cluster's population
  main_cluster_population_percentage <- 0.6 - (num_clusters - 1) * 0.05
  main_cluster_population <- (total_population * main_cluster_population_percentage) 
  population <- population + round(main_cluster_population * main_cluster_raw_population / sum(main_cluster_raw_population))
  
  # For each small cluster...
  for (i in 1:(num_clusters - 1)) {
    # Randomly select a center cell for the small cluster
    small_cluster_cell <- sample(1:nrow(df_population), 1)
    small_cluster_x <- df_population$x_centroid[small_cluster_cell]
    small_cluster_y <- df_population$y_centroid[small_cluster_cell]
    
    # Calculate the distances from each cell to the small cluster
    small_cluster_distances <- sqrt((df_population$x_centroid - small_cluster_x)^2 + (df_population$y_centroid - small_cluster_y)^2)
    
    # Calculate the raw population values for the small cluster (not yet scaled to the total population)
    small_cluster_raw_population <- 1 / (1 + small_cluster_distances / desired_gradient) # 1 in denominator to avoid division by zero and to ensure that the raw population values are within the range of 0 to 1.
    
    # Scale the raw population values for the small cluster so that they sum up to the small cluster's population
    small_cluster_population <- (total_population * (1 - main_cluster_population_percentage)) / (num_clusters - 1) # or vary between clusters
    population <- population + round(small_cluster_population * small_cluster_raw_population / sum(small_cluster_raw_population))
  }
  # Calculate the remaining population
  remaining_population <- total_population - sum(population)
  # Randomly assign the remaining population to some of the cells
  selected_cells <- sample(nrow(df_population), abs(remaining_population))
  
  if (remaining_population > 0) {
    population[selected_cells] <- population[selected_cells] + 1
  } else if (remaining_population < 0) {
    population[selected_cells] <- population[selected_cells] - 1
  }
  
  # Assign the calculated population values to the data frame
  df_population$population <- population
}

  
# Plotting ----
library(ggplot2)
library(plotly)

p <- ggplot(df_population, aes(x = x_centroid, y = y_centroid, fill = population)) +
  geom_tile(color = "gray") +
  scale_fill_gradient(low = "white", high = "red") +
  coord_equal() +
  labs(title = "Population Distribution", x = "X Centroid", y = "Y Centroid", fill = "Population") +
  theme_minimal()

# Convert the ggplot figure to a plotly figure
p <- ggplotly(p)

# Print the plot
print(p)

no_of_cells <- 100
no_of_entropy_steps <- 5
total_population <- 5000
method <- "radial"

# Define Entropy steps
min_entropy <- 0 #very clustered
max_entropy <- log(no_of_cells) #uniform
desired_entropies = seq(min_entropy, max_entropy, length.out = 5)

# generate a sequence of coordinates for centroids
centroid_coords <- seq(50, by = 100, length.out = sqrt(no_of_cells)) # sqrt(no_of_cells): number of cells per row
df_population <- expand.grid(x_centroid = centroid_coords, y_centroid = centroid_coords)

# Function to generate a population distribution with a given entropy
generate_population <- function(desired_entropy, total_population) {
  # Initialize the population distribution
  population <- rep(0, no_of_cells)
  # Calculate the number of clusters based on the desired entropy
  # For simplicity, we'll assume that each cluster has the same population
  num_clusters <- round(exp(desired_entropy))
  
  if (method == "radial") {
    # Randomly select a center cell
    center_cell <- sample(1:no_of_cells, 1)
    # Assign the population to the cells in a radial pattern
    for (i in 1:no_of_cells) {
      distance <- abs(i - center_cell)
      population[i] <- total_population / (1 + distance)
    }
  }
  
  else if (method == "main_and_small_clusters") {
    # Randomly select a main cluster
    main_cluster <- sample(1:no_of_cells, 1)
    # Assign a large portion of the population to the main cluster
    population[main_cluster] <- 0.8 * total_population
    # Assign the remaining population to multiple smaller clusters
    remaining_clusters <- num_clusters - 1
    small_cluster_cells <- sample((1:no_of_cells)[-main_cluster], remaining_clusters)
    population[small_cluster_cells] <- 0.2 * total_population / remaining_clusters
  }
  
  
  
  
  # this is worse than my random before because it is random everywhere the same and it is not random clusters
  else if(method == "random") {
    # Randomly select cells for the clusters
    cluster_cells <- sample(1:no_of_cells, num_clusters)
    # Assign the population to the clusters
    population[cluster_cells] <- total_population / num_clusters
    
  }
    
  else if (method == "centered") {
      # Assign one cluster to the center cell
      center_cell <- no_of_cells %/% 2 + 1
      population[center_cell] <- total_population / 2
      # Assign the rest of the clusters to cells evenly spaced around the center
      remaining_clusters <- num_clusters - 1
      for (i in 1:remaining_clusters) {
        cell <- (center_cell - 1 + no_of_cells * (i - 1) / remaining_clusters) %% no_of_cells + 1
        population[cell] <- total_population / (2 * remaining_clusters)
      }}
  
  else if (method == "lin_grad") {
    # Assign the population to the clusters
    # For simplicity, we'll assign the clusters to the first few cells
    # In practice, you might want to assign the clusters to random cells or use some other method
    population[1:num_clusters] <- total_population / num_clusters
  }

  
  return(population)
}

population_distributions <- list()
for (desired_entropy in desired_entropies) {
  df_population$population <- generate_population(desired_entropy, total_population)
  
  # Check the entropy of the generated distribution
  # entropy <- calculate_entropy(df_population$population)
  
  # Store the population distribution
  population_distributions[[paste0("entropy_", desired_entropy)]] <- df_population
}

library(ggplot2)

# Function to plot a population distribution
plot_population <- function(df_population, title) {
  ggplot(df_population, aes(x = x_centroid, y = y_centroid, fill = population)) +
    geom_tile(color = "gray") +
    scale_fill_gradient(low = "white", high = "red") +
    coord_equal() +
    labs(title = title, x = "X Centroid", y = "Y Centroid", fill = "Population") +
    theme_minimal()
}

# Plot the population distributions
for (i in seq_along(population_distributions)) {
  print(plot_population(population_distributions[[i]], names(population_distributions)[i]))
}




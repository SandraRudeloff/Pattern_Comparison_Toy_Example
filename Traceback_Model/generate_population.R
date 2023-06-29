library(ggplot2)
library(plotly)

assign_remaining_population <- function(df_population, total_population) {
  remaining_population <- total_population - sum(df_population$population)

  # Randomly assign the remaining population to some of the cells
  selected_cells <- sample(nrow(df_population), abs(remaining_population), replace = TRUE)
  df_population$population[selected_cells] <- df_population$population[selected_cells] + sign(remaining_population)

  return(df_population)
}

# Uniform ----
generate_uniform_population <- function(df_population, total_population) {
  df_population$population <- floor(total_population / nrow(df_population))
  df_population <- assign_remaining_population(df_population, total_population)
}

# Random ----
generate_random_population <- function(df_population, total_population) {
  proportions <- runif(nrow(df_population)) # generates random numbers following a uniform distribution

  # Scale the proportions so that they sum up to the total population
  population <- total_population * proportions / sum(proportions)

  df_population$population <- floor(population)

  df_population <- assign_remaining_population(df_population, total_population)

  return(df_population)
}

# Linear ----
generate_linear_population <- function(df_population, total_population, desired_gradient) {
  # Calculate the raw population values (not yet scaled to the total population)
  raw_population <- exp(-df_population$x_centroid / desired_gradient)

  # Scale the raw population values so that they sum up to the total population
  df_population$population <- round(total_population * raw_population / sum(raw_population))

  df_population <- assign_remaining_population(df_population, total_population)

  return(df_population)
}

# Radial Clusters ----
# Radial cluster with num_cluster = 1 is the same as the old radial code
generate_radial_clusters_population <- function(df_population, total_population, desired_gradient, num_clusters) {
  # Initialize the population distribution
  population <- rep(0, no_of_cells)

  # For each cluster...
  for (i in 1:num_clusters) {
    # Randomly select a center cell
    center_cell <- sample(1:nrow(df_population), 1)
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

  df_population$population <- population
  df_population <- assign_remaining_population(df_population, total_population)

  return(df_population)
}

# Radial main and small clusters ----
generate_main_and_small_clusters_population <- function(df_population, total_population, desired_gradient, num_clusters) {
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

  df_population$population <- population

  df_population <- assign_remaining_population(df_population, total_population)

  return(df_population)
}

# Plotting ----
plot_population <- function(df_population, investigation_scenario) {
  if (!dir.exists("Results/Population")) {
    dir.create("Results/Population", recursive = TRUE)
  }
  file_name = paste0(sprintf("Results/Population/Population_Scenario_%s", investigation_scenario), ".png")
  
  # Create the plot
  p <- ggplot(df_population, aes(x = x_centroid, y = y_centroid)) +
    geom_tile(aes(fill = population), color = "gray") +
    scale_fill_gradient(low = "white", high = "darkred", guide = "none") +  # Disable the default legend
    scale_x_continuous(breaks = seq(min(df_population$x_centroid - 50), max(df_population$x_centroid + 50), by = 200)) +  # Set x-axis breaks
    scale_y_continuous(breaks = seq(min(df_population$y_centroid -50), max(df_population$y_centroid +50 ), by = 200)) +
    coord_equal() +
    labs(title = sprintf("Population Distribution Scenario %s", investigation_scenario), x = "X Centroid", y = "Y Centroid") +
    theme_minimal()
  
  # Create a custom legend
  legend_df <- data.frame(
    population = seq(min(df_population$population), max(df_population$population), length.out = 100)
  )
  p_legend <- ggplot(legend_df, aes(x = 1, y = population, fill = population)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "darkred") +
    scale_y_continuous(breaks = floor(seq(min(df_population$population), max(df_population$population), length.out = 5))) +  # Add labels to the legend
    theme_minimal() +
    labs(title = "", x = "", y = "") + 
    theme(legend.position = "none", axis.text.x = element_blank(), axis.ticks.x = element_blank(),  # Remove x-axis
          panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # Remove grid lines

  
  # Combine the plot and the legend
  p_combined <- gridExtra::grid.arrange(p, p_legend, ncol = 2, widths = c(4, 1))

  ggsave(file_name, p_combined, width = 8, height = 5)
  # Convert the ggplot figure to a plotly figure
  p <- ggplotly(p)
  
  # Print the plot
  print(p)
}



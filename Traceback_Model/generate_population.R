library(ggplot2)
library(plotly)
library(gridExtra)
set.seed(123) # Set seed for reproducibility

generate_population <-
  function(population_data, no_of_cells) {
    cells_per_row <- sqrt(no_of_cells)

    # generate a sequence of coordinates for centroids and generate all combinations of these coordinates
    centroid_coords <-
      seq(0.05, by = 0.1, length.out = cells_per_row)
    df_population <-
      expand.grid(x_centroid = centroid_coords, y_centroid = centroid_coords)

    total_population <- population_data$total_population

    population_type <- population_data$population_type

    if (population_type == "radial_clusters" ||
      population_type == "main_and_small_clusters" ||
      population_type == "linear") {
      # used for all radial type populations
      desired_gradient <- population_data$desired_gradient
      # high values mean a large spreading
    }

    if (population_type == "radial_clusters" ||
      population_type == "main_and_small_clusters") {
      num_clusters <- population_data$num_clusters
    }

    # generate population
    df_population <- switch(population_type,
      "random" = generate_random_population(df_population, total_population),
      "uniform" = generate_uniform_population(df_population, total_population),
      "linear" = generate_linear_population(df_population, total_population, desired_gradient),
      "radial_clusters" = generate_radial_clusters_population(
        df_population,
        total_population,
        desired_gradient,
        num_clusters,
        no_of_cells
      ),
      "main_and_small_clusters" = generate_main_and_small_clusters_population(
        df_population,
        total_population,
        desired_gradient,
        num_clusters,
        no_of_cells
      )
    )

    return(df_population)
  }


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
generate_radial_clusters_population <- function(df_population, total_population, desired_gradient, num_clusters, no_of_cells) {
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
generate_main_and_small_clusters_population <- function(df_population, total_population, desired_gradient, num_clusters, no_of_cells) {
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
plot_and_save_population <- function(df_population, investigation_scenario) {
  if (!dir.exists("Results/Population")) {
    dir.create("Results/Population", recursive = TRUE)
  }
  file_name <- paste0(sprintf("Results/Population/Population_Scenario_%s", investigation_scenario), ".png")

  # Create the main plot
  p <- ggplot(df_population, aes(x = x_centroid, y = y_centroid, fill = population)) +
    geom_tile(width = 0.1, height = 0.1, alpha = 0.8) +
    scale_fill_gradient(low = "white", high = "cadetblue", guide = "none") + # Disable the default legend
    scale_x_continuous(breaks = seq(0, 1, by = 0.1)) + # Set x-axis breaks
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    labs(title = sprintf("Visualization of Scenario %s", investigation_scenario), x = "X Coordinate", y = "Y Coordinate") +
    theme_minimal() +
    theme(aspect.ratio = 1, panel.grid.minor = element_blank())

  # Create a custom legend
  legend_df <- data.frame(
    population = seq(min(df_population$population), max(df_population$population), length.out = 100)
  )
  p_legend <- ggplot(legend_df, aes(x = 1, y = population, fill = population)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "cadetblue") +
    scale_y_continuous(breaks = floor(seq(min(df_population$population), max(df_population$population), length.out = 5))) + # Add labels to the legend
    theme_minimal() +
    labs(title = "", x = "", y = "") +
    theme(
      legend.position = "none", axis.text.x = element_blank(), axis.ticks.x = element_blank(), # Remove x-axis
      panel.grid.major = element_blank(), panel.grid.minor = element_blank()
    ) # Remove grid lines

  # Combine the plot and the legend
  p_combined <- grid.arrange(p, p_legend, ncol = 2, widths = c(4, 1))

  ggsave(file_name, p_combined, width = 8, height = 5)
  plot(p_combined)
}

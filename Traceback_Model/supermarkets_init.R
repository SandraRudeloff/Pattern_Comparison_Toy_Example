library(readxl)
source("generate_supermarkets.R")
set.seed(12)
# Input ----
scenarios <- c(1,2,3,4,5,6,7,8,9,10,11,12, 13, 14, 15)
no_of_cells <- 100

for (investigation_scenario in scenarios) {
  # Read Data ----
  total_population <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Population"), scenario == investigation_scenario)$total_population
  population_type <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Population"), scenario == investigation_scenario)$population_type
  
  if (population_type == "radial_clusters" || population_type ==  "main_and_small_clusters" || population_type == "linear") {
    desired_gradient <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Population"), scenario == investigation_scenario)$desired_gradient
    # high values mean a large spreading # used for all radial type populations
  }
  
  if (population_type == "radial_clusters" || population_type ==  "main_and_small_clusters" ) {
    num_clusters <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Population"), scenario == investigation_scenario)$num_clusters
  }
  
  # calculate number of cells per row
  cells_per_row <- sqrt(no_of_cells)
  
  # generate a sequence of coordinates for centroids and generate all combinations of these coordinates
  centroid_coords <- seq(50, by = 100, length.out = cells_per_row)
  df_population <- expand.grid(x_centroid = centroid_coords, y_centroid = centroid_coords)
  
  # generate population
  df_population <- switch(population_type,
                          "random" = generate_random_population(df_population, total_population),
                          "uniform" = generate_uniform_population(df_population, total_population),
                          "linear" = generate_linear_population(df_population, total_population, desired_gradient),
                          "radial_clusters" = generate_radial_clusters_population(df_population, total_population, desired_gradient, num_clusters),
                          "main_and_small_clusters" = generate_main_and_small_clusters_population(df_population, total_population, desired_gradient, num_clusters)
  )
  plot_population(df_population,investigation_scenario)
}
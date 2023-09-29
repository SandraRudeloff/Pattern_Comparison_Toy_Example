library(readxl)
source("generate_population.R")
set.seed(333)
# Input ----
scenarios <- 7:15
no_of_cells <- 100

for (investigation_scenario in scenarios) {
  print("new scenario")
  print(investigation_scenario)
  population_data <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Population"), scenario_id == investigation_scenario)
  df_population <- generate_population(population_data, no_of_cells)
  df_population$cell_id <- as.numeric(row.names(df_population))

  plot_and_save_population(df_population, investigation_scenario)
}

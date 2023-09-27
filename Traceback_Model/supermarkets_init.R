library(readxl)
source("generate_supermarkets.R")
source("generate_population.R")
set.seed(123) # Set seed for reproducibility

# Input ----
scenarios <- c(7)
no_of_cells <- 100


for (investigation_scenario in scenarios) {
  # Population ----
  population_data <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Population"), scenario_id == investigation_scenario)
  df_population <- generate_population(population_data, no_of_cells)
  df_population$cell_id <- as.numeric(row.names(df_population))
  plot_and_save_population(df_population,7 )
  
  # Read Data ----
  chain_details <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Chain_Details"), scenario_id == investigation_scenario)
  df_shops <- data.frame()

  # loop through each chain and generate shops
  for (current_chain in unique(chain_details$chain_id)) {
    chain_data <- chain_details %>%
      filter(chain_id == current_chain)
    
    df_shops_current <- generate_shops(no_of_cells, chain_data, df_population)
    df_shops_current$chain <- current_chain
    
    df_shops <- rbind(df_shops, df_shops_current)
  }
  rownames(df_shops) <- NULL
}

print(df_shops)

field <- expand.grid(x = seq(0.05, 1, by = 0.1), y = seq(0.05, 1, by = 0.1))

plot_stores(field, df_shops)


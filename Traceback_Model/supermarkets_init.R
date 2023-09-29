library(readxl)
source("generate_supermarkets.R")
source("generate_population.R")
set.seed(123) # Set seed for reproducibility
options(warn = 2)

# Input ----
scenarios <- 1:17
no_of_cells <- 100
field <- expand.grid(x = seq(0.05, 1, by = 0.1), y = seq(0.05, 1, by = 0.1))

# Population ----
population_data <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Population"), scenario_id == 7)
df_population <- generate_population(population_data, no_of_cells)
df_population$cell_id <- as.numeric(row.names(df_population))
plot_and_save_population(df_population, 7)

for (investigation_scenario in scenarios) {
  chain_details <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Chain_Details"), scenario_id == investigation_scenario)
  df_shops <- data.frame()

  unique_chains <- unique(chain_details$chain_id)
  # loop through each chain and generate shops
  for (current_chain in unique_chains) {
    chain_data <- chain_details %>%
      filter(chain_id == current_chain)

    is_first_chain <- (current_chain == unique_chains[1])

    if (is_first_chain) {
      df_shops_current <- generate_shops(no_of_cells, chain_data, df_population, is_first_chain = TRUE)
    } else {
      df_shops_current <- generate_shops(no_of_cells, chain_data, df_population, is_first_chain = FALSE, df_shops_chain1 = df_shops_first_chain)
    }

    df_shops_current$chain <- current_chain
    df_shops <- rbind(df_shops, df_shops_current)

    if (is_first_chain) {
      df_shops_first_chain <- df_shops_current
    }
  }
  plot_stores(field, df_shops, investigation_scenario)
}

library(readxl)
source("generate_supermarkets.R")
set.seed(12)
# Input ----
scenarios <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
no_of_cells <- 100

for (investigation_scenario in scenarios) {
  # Read Data ----
  chain_details <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Chain_Details"), scenario_id == investigation_scenario)
  df_shops <- data.frame()

  # Loop through each chain in the specific scenario to create the final dataframe
  for (current_chain_id in unique(chain_details$chain_id)) {
    chain_data <- chain_details %>%
      filter(chain_id == current_chain_id)

    df_shops_current <- generate_shops(no_of_cells, chain_data)
    df_shops_current$chain <- current_chain_id

    df_shops <- rbind(df_shops, df_shops_current)
  }
}

print(df_shops)

field <- expand.grid(x = seq(0.05, 1, by = 0.1), y = seq(0.05, 1, by = 0.1))
# Generate stores
num_stores <- 10
stores <- generate_uniform_shops(field, num_stores)
plot_stores(field, stores)
print(stores)
print(served_cells(field, stores))

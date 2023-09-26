source("trace_outbreak.R")

investigation_scenarios <- c(1)
no_of_cells <- 100
delta <- 0.05 # Define half the side length of a square

results_df <- data.frame(
  scenario_id = character(),
  outbreak_id = character(),
  chain_id = character(),
  GLRT_statistic = numeric(),
  p_value = numeric(),
  decision = character(),
  alpha = numeric(),
  beta = numeric(),
  likelihood_value = numeric(),
  stringsAsFactors = FALSE # tells R not to convert character vectors to factors when creating a data frame
)

for (scenario in investigation_scenarios) {
  print(scenario)
  current_results <- analyze_scenario(scenario, no_of_cells, delta, results_df)
  results_df <- rbind(results_df, current_results)
}

write.csv(results_df, "Data/results.csv", row.names = FALSE)

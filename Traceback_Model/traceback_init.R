library(writexl)
source("trace_outbreak.R")
options(warn = 2)
options(digits = 4)

investigation_scenarios <- 15:16
no_of_cells <- 100
delta <- 0.05 # half the side length of one square

traceback_results_df <- data.frame(
  scenario_id = character(),
  outbreak_id = character(),
  traced_chain = character(),
  GLRT_statistic = numeric(),
  p_value = numeric(),
  decision = character(),
  alpha = numeric(),
  beta = numeric(),
  likelihood_alternative = numeric(),
  beta_best = numeric(),
  tolerance_best = numeric(),
  stringsAsFactors = FALSE # tells R not to convert character vectors to factors when creating a data frame
)

flow_results_df <- data.frame(
  scenario_id = character(),
  beta_best = numeric(),
  tolerance_best = numeric(),
  stringsAsFactors = FALSE
)

for (scenario in investigation_scenarios) {
  print(scenario)
  current_results <- analyze_scenario(scenario, no_of_cells, delta)
  traceback_results_df <- rbind(traceback_results_df, current_results$traceback_results)
  flow_results_df <- rbind(flow_results_df, current_results$flow_results)
}

results_list <- list("traceback results" = traceback_results_df, "flow results" = flow_results_df)
write_xlsx(results_list, "Data/Results/results.xlsx")

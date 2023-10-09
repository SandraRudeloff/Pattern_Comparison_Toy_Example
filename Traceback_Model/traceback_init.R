library(writexl)
source("trace_outbreak.R")
options(warn = 2)
options(digits = 15)

path_to_write_results = "Results/"
investigation_scenarios <-163:320

no_of_cells <- 100
half_side_length <- 0.05 # half the side length of one square cell

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
  current_results <- analyze_scenario(scenario, no_of_cells, half_side_length)
  traceback_results_df <- rbind(traceback_results_df, current_results$traceback_results)
  flow_results_df <- rbind(flow_results_df, current_results$flow_results)
}

results_list <- list("traceback results" = traceback_results_df, "flow results" = flow_results_df)
scenarios_str <- paste(investigation_scenarios[1], investigation_scenarios[length(investigation_scenarios)], sep = "_")
write_xlsx(results_list, paste0(path_to_write_results, "results_", scenarios_str, ".xlsx"))

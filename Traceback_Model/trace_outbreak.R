library(reticulate) # connection to python scripts
library(readxl) # Read Scenario Definition from Excel
library(DEoptim) # optimizer
library(RColorBrewer) # visualization with color set
library(ggplot2)
library(grid)
library(ggnewscale)

source("generate_population.R")
source("generate_supermarkets.R")
source_python("../Diffusion Model/gravity_model.py")
source_python("../Diffusion Model/outbreak_generation.py")
set.seed(123)

# Helper functions ----
# function to check if a point is within a square centered at (x, y) with side length 2*delta
point_in_square <- function(px, py, x, y, delta) {
  return(px >= (x - delta) & px <= (x + delta) & py >= (y - delta) & py <= (y + delta))
}

# Compute distance between two points
compute_distance <- function(x1, y1, x2, y2) {
  return(sqrt((x1 - x2)^2 + (y1 - y2)^2))
}

get_y <- function(df_population, df_outbreak, delta) {
  y <- numeric(nrow(df_population))
  for (i in 1:nrow(df_population)) {
    y[i] <- sum(point_in_square(df_outbreak$x_centroid, df_outbreak$y_centroid, df_population$x_centroid[i], df_population$y_centroid[i], delta))
  }
  return(y)
}

get_N <- function(df_population) {
  return(df_population$population)
}
get_population <- function(investigation_scenario, no_of_cells) {
  population_data <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Population"), scenario_id == investigation_scenario)
  df_population <- generate_population(population_data, no_of_cells)
  df_population$cell_id <- as.numeric(row.names(df_population))
  return(df_population)
}

get_shops <- function(investigation_scenario, no_of_cells, df_population) {
  chain_details <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Chain_Details"), scenario_id == investigation_scenario)
  df_shops <- data.frame()

  # loop through each chain and generate shops
  for (current_chain in unique(chain_details$chain_id)) {
    chain_data <- chain_details %>%
      filter(chain_id == current_chain)

    df_shops_current <- generate_shops(no_of_cells, chain_data)
    df_shops_current$chain <- current_chain

    df_shops <- rbind(df_shops, df_shops_current)
  }

  # Assign cell_id to the stores
  df_shops <- df_shops %>%
    rowwise() %>%
    mutate(
      cell_id = which(
        (df_population$x_centroid - 0.05) <= x &
          (df_population$x_centroid + 0.05) >= x &
          (df_population$y_centroid - 0.05) <= y &
          (df_population$y_centroid + 0.05) >= y
      )
    ) %>%
    ungroup()

  return(df_shops)
}

# Outbreak Generation ----
calculate_flow_of_goods <- function(outbreak_data, df_population_py, df_shops_py) {
  empirical_mean_shopping_distance <- outbreak_data$empirical_mean_shopping_distance
  tolerance <- outbreak_data$tolerance
  list_hyman_results <- hyman_model(empirical_mean_shopping_distance, tolerance, df_population_py, df_shops_py)
  return(list_hyman_results)
}

generate_outbreaks_for_chain <- function(chain, list_outbreak_scenario_sizes, no_of_trials_per_scenario, flow, df_shops_py, df_population_py) {
  outbreak_data_list <- list()
  for (no_of_outbreak_cases in list_outbreak_scenario_sizes) {
    for (trial in seq_len(no_of_trials_per_scenario)) {
      outbreak <- generate_outbreak(chain, no_of_outbreak_cases, flow, df_shops_py, df_population_py)
      identifier <- paste(chain, no_of_outbreak_cases, trial, sep = "_")
      outbreak_data_list[[identifier]] <- outbreak
    }
  }
  return(outbreak_data_list)
}

get_outbreaks <- function(investigation_scenario, df_shops, df_population) {
  df_shops_py <- r_to_py(df_shops)
  df_population_py <- r_to_py(df_population)
  outbreak_data <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Outbreaks"), scenario_id == investigation_scenario)
  if (is.character(outbreak_data$outbreak_scenario_sizes)) {
    list_outbreak_scenario_sizes <- as.integer(unlist(strsplit(outbreak_data$outbreak_scenario_sizes, ",")))
  } else {
    list_outbreak_scenario_sizes <- as.integer(outbreak_data$outbreak_scenario_sizes)
  }
  no_of_trials_per_scenario <- as.integer(outbreak_data$no_of_trials_per_scenario) # Hier könnte man auch noch eine Logik hinterlegen

  ### Calculate Flow of Goods
  list_hyman_results <- calculate_flow_of_goods(outbreak_data, df_population_py, df_shops_py)
  flow <- list_hyman_results[[1]]
  beta_best <- list_hyman_results[[2]]
  tolerance_best <- list_hyman_results[[3]]

  flow_py <- r_to_py(flow)

  ### Generate Outbreaks for Scenario
  outbreak_list <- list()

  for (chain in unique(df_shops$chain)) {
    visualize_flow_for_chain(investigation_scenario, chain, df_shops_py, flow_py)
    chain_outbreaks <- generate_outbreaks_for_chain(chain, list_outbreak_scenario_sizes, no_of_trials_per_scenario, flow, df_shops_py, df_population_py)
    outbreak_list <- c(outbreak_list, chain_outbreaks)
  }
  return(list(outbreak_list = outbreak_list, beta_best = beta_best, tolerance_best = tolerance_best))
}

# Visualizaton ----
visualize_scenario <- function(investigation_scenario, df_shops, df_population, df_outbreak, outbreak_name) {
  # Generate a color palette for the stores of different chains avoiding red and blue
  unique_chains <- unique(df_shops$chain)
  all_colors <- brewer.pal(9, "Set1")
  all_colors <- all_colors[!all_colors %in% c("#E41A1C", "#377EB8")]
  chain_colors <- all_colors[1:length(unique_chains)]
  names(chain_colors) <- unique_chains

  # Create the main plot with population, stores, and outbreak cases
  p_main <- ggplot() +
    # Plot the population data
    geom_tile(
      data = df_population,
      aes(x = x_centroid, y = y_centroid, fill = population),
      width = 0.1,
      height = 0.1,
      alpha = 0.8
    ) +
    scale_fill_gradient(
      low = "white",
      high = "cadetblue",
      guide = "none"
    ) +

    # Introduce a new scale for the shop chains
    new_scale_fill() +

    # Plot the shops data
    geom_point(
      data = df_shops,
      aes(x = x, y = y, fill = chain),
      size = 3,
      shape = 23,
      alpha = 0.8
    ) +
    scale_fill_manual(values = chain_colors, name = "Shop Chain") +

    # Plot the outbreak data
    geom_point(
      data = df_outbreak,
      aes(x = x_centroid, y = y_centroid),
      color = "red",
      size = 2,
      shape = 21,
      fill = "red",
      alpha = 0.8
    ) +

    # Adjust the x and y axis breaks to have lines every 100m
    scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +

    # Add labels and theme
    labs(
      title = sprintf("Visualization of Scenario: %s, Outbreak: %s", investigation_scenario, outbreak_name),
      x = "X Coordinate",
      y = "Y Coordinate",
      color = "Shop Chain"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      aspect.ratio = 1,
      panel.grid.minor = element_blank()
    )

  # Create a custom legend for the population
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

  df_shops$sales <- round(df_shops$sales, 2)

  table_grob_shops <- arrangeGrob(textGrob("Shops", gp = gpar(fontsize = 12, fontface = "bold")), tableGrob(df_shops[, c("chain", "x", "y", "sales", "cell_id")], rows = NULL), nrow = 2, heights = c(0.3, 4.7))
  table_grob_outbreak <- arrangeGrob(textGrob("Outbreak Cases", gp = gpar(fontsize = 12, fontface = "bold")), tableGrob(df_outbreak), nrow = 2, heights = c(0.3, 4.7))

  top_row <- arrangeGrob(p_main, p_legend, ncol = 2, widths = c(4, 0.5))
  bottom_row <- arrangeGrob(table_grob_shops, table_grob_outbreak, ncol = 2, widths = c(2, 2))

  p_combined <- grid.arrange(top_row, bottom_row, nrow = 2, heights = c(5, 2))


  if (!dir.exists(paste0("Data/Results/Scenario_", investigation_scenario))) {
    dir.create(paste0("Data/Results/Scenario_", investigation_scenario))
  }

  plot_filename <- paste0("Data/Results/Scenario_", investigation_scenario, "/Scenario_", investigation_scenario, "_Outbreak_", outbreak_name, ".png")
  ggsave(plot_filename, plot = p_combined, width = 10, height = 8)
}

# Raised Incidence Calculation ----
# Risk function for distance d from the source
risk_function <- function(d, alpha, beta) {
  if (alpha == 0 & beta == 0) {
    return(1)
  } else {
    return(1 + alpha * exp(-(d / beta)^2))
  }
}

## Risk matrix ----
# The risk matrix contains on the rows the cells, 1 being in the left lower corner, and the stores on the columns.
compute_risk_matrix <- function(df_population, df_shops, alpha, beta) {
  risk_matrix <- matrix(0, nrow(df_population), nrow(df_shops))
  for (i in 1:nrow(df_population)) {
    for (j in 1:nrow(df_shops)) {
      d <- compute_distance(df_population$x_centroid[i], df_population$y_centroid[i], df_shops$x[j], df_shops$y[j])
      risk_matrix[i, j] <- risk_function(d, alpha, beta)
    }
  }
  return(risk_matrix)
}

# Likelihood ----
# The expected value mu is the overall prevalance of the disease (rho) times the number of population in that cell times the risk_matrix value at that position
# mu enhtält einen value per cell (the expected number of sick people following the model)

# Erstmal addieren wir alle expected values und die sind dann negativ
likelihood_function_minimize <- function(params, y, N, df_population, df_shops) {
  alpha <- params[1]
  beta <- params[2]
  rho <- sum(y) / sum(N)

  if (alpha < 0 || beta < 0) {
    return(1e6) # since this is out of bound, we set a very high value that will never be picked by a minimization
  }

  risk_matrix <- compute_risk_matrix(df_population, df_shops, alpha, beta)
  mu <- rho * N * apply(risk_matrix, 1, prod)

  log_likelihood <- -sum(mu) + sum(y * log(mu)) # without y! because it does not do anything for the optimization

  if (is.nan(log_likelihood)) {
    return(1e6) # since nan isn't helpful we set a very high value that will never be picked by a minimization
  }
  return(-log_likelihood) # We return the negative likelihood because optimizers in R typically minimize
}

# Standard Errors ----
## Hessian
calculate_std_errors_Hessian <- function(result_optimization_DEoptim, y, N, df_population, df_shops) {
  library(numDeriv)
  hessian_matrix <- hessian(likelihood_function_minimize, c(result_optimization_DEoptim$optim$bestmem[1], result_optimization_DEoptim$optim$bestmem[2]), y = y, N = N, df_population = df_population, df_shops = df_shops)
  inverse_hessian <- solve(hessian_matrix)
  standard_errors <- sqrt(diag(inverse_hessian))
  return(standard_errors)
}

## Monte Carlo
calculate_std_errors_MC <- function(result_optimization_DEoptim, n_simulations, df_population, df_shops) {
  alpha_estimates <- numeric()
  beta_estimates <- numeric()

  # Loop over simulations
  for (i in 1:n_simulations) {
    risk_matrix <- compute_risk_matrix(df_population, df_shops, result_optimization_DEoptim$optim$bestmem[1], result_optimization_DEoptim$optim$bestmem[2])
    rho <- sum(y) / sum(df_population$population)

    mu <- rho * N * apply(risk_matrix, 1, prod)

    # Generate simulated data
    simulated_y <- rpois(length(mu), lambda = mu)

    # Fit the model to the simulated data
    result_simulated <- DEoptim(fn = likelihood_function_minimize, lower = lower_bounds, upper = upper_bounds, y = simulated_y, N = N, df_population = df_population, df_shops = df_shops) # ,  DEoptim.control(itermax = 100))

    # Store the estimated parameters
    alpha_estimates[i] <- result_simulated$optim$bestmem[1]
    beta_estimates[i] <- result_simulated$optim$bestmem[2]
  }

  # Calculate standard errors
  se_alpha <- sd(alpha_estimates)
  se_beta <- sd(beta_estimates)

  standard_errors <- list(se_alpha = se_alpha, se_beta = se_beta)
  return(standard_errors)
}

get_lower_bounds <- function(result_optimization_DEoptim, z_value, std_error) {
  return(result_optimization_DEoptim$optim$bestmem - z_value * std_error)
}

get_upper_bounds <- function(result_optimization_DEoptim, z_value, std_error) {
  result_optimization_DEoptim$optim$bestmem + z_value * std_error
}

# Main traceback function ----
analyze_scenario <- function(investigation_scenario, no_of_cells, delta, traceback_results_df, flow_results_df) {
  # Collect Variables ----
  # All values are measured in km.
  df_population <- get_population(investigation_scenario, no_of_cells)
  N <- get_N(df_population) # number of people at risk in each subregion

  df_shops <- get_shops(investigation_scenario, no_of_cells, df_population)

  list_outbreak_data <- get_outbreaks(investigation_scenario, df_shops, df_population)
  outbreak_list <- list_outbreak_data$outbreak_list
  beta_best <- list_outbreak_data$beta_best
  tolerance_best <- list_outbreak_data$tolerance_best

  new_row_flow_results <- data.frame(
    scenario_id = investigation_scenario,
    beta_best = beta_best,
    tolerance_best = tolerance_best,
    stringsAsFactors = FALSE
  )

  flow_results_df <- rbind(flow_results_df, new_row_flow_results)

  # set boundaries for optimization
  lower_bounds <- c(alpha = 0.001, beta = 0.0001)
  upper_bounds <- c(alpha = 5000, beta = 50)

  for (outbreak_name in names(outbreak_list)) {
    df_outbreak <- outbreak_list[[outbreak_name]]

    visualize_scenario(investigation_scenario, df_shops, df_population, df_outbreak, outbreak_name)
    y <- get_y(df_population, df_outbreak, delta)

    for (chain in unique(df_shops$chain)) {
      chain_shops <- df_shops[df_shops$chain == chain, ]
      logLik_null <- -likelihood_function_minimize(c(0, 0), y = y, N = N, df_population = df_population, df_shops = chain_shops)
      result_alternative_DEoptim <- DEoptim(fn = likelihood_function_minimize, lower = lower_bounds, upper = upper_bounds, y = y, N = N, df_population = df_population, df_shops = chain_shops, control = list(trace = FALSE))
      logLik_alternative_DEoptim <- -result_alternative_DEoptim$optim$bestval

      GLRT_statistic <- 2 * (logLik_alternative_DEoptim - logLik_null) # y! kürzt sich raus

      # Determine the degrees of freedom (difference in number of parameters between the two models)
      df <- 2 # alpha and beta are the additional parameters in the alternative model

      p_value <- 1 - pchisq(GLRT_statistic, df)


      # Decide on the hypothesis based on a significance level (e.g., 0.05)
      if (p_value < 0.05) {
        decision <- "Reject the null hypothesis in favor of the alternative."
      } else {
        decision <- "Fail to reject the null hypothesis."
      }

      new_row_traceback_results <- data.frame(
        scenario_id = investigation_scenario,
        outbreak_id = outbreak_name,
        traced_chain = chain,
        alpha = result_alternative_DEoptim$optim$bestmem[1],
        beta = result_alternative_DEoptim$optim$bestmem[2],
        likelihood_null = logLik_null,
        likelihood_alternative = likelihood_function_minimize(c(result_alternative_DEoptim$optim$bestmem[1], result_alternative_DEoptim$optim$bestmem[2]), y = y, N = N, df_population = df_population, df_shops = chain_shops),
        GLRT_statistic = GLRT_statistic,
        p_value = p_value,
        decision = decision,
        stringsAsFactors = FALSE
      )

      traceback_results_df <- rbind(traceback_results_df, new_row_traceback_results)

      # Std. errors ----
      # z_value <- 1.96 # For a 95% confidence level
      #
      # std_error_Hessian <- calculate_std_errors_Hessian(result_alternative_DEoptim, y = y, N=N, df_population, df_shops)
      # lower_bounds_Hessian <- get_lower_bounds(result_alternative_DEoptim, z_value, std_error_Hessian)
      # upper_bounds_Hessian <- get_upper_bounds(result_alternative_DEoptim, z_value, std_error_Hessian)

      # n_simulations <- 10
      # std_error_MC <-  calculate_std_errors_MC(result_alternative_DEoptim, n_simulations, df_population, df_shops)
      # lower_bounds_MC <- get_lower_bounds(result_alternative_DEoptim, z_value, std_error_MC)
      # upper_bounds_MC <- get_upper_bounds(result_alternative_DEoptim, z_value, std_error_MC)
    }
  }
  return(list(traceback_results = traceback_results_df, flow_results = flow_results_df))
}

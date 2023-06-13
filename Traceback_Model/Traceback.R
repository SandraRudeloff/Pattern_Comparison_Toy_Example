options(reticulate.USE_CACHE = FALSE)
library(spatstat)
library(reticulate)
library(RColorBrewer)
pd <- import("pandas")
library(readxl)
library(openxlsx)

# Definition of input Data ----
scenarios <- c(1,2,3,4)
chains_to_investigate <- list("Chain 1")
set.seed(123)
for (investigation_scenario in scenarios) {
  # Read Data ----
  window <- owin(c(0, 1000), c(0, 1000))
  
  ## Outbreak Data ----
  ### Outbreak made with Diffusion Model ----
  # outbreak_data <- pd$read_pickle(paste("C:\\Users\\srude\\Documents\\Dev Kram\\Toy Example Traceback Model\\Diffusion Model\\Outputs\\Outbreaks\\Scenario_", as.character(scenario), "\\Outbreak_Chain 1_10_0.pkl", sep = ""))
  # outbreak_data <- pd$read_pickle("./Data/Outbreaks/Toy_Outbreak_10.pkl")
  # ppp_outbreak <- ppp(x=outbreak_data$x_centroid, y=outbreak_data$y_centroid, window = window)
  
  ### Outbreak artificially made to test model ----
  df_outbreak <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Outbreak_Locations"), scenario == investigation_scenario)
  ppp_outbreak <- ppp(x = df_outbreak$case_x, y = df_outbreak$case_y, window = window)
  ppp_outbreak <- rescale(ppp_outbreak, 1000, "km")
  
  
  ## Population Data ----
  
  no_of_cells <- 100
  
  # calculate number of cells per row
  cells_per_row <- sqrt(no_of_cells)
  
  # generate a sequence of coordinates for centroids
  centroid_coords <- seq(50, by = 100, length.out = cells_per_row)
  
  # generate all combinations of these coordinates
  df_population <- expand.grid(x_centroid = centroid_coords, y_centroid = centroid_coords)
  
  population_type <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Population"), scenario == investigation_scenario)$population_type
  
  # assign population
  if (population_type == "uniform") {
    population_per_cell <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Population"), scenario == investigation_scenario)$population_per_cell
    df_population$population <- rep(population_per_cell, nrow(df_population))
  } else if (population_type == "random") {
    # random distribution between 1 and 100
    df_population$population <- sample(1:100, nrow(df_population), replace = TRUE)
  }
  
  # kernel smooth population
  ppp_population <- ppp(x = df_population$x_centroid, y = df_population$y_centroid, window = window, marks = df_population$population)
  ppp_population <- rescale(ppp_population, 1000, "km")
  smo_population <- density(ppp_population, eps = 0.1, positive = TRUE, weights = marks(ppp_population))
  
  im_population <- smo_population
  im_population <- eval.im(im_population / 100)
  im_population <- eval.im(pmax(im_population, 1e-10))
  
  ## Shops Data ----
  ### Private Computer ----
  ## shops <- pd$read_pickle(paste("C:\\Users\\srude\\Documents\\Pattern Comparison Project\\Toy_Example\\Diffusion_Model\\Outputs\\Stores\\stores_", as.character(scenario), ".pkl", sep = ""))
  
  
  ### Work Computer ----
  ## shops <- pd$read_pickle(paste("C:\\Users\\Sandra.Rudeloff\\Documents\\Pattern Comparison Project\\Toy_Example\\Diffusion_Model\\Outputs\\Stores\\stores_", as.character(scenario), ".pkl", sep = ""))
  
  
  ## Shops Data
  df_shops <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Store_Locations"), scenario == investigation_scenario)
  
  ppp_shops <- ppp(x = df_shops$store_x, y = df_shops$store_y, window = window, marks = as.factor(df_shops$chain))
  ppp_shops <- rescale(ppp_shops, 1000, "km")
  
  # Quadrature Scheme ----
  Q <- quadscheme(ppp_outbreak, eps = 0.1)
  
  
  # Null Model ----
  fit0 <- ppm(Q ~ offset(log(im_population)))
  print(fit0)
  
  
  # Alternative Model ----
  create_raisin_func <- function(x, y) {
    # Construct raisin expressions
    ls_all_raisins <- mapply(function(xi, yi) {
      paste0("log((1 +  abs(alpha) * (exp(-(abs(beta)) * ((x- ", xi, ")^2 + (y- ", yi, ")^2)))))")
    }, x, y, SIMPLIFY = FALSE)
  
    # Collapse into single string
    str_all_raisins <- paste(ls_all_raisins, collapse = "+")
  
    # Create raisin function
    eval(parse(text = paste("raisin_func <- function(x, y, alpha, beta) {(", str_all_raisins, ")}", sep = "")))
  
    return(raisin_func)
  }
  
  
  fit_model <- function(Q, im_population, fit0, chain, start_alpha, start_beta) {
    fit1 <- ippm(Q ~ offset(log(im_population) + raisin_func),
      start = list(alpha = start_alpha, beta = start_beta), nlm.args = list(stepmax = 1), gcontrol = glm.control(maxit = 1000)
    )
    return(fit1)
  }
  
  
  calculate_anova <- function(fit0, fit1) {
    return(anova(fit0, fit1, test = "LRT"))
  }
  
  
  plot_example <- function(ppp_chosen) {
    # Get min and max of the image
    min_val <- min(im_population)
    max_val <- max(im_population)
  
    tolerance <- 1e-6 # Adjust the tolerance as needed
  
    # Check if the density is uniform
    is_uniform <- abs(min_val - max_val) < tolerance
  
    # Define the number of breaks
    n_breaks <- 8
  
    if (is_uniform) {
      # If density is uniform, use a single color
      X <- layered(im_population, unmark(subset(ppp_shops, marks != chain, drop = TRUE)), ppp_chosen, ppp_outbreak)
      layerplotargs(X)[[1]] <- list(col = "grey")
    } else {
      # Create a sequence of breaks from min to max
      # If density is variable, use a color palette
      color_palette <- brewer.pal(n = n_breaks, name = "Blues")
      breaks <- seq(from = min_val, to = max_val, length.out = n_breaks + 1)
  
      X <- layered(im_population, unmark(subset(ppp_shops, marks != chain, drop = TRUE)), ppp_chosen, ppp_outbreak)
      layerplotargs(X)[[1]] <- list(col = color_palette, breaks = breaks)
    }
  
    layerplotargs(X)[[2]] <- list(pch = 18, cex = 0.8, col = "#386f9c")
    layerplotargs(X)[[3]] <- list(pch = 18, cex = 1.5, col = "gold")
    layerplotargs(X)[[4]] <- list(pch = 20, col = "red2", cex = 1.5)
  
    return(X)
  }
  
  save_results_in_XLSX <- function(chains_to_investigate, start_alpha_values, start_beta_values, fit_results, scenario_folder) {
    all_results <- data.frame()
    for (chain in chains_to_investigate) {
      for (start_beta in start_beta_values) {
        for (start_alpha in start_alpha_values) {
          resulting_alpha <- fit_results[[chain]][[sprintf("start_beta_%s", start_beta)]][[paste0("start_alpha_", start_alpha)]]$fit1$covfunargs$alpha
          resulting_beta <- fit_results[[chain]][[sprintf("start_beta_%s", start_beta)]][[paste0("start_alpha_", start_alpha)]]$fit1$covfunargs$beta
          pr_chi <- fit_results[[chain]][[sprintf("start_beta_%s", start_beta)]][[paste0("start_alpha_", start_alpha)]]$anova_result$`Pr(>Chi)`[2]
          deviance <- fit_results[[chain]][[sprintf("start_beta_%s", start_beta)]][[paste0("start_alpha_", start_alpha)]]$anova_result$Deviance[2]
          
          # Create a data frame
          df <- data.frame(
            Chain = chain,
            Start_Beta = start_beta,
            Start_Alpha = start_alpha,
            Result_Alpha = format(resulting_alpha, scientific = FALSE),
            Result_Beta = format(resulting_beta, scientific = FALSE),
            Pr_Chi = pr_chi,
            Deviance = deviance
          )
          
          # Append the data frame to the all_results data frame
          all_results <- rbind(all_results, df)
        
      }}}
    # Write to an Excel file
    
    filename <- sprintf("%s/results_scenario_%s.xlsx", scenario_folder, investigation_scenario)
    openxlsx::write.xlsx(all_results, filename)
    
  }
  
  # Define a vector of starting values for beta
  start_beta_values <- c(1, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200)
  start_alpha_values <- c(1, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200)
  # Initialize a list to store the results
  fit_results <- list()
  
  for (chain in chains_to_investigate) {
    print(chain)
    ppp_chosen <- unmark(subset(ppp_shops, marks == chain, drop = TRUE))
  
    plot(plot_example(ppp_chosen), main = sprintf("Potential sources and cases for scenario %s", investigation_scenario), axes = TRUE, xlim = c(0, 1), ylim = c(0, 1))
  
    raisin_func <- create_raisin_func(ppp_chosen$x, ppp_chosen$y)
    
    # Initialize a list to store the results for this chain
    chain_results <- list()
    
    for (start_beta in start_beta_values) {
      chain_results[[paste0("start_beta_", start_beta)]] <- list()
      for (start_alpha in start_alpha_values) {
        fit1 <- fit_model(Q, im_population, fit0, chain, start_alpha, start_beta )
        print(sprintf("Alternative Model for %s using start_alpha: %s and start_beta: %s", chain, start_alpha, start_beta))
        print(fit1)
        anova_result <- calculate_anova(fit0, fit1)
        print(sprintf("Anova for %s using start_alpha: %s and start_beta: %s", chain, start_alpha, start_beta))
        print(anova_result)
        
        # Save the results to the list
        chain_results[[paste0("start_beta_", start_beta)]][[paste0("start_alpha_", start_alpha)]] <- list(fit1 = fit1, anova_result = anova_result)
  
      }
  
    }
    # Store the results for this chain in the overall results list
    fit_results[[chain]] <- chain_results
    
  }
  
  # Save results
  scenario_folder <- sprintf("Results/Scenario_%s", investigation_scenario)
  if (!dir.exists(scenario_folder)) {
    dir.create(scenario_folder, recursive = TRUE)
  }
  
  save_results_in_XLSX(chains_to_investigate, start_alpha_values, start_beta_values, fit_results, scenario_folder)
  
  filename <- sprintf("%s/results_scenario_%s.rds", scenario_folder, investigation_scenario)
  saveRDS(fit_results, filename)
  gc()
}
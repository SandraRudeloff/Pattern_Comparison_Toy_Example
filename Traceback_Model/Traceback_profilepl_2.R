options(reticulate.USE_CACHE = FALSE)
library(spatstat)
library(reticulate)
library(RColorBrewer)
pd <- import("pandas")
library(readxl)
library(openxlsx)
source("generate_population.R")

# Definition of input Data ----
scenarios <- c(1)

# Define a vector of starting values for the parameters and put the into a dataframe as input for profilepl
start_alpha_values <- c(1, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200)
start_beta_values <-  c(1, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200)

s <- expand.grid(alpha = start_alpha_values, beta = start_beta_values)


scenarios_data_population <- read_excel("./Data/scenarios.xlsx", sheet = "Population")
set.seed(123)

read_population_data <- function(investigation_scenario,no_of_cells ) {  
  cells_per_row <- sqrt(no_of_cells)
  
  # generate a sequence of coordinates for centroids and generate all combinations of these coordinates
  centroid_coords <- seq(50, by = 100, length.out = cells_per_row)
  df_population <- expand.grid(x_centroid = centroid_coords, y_centroid = centroid_coords)
  
  population_type <- subset(scenarios_data_population, scenario == investigation_scenario)$population_type
  total_population <- subset(scenarios_data_population, scenario == investigation_scenario)$total_population
  
  if (population_type == "radial_clusters" || population_type ==  "main_and_small_clusters" || population_type == "linear") {
    # used for all radial type populations
    desired_gradient <- subset(scenarios_data_population, scenario == investigation_scenario)$desired_gradient
    # high values mean a large spreading 
  }
  
  if (population_type == "radial_clusters" || population_type ==  "main_and_small_clusters" ) {
    num_clusters <- subset(scenarios_data_population, scenario == investigation_scenario)$num_clusters
  }
  
  # generate population
  df_population <- switch(population_type,
                          "random" = generate_random_population(df_population, total_population),
                          "uniform" = generate_uniform_population(df_population, total_population),
                          "linear" = generate_linear_population(df_population, total_population, desired_gradient),
                          "radial_clusters" = generate_radial_clusters_population(df_population, total_population, desired_gradient, num_clusters),
                          "main_and_small_clusters" = generate_main_and_small_clusters_population(df_population, total_population, desired_gradient, num_clusters)
  )
  
  # kernel smooth population
  ppp_population <- ppp(x = df_population$x_centroid, y = df_population$y_centroid, window = window, marks = df_population$population)
  ppp_population <- rescale(ppp_population, 1000, "km")
  smo_population <- density(ppp_population, eps = 0.1, positive = TRUE, weights = marks(ppp_population))
  
  im_population <- smo_population
  im_population <- eval.im(im_population / 100)
  im_population <- eval.im(pmax(im_population, 1e-10))
  
  return(im_population)
}

for (investigation_scenario in scenarios) {
  no_of_cells <- 100
  window <- owin(c(0, 1000), c(0, 1000))
  
  ### Population Data ----
  im_population <- read_population_data(investigation_scenario, no_of_cells)
  
  ## Outbreak Data ----
  ### Outbreak artificially made to test model ----
  df_outbreak <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Outbreak_Locations"), scenario == investigation_scenario)
  ppp_outbreak <- ppp(x = df_outbreak$case_x, y = df_outbreak$case_y, window = window)
  ppp_outbreak <- rescale(ppp_outbreak, 1000, "km")
  
  ## Shops Data
  df_shops <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Store_Locations"), scenario == investigation_scenario)
  chains_to_investigate <- as.list(unique(df_shops$chain))
  
  ppp_shops <- ppp(x = df_shops$store_x, y = df_shops$store_y, window = window, marks = as.factor(df_shops$chain))
  ppp_shops <- rescale(ppp_shops, 1000, "km")
  
  # Quadrature Scheme ----
  Q <- quadscheme(ppp_outbreak, eps = 0.1)
  
  # Null Model ----
  fit0 <- ppm(Q ~ offset(log(im_population)))
  print(fit0)
  
  
  # Alternative Model ----
  # create_raisin_func <- function(x, y) {
  #   # Construct raisin expressions
  #   ls_all_raisins <- mapply(function(xi, yi) {
  #     paste0("log((1 +  abs(alpha) * (exp(-(abs(beta)) * ((x- ", xi, ")^2 + (y- ", yi, ")^2)))))")
  #   }, x, y, SIMPLIFY = FALSE)
  #   
  #   # Collapse into single string
  #   str_all_raisins <- paste(ls_all_raisins, collapse = "+")
  #   
  #   # Create raisin function
  #   eval(parse(text = paste("raisin_func <- function(x, y, alpha, beta) {(", str_all_raisins, ")}", sep = "")))
  #   
  #   return(raisin_func)
  # }
  
  create_raisin_func <- function(x_coords, y_coords) {
    # Construct raisin expressions
    ls_all_raisins <- mapply(function(xi, yi) {
      paste0("log((1 +  abs(alpha) * (exp(-(abs(beta)) * ((x- ", xi, ")^2 + (y- ", yi, ")^2)))))")
    }, x_coords, y_coords, SIMPLIFY = FALSE)
    
    # Collapse into single string
    str_all_raisins <- paste(ls_all_raisins, collapse = "+")
    
    # Create raisin function
    eval(parse(text = paste("raisin_func <- function(alpha, beta) {(", str_all_raisins, ")}", sep = "")))
    
    return(raisin_func)
  }
  
  
  
  
  fit_alternative_model <- function(Q, im_population, fit0, chain) {
    # fit1 <- ippm(Q ~ offset(log(im_population) + raisin_func),
    #              start = list(alpha = start_alpha, beta = start_beta), nlm.args = list(stepmax = 1), gcontrol = glm.control(maxit = 1000)
    # )
    fit1 <- profilepl(s, raisin_func, Q ~ offset(log(im_population)), aic = FALSE, rbord = NULL, verbose = TRUE, fast=TRUE)
    return(fit1)
  }
  
  calculate_anova <- function(fit0, fit1) {
    return(anova(fit0, fit1, test = "LRT"))
  }
  
  plot_example <- function(ppp_chosen) {
    X <- layered(im_population, unmark(subset(ppp_shops, marks != chain, drop = TRUE)), ppp_chosen, ppp_outbreak)
    
    # Get min and max of the image
    min_val <- min(im_population)
    max_val <- max(im_population)
    
    # Check if the density is uniform
    is_uniform <- abs(min_val - max_val) < 1e-6 # Adjust the tolerance as needed
    
    if (is_uniform) {
      # If density is uniform, use a single color
      layerplotargs(X)[[1]] <- list(col = "grey")
    } else {
      # Define the number of breaks
      n_breaks <- 8
      
      # Create a sequence of breaks from min to max
      # If density is variable, use a color palette
      color_palette <- brewer.pal(n = n_breaks, name = "Blues")
      breaks <- seq(from = min_val, to = max_val, length.out = n_breaks + 1)
      
      layerplotargs(X)[[1]] <- list(col = color_palette, breaks = breaks)
    }
    
    layerplotargs(X)[[2]] <- list(pch = 18, cex = 1.0, col = "#44a832")
    layerplotargs(X)[[3]] <- list(pch = 18, cex = 1.5, col = "gold")
    layerplotargs(X)[[4]] <- list(pch = 20, col = "red2", cex = 1.5)
    
    return(X)
  }
  
  for (chain in chains_to_investigate) {
    print(chain)
    ppp_chosen <- unmark(subset(ppp_shops, marks == chain, drop = TRUE))
    
    plot(plot_example(ppp_chosen), main = sprintf("Potential sources and cases for scenario %s", investigation_scenario), axes = TRUE, xlim = c(0, 1), ylim = c(0, 1))
    
    raisin_func <- create_raisin_func(ppp_chosen$x, ppp_chosen$y)

    fit1 <- fit_alternative_model(Q, im_population, fit0, chain)
    print(fit1)
    summary(fit1)
    
    anova_result <- calculate_anova(fit0, fit1)
    print("Anova")
    print(anova_result)

  }
  
}
options(reticulate.USE_CACHE = FALSE)
library(spatstat) 
library(reticulate) 
library(RColorBrewer) 
pd <- import("pandas")
library(readxl)

# Definition of input Data
chains_to_investigate <- list("Chain 1" )
investigation_scenario <- 4


set.seed(123)
# Read Data
window <- owin(c(0,1000), c(0,1000))

## Outbreak made with Diffusion Model:
# outbreak_data <- pd$read_pickle(paste("C:\\Users\\srude\\Documents\\Dev Kram\\Toy Example Traceback Model\\Diffusion Model\\Outputs\\Outbreaks\\Scenario_", as.character(scenario), "\\Outbreak_Chain 1_10_0.pkl", sep = ""))
# outbreak_data <- pd$read_pickle("./Data/Outbreaks/Toy_Outbreak_10.pkl") 
# ppp_outbreak <- ppp(x=outbreak_data$x_centroid, y=outbreak_data$y_centroid, window = window)

# Outbreak artificially made to test model:
df_outbreak <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Outbreak_Locations"), scenario == investigation_scenario)
ppp_outbreak <- ppp(x = df_outbreak$case_x, y = df_outbreak$case_y, window = window ) 
ppp_outbreak <- rescale(ppp_outbreak, 1000, "km")


## Population Data
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

ppp_population <- ppp(x = df_population$x_centroid, y = df_population$y_centroid, window = window, marks = df_population$population)
ppp_population <- rescale(ppp_population, 1000, "km")
smo_population <- density(ppp_population, eps = 0.1, positive = TRUE, weights = marks(ppp_population)) 

im_population <- smo_population
im_population <- eval.im(im_population / 100)
im_population <- eval.im(pmax(im_population, 1e-10))

plot(im_population)



# assign population
df_population$population <- population_per_cell

ppp_population <- ppp(x = df_population$x_centroid, y = df_population$y_centroid, window = window, marks = df_population$population)
ppp_population <- rescale(ppp_population, 1000, "km")
smo_population <- density(ppp_population, eps = 0.1, positive = TRUE, weights = marks(ppp_population)) 

im_population <- smo_population
im_population <- eval.im(im_population / 100)
im_population <- eval.im(pmax(im_population, 1e-10))

plot(im_population)






## Private Computer
## shops <- pd$read_pickle(paste("C:\\Users\\srude\\Documents\\Pattern Comparison Project\\Toy_Example\\Diffusion_Model\\Outputs\\Stores\\stores_", as.character(scenario), ".pkl", sep = ""))


## Work Computer
## shops <- pd$read_pickle(paste("C:\\Users\\Sandra.Rudeloff\\Documents\\Pattern Comparison Project\\Toy_Example\\Diffusion_Model\\Outputs\\Stores\\stores_", as.character(scenario), ".pkl", sep = ""))


## Shops Data
df_shops <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Store_Locations"), scenario == investigation_scenario)


## --------------------------------------------------------------------------
ppp_shops <- ppp(x = df_shops$store_x, y = df_shops$store_y, window = window, marks = as.factor(df_shops$chain))
ppp_shops <- rescale(ppp_shops, 1000, "km")


## Quadrature Scheme
Q <- quadscheme(ppp_outbreak, eps = 0.1)


## Null Model
fit0 <- ppm(Q ~ offset(log(im_population)))
print(fit0)


## Alternative Model
create_raisin_func <- function(x, y) {
  # Construct raisin expressions
  ls_all_raisins <- mapply(function(xi, yi) {
    paste0("log((1 +  abs(alpha) * (exp(-(abs(beta)) * ((x- ", xi, ")^2 + (y- ", yi, ")^2)))))")
  }, x, y, SIMPLIFY = FALSE)

  # Collapse into single string
  str_all_raisins <- paste(ls_all_raisins, collapse = "+")

  # Create raisin function
  eval(parse(text = paste('raisin_func <- function(x, y, alpha, beta) {(' , str_all_raisins , ')}', sep = '')))

  return(raisin_func)
}


fit_model <- function(Q, im_population, fit0, chain) {
  fit1 <- ippm(Q ~ offset(log(im_population) + raisin_func),
               start = list(alpha = 5, beta = 15), nlm.args = list(stepmax = 1), gcontrol = glm.control(maxit = 1000))
  print(sprintf("Alternative Model for %s", chain))
  print(fit1)
  return(fit1)
}


calculate_anova <- function(fit0, fit1, chain) {
  print(sprintf("Anova for %s", chain))
  anova_result <- (anova(fit0, fit1, test = "LRT"))
  print(anova_result)
  return(anova_result)
}


plot_example <- function(ppp_chosen) {
  # Get min and max of the image
  min_val <- min(im_population)
  max_val <- max(im_population)
  
  tolerance <- 1e-6  # Adjust the tolerance as needed
  
  # Check if the density is uniform
  is_uniform <- abs(min_val - max_val) < tolerance
  
  # Check if the density is uniform
  #is_uniform <- min_val == max_val
  
  # Define the number of breaks you want
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

for (chain in chains_to_investigate) {
  print(chain)
  ppp_chosen <- unmark(subset(ppp_shops, marks == chain, drop = TRUE))
  
  X <- plot_example(ppp_chosen)
  plot(X, main = "Potential sources and cases", axes = TRUE,  xlim = c(0, 1), ylim = c(0, 1))

  raisin_func <- create_raisin_func(ppp_chosen$x, ppp_chosen$y)
  fit1 <- fit_model(Q, im_population, fit0, chain)
  anova_result <- calculate_anova(fit0, fit1, chain)
}

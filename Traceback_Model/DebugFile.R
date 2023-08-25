library(readxl)
library(ggplot2)
library(dplyr)
library(ggnewscale)
library(RColorBrewer)
library(plotly)
source("generate_population.R")

investigation_scenario <- 16
no_of_cells <- 100

read_population_data <- function(investigation_scenario,no_of_cells ) {  
  cells_per_row <- sqrt(no_of_cells)
  
  # generate a sequence of coordinates for centroids and generate all combinations of these coordinates
  centroid_coords <- seq(50, by = 100, length.out = cells_per_row)
  df_population <- expand.grid(x_centroid = centroid_coords, y_centroid = centroid_coords)
  
  population_type <- subset(scenarios_data_population, scenario == investigation_scenario)$population_type
  total_population <- subset(scenarios_data_population, scenario == investigation_scenario)$total_population
  
  # generate population
  df_population <- switch(population_type,
                          "uniform" = generate_uniform_population(df_population, total_population)  )
  
  return(df_population)
}

scenarios_data_population <- read_excel("./Data/scenarios.xlsx", sheet = "Population")

df_population <- read_population_data(investigation_scenario, no_of_cells) 


df_outbreak <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Outbreak_Locations"), scenario == investigation_scenario)[,2:3]
df_shops <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Store_Locations"), scenario == investigation_scenario)[,2:4]

# Risk function for distance d from the source
risk_function <- function(d, alpha, beta) {
  return(1 + alpha * exp(- (d/beta)^2))
}

# Compute distance between two points
compute_distance <- function(x1, y1, x2, y2) {
  return(sqrt((x1 - x2)^2 + (y1 - y2)^2))
}

# Compute the risk matrix for each subregion and source
compute_risk_matrix <- function(df_population, df_shops, alpha, beta) {
  risk_matrix <- matrix(0, nrow(df_population), nrow(df_shops))
  for (i in 1:nrow(df_population)) {
    for (j in 1:nrow(df_shops)) {
      d <- compute_distance(df_population$x_centroid[i], df_population$y_centroid[i], df_shops$store_x[j], df_shops$store_y[j])
      risk_matrix[i, j] <- risk_function(d, alpha, beta)
      print( risk_matrix[i, j] )
    }
  }
  return(risk_matrix)
}

# 3. Compute Expected Number of Cases ----
#For each subregion, compute the expected number of cases (mu) using the risk functions and the population at risk in that subregion.

# 4. Define the Likelihood Function ----
# Using the expected number of cases and the observed number of cases, define the likelihood function.
# Likelihood function
likelihood_function <- function(params, y, N) {
  alpha <- params[1]
  beta <- params[2]
  rho <- params[3]
  print(paste("alpha: ", alpha, "beta: ", beta, "rho: ", rho))
  
  risk_matrix <- compute_risk_matrix(df_population, df_shops, alpha, beta)
  mu <- rho * N * apply(risk_matrix, 1, prod)
  
  L <- sum(-mu + y * log(mu))
  print(paste("likelihood: ", L))
  
  return(-L) # We return the negative likelihood because optimizers in R typically minimize
}

# Construct Y
# Define a function to check if a point is within a square centered at (x, y) with side length 2*delta
point_in_square <- function(px, py, x, y, delta) {
  return(px >= (x - delta) & px <= (x + delta) & py >= (y - delta) & py <= (y + delta))
}

# Define a delta (half the side length of the square)
delta <- 50  # You can adjust this value based on your data's scale

# Observed number of cases in each subregion
y <- numeric(nrow(df_population))
for (i in 1:nrow(df_population)) {
  y[i] <- sum(point_in_square(df_outbreak$case_x, df_outbreak$case_y, df_population$x_centroid[i], df_population$y_centroid[i], delta))
}

# 5. Optimize the Likelihood Function ----
# Use an optimization algorithm to estimate the parameters (alpha, beta, and rho)
# Number of people at risk in each subregion
N <- df_population$population

# Initial parameter values
initial_params <- c(alpha=100, beta=100, rho=1)

# Optimization
# Lower bounds for the parameters
lower_bounds <- c(alpha=0, beta=0, rho=0.00000001)

# Optimization with constraints
result <- optim(par=initial_params, fn=likelihood_function, y=y, N=N, method="L-BFGS-B", lower=lower_bounds, hessian = TRUE)

# Extract estimated parameters
estimated_params <- result$par
print(estimated_params)

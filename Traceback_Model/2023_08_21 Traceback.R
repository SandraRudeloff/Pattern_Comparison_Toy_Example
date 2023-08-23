library(readxl)
source("generate_population.R")

investigation_scenario <- 1
no_of_cells <- 100


# 1. Collect Variables ----
scenarios_data <- read_excel("./Data/scenarios.xlsx")
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
  
  return(df_population)
}

scenarios_data_population <- read_excel("./Data/scenarios.xlsx", sheet = "Population")

### Population Data ----
df_population <- read_population_data(investigation_scenario, no_of_cells) # number of people at risk in each subregion

### Outbreak Data ---- 
df_outbreak <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Outbreak_Locations"), scenario == investigation_scenario)

df_shops <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Store_Locations"), scenario == investigation_scenario)


# 2. Calculate the Risk Function for Each Source and Subregion ---- 
# For each subregion centroid, compute the risk function for each store (source) based on the distance between the subregion centroid and the store.

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
  
  risk_matrix <- compute_risk_matrix(df_population, df_shops, alpha, beta)
  mu <- rho * N * apply(risk_matrix, 1, prod)
  
  L <- sum(-mu + y * log(mu))
  return(-L) # We return the negative likelihood because optimizers in R typically minimize
}

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
initial_params <- c(alpha=1, beta=1, rho=1)

# Optimization
result <- optim(par=initial_params, fn=likelihood_function, y=y, N=N, method="SANN", hessian = TRUE)

# Extract estimated parameters
estimated_params <- result$par
print(estimated_params)


lower_bounds <- c(alpha=0, beta=0, rho=0)
upper_bounds <- c(alpha=1000, beta=1000, rho=10)  # You can adjust these bounds based on your domain knowledge.
result <- DEoptim(fn=likelihood_function, lower=lower_bounds, upper=upper_bounds, y=y, N=N)
estimated_params <- result$optim$bestmem
print(estimated_params)



# Fit the null model
null_likelihood_function <- function(params, y, N) {
  rho <- params[1]
  mu <- rho * N
  
  L <- sum(-mu + y * log(mu))
  return(-L)
}

# Optimize the null model
initial_params_null <- c(rho=1)
result_null <- optim(par=initial_params_null, fn=null_likelihood_function, y=y, N=N, method="SANN", hessian = TRUE)
logLik_null <- -result_null$value

# Optimize the alternative model using DEoptim
lower_bounds <- c(alpha=0, beta=0, rho=0)
upper_bounds <- c(alpha=1, beta=1, rho=10)
result_alternative <- DEoptim(fn=likelihood_function, lower=lower_bounds, upper=upper_bounds, y=y, N=N)
logLik_alternative <- -result_alternative$optim$bestval

# Compute the GLRT statistic
GLRT_statistic <- -2 * (logLik_null - logLik_alternative)

# Determine the degrees of freedom (difference in number of parameters between the two models)
df <- 2  # alpha and beta are the additional parameters in the alternative model

# Compute the p-value
p_value <- 1 - pchisq(GLRT_statistic, df)

# Print the results
cat("GLRT statistic:", GLRT_statistic, "\n")
cat("Degrees of freedom:", df, "\n")
cat("P-value:", p_value, "\n")

# Decide on the hypothesis based on a significance level (e.g., 0.05)
if (p_value < 0.05) {
  cat("Reject the null hypothesis in favor of the alternative.\n")
} else {
  cat("Fail to reject the null hypothesis.\n")
}


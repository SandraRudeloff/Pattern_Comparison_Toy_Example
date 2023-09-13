
# Helper functions ----
# function to check if a point is within a square centered at (x, y) with side length 2*delta
point_in_square <- function(px, py, x, y, delta) {
  return(px >= (x - delta) & px <= (x + delta) & py >= (y - delta) & py <= (y + delta))
}

# Compute distance between two points
compute_distance <- function(x1, y1, x2, y2) {
  return(sqrt((x1 - x2)^2 + (y1 - y2)^2))
}

get_y <- function(df_population, df_outbreak, delta){
  y <- numeric(nrow(df_population))
  for (i in 1:nrow(df_population)) {
    y[i] <- sum(point_in_square(df_outbreak$x_centroid, df_outbreak$y_centroid, df_population$x_centroid[i], df_population$y_centroid[i], delta))
  }
  return(y)
}

get_N <- function(df_population){
  return(df_population$population)
}

# Raised Incidence Calculation ----
## Risk function for distance d from the source ----
# Calculate the Risk Function for each Source and subregion
risk_function <- function(d, alpha, beta) {
  return(1 + alpha * exp(-(d/beta)^2))
}

## Risk matrix ----
# The risk matrix contains on the rows the cells, 1 being in the left lower corner, and the stores on the columns.
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

# Likelihood ----

#The expected value mu is the overall prevalance of the disease (rho) times the number of population in that cell times the risk_matrix value at that position
#mu enhtält einen value per cell. 

#Erstmal addieren wir alle expected values und die sind dann negativ und dann 

likelihood_function_minimize <- function(params, y, N, df_population, df_shops) {
  alpha <- params[1]
  beta <- params[2]
  rho <- sum(y) / sum(N)
  
  if (alpha < 0 || beta < 0) {
    return(1e6)
  }
  
  risk_matrix <- compute_risk_matrix(df_population, df_shops, alpha, beta)
  mu <- rho * N * apply(risk_matrix, 1, prod)
  
  log_likelihood <- -sum(mu) + sum(y * log(mu)) # without y! because it does not do anything for the optimization
  
  if (is.nan(log_likelihood)) {
    return(1e6)
  }
  return(-log_likelihood) # We return the negative likelihood because optimizers in R typically minimize
}

# Standard Errors ----
## Hessian 
calculate_std_errors_Hessian <- function(result_optimization_DEoptim, y, N, df_population, df_shops){
  library(numDeriv)
  hessian_matrix <- hessian(likelihood_function_minimize, c(result_optimization_DEoptim$optim$bestmem[1], result_optimization_DEoptim$optim$bestmem[2]), y = y, N = N, df_population = df_population, df_shops = df_shops)
  inverse_hessian <- solve(hessian_matrix)
  standard_errors <- sqrt(diag(inverse_hessian))
  return(standard_errors)
}

## Monte Carlo 
calculate_std_errors_MC <- function(result_optimization_DEoptim, n_simulations, df_population, df_shops){
  # Initialize variables to store parameter estimates
  alpha_estimates <- numeric()
  beta_estimates <- numeric()

  
  # Loop over simulations
  for (i in 1:n_simulations) {
    
    risk_matrix <- compute_risk_matrix(df_population, df_shops, result_optimization_DEoptim$optim$bestmem[1], result_optimization_DEoptim$optim$bestmem[2])
    rho <- sum(y) / sum(df_population$population)
    
    mu <- rho * N * apply(risk_matrix, 1, prod)
    
    # Step 2: Generate simulated data (this will depend on your specific model)
    simulated_y <- rpois(length(mu), lambda = mu)
    
    # Step 3: Fit the model to the simulated data
    result_simulated <- DEoptim(fn = likelihood_function_minimize, lower = lower_bounds, upper = upper_bounds, y = simulated_y, N = N, df_population = df_population, df_shops = df_shops)#,  DEoptim.control(itermax = 100))
    
    # Store the estimated parameters
    alpha_estimates[i] <- result_simulated$optim$bestmem[1]
    beta_estimates[i] <- result_simulated$optim$bestmem[2]
    
  }
  
  # Step 4: Calculate standard errors
  se_alpha <- sd(alpha_estimates)
  se_beta <- sd(beta_estimates)
  
  standard_errors <- list(se_alpha = se_alpha, se_beta = se_beta)
  return(standard_errors)

}






likelihood_function_maximize <- function(params, y, N) {
  alpha <- params[1]
  beta <- params[2]
  rho <- sum(y) / sum(df_population$population)
  
  
  if (alpha < 0 || beta < 0) {
    return(1e6)
  }
  
  risk_matrix <- compute_risk_matrix(df_population, df_shops, alpha, beta)
  mu <- rho * N * apply(risk_matrix, 1, prod)
  
  log_likelihood <- -sum(mu) + sum(y * log(mu)) # without y! because it does not do anything for the optimization
  
  if (is.nan(log_likelihood)) {
    return(-1e6)
  }
  return(log_likelihood)
}
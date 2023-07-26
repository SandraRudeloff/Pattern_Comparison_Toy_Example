# 0.  Collect Variables
# pop_density
# Install the akima package if you haven't already
if (!require(akima)) {
  install.packages("akima")
}

# Load the akima package
library(akima)

# Interpolate the population data
interp <- with(df_population, interp(x_centroid, y_centroid, population))

# Define the background intensity function
lambda0 <- function(x, y) {
  # Use bilinear interpolation to get the population at (x, y)
  population <- akima::interp.linear(interp, x, y)
  
  # Return the population divided by the total population
  return(population / sum(df_population$population))
}



# shops_data
# outbreak data




# 1. define the intensity function for your model
lambda <- function(x, y, rho, theta1, theta2, y_i) {
  rho * lambda0(x) * prod(1 + theta1 * exp(-theta2 * (x - y_i)^2))
}

# 2. Define log-likelihood
log_likelihood <- function(params, x, y, y_i) {
  rho <- params[1]
  theta1 <- params[2]
  theta2 <- params[3]
  
  sum(log(lambda(x, y, rho, theta1, theta2, y_i))) - integral(lambda, rho, theta1, theta2, y_i)
}
# integral function is a placeholder for the integral of the intensity function over the entire space. You would need to replace this with a suitable numerical integration function.



# 3. optimize the log-likelihood
start <- c(1, 1, 1)  # initial guesses for rho, theta1, theta2
result <- optim(start, log_likelihood, x = x, y = y, y_i = y_i, control = list(fnscale = -1))
# The control argument is used to tell optim to maximize the function (by default, optim minimizes functions).
# The optim function returns a list that includes the maximum log-likelihood (result$value) and the estimated parameters (result$par).
risk_function <- function(d, alpha, beta) {
  return(1 + alpha * exp(- (d/beta)^2))
}

likelihood_function <- function(params, y, N, d_matrix) {
  alpha <- params[1]
  beta <- params[2]
  rho <- params[3]
  
  mu <- numeric(length(y))
  for (i in 1:length(y)) {
    mu[i] <- rho * N[i] * prod(apply(d_matrix[i, , drop=FALSE], 2, risk_function, alpha=alpha, beta=beta))
  }
  
  L <- sum(-mu + y * log(mu))
  return(-L) # We return the negative likelihood because optimizers in R typically minimize
}


# Sample data
y <- c(10,15,12) # Number of cases in each subregion
N <- c(100, 150, 120) # Number of people at risk in each subregion
d_matrix <- matrix(c(5, 10, 3, 8, 7, 6), ncol=2)# Distance matrix with dimensions p x q


# Initial parameter values
initial_params <- c(alpha=0.5, beta=1, rho=1)

# Optimization
result <- optim(par=initial_params, fn=likelihood_function, y=y, N=N, d_matrix=d_matrix, method="BFGS")

# Extract estimated parameters
estimated_params <- result$par
print(estimated_params)
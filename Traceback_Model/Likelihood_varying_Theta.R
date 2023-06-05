library(spatstat)
# Varying theta with the normal implementation 

theta_1 <- 1
theta_2 <- 1.15
# Example 1: Case with outbreak case close to one store
outbreak_cases <- list(c(0,0), c(0.5,0.5))

x_stores <- c(0, 1, 1, 0)
y_stores <- c(0, 1, 0, 1)

for (outbreak in outbreak_cases) {
  # Calculate the distances between outbreak case and stores
  distances <- sqrt((outbreak[1] - x_stores)^2 + (outbreak[2] - y_stores)^2)
  
  # Calculate the raised incidence part of the intensity function
  raised_incidence <- 1 + theta_1 * exp(-theta_2 * (distances))
  
  # Print the likelihood for the raised incidence
  print(paste("Likelihood for outbreak", str(outbreak), prod(raised_incidence)))
  
}



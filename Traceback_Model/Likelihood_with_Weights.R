# Modify the calculate_weights function to apply a weight transformation
calculate_weights <- function(x_outbreak, y_outbreak, x_stores, y_stores, power) {
  distances <- sqrt((x_outbreak - x_stores)^2 + (y_outbreak - y_stores)^2)
  weights <- (1 / distances)^power
  normalized_weights <- weights / sum(weights)
  return(normalized_weights)
}

# Example 1: Case with outbreak case close to one store
x_outbreak <- -0.1
y_outbreak <- -0.1
x_stores <- c(0, 0, 1, 1)
y_stores <- c(0, 1, 1, 0)

# Calculate the weights using the weight transformation
weights <- calculate_weights(x_outbreak, y_outbreak, x_stores, y_stores, power = 2)

# Calculate the raised incidence part of the intensity function with adjusted weights
raised_incidence <- 1 + abs(1) * exp(-1 * weights * ((x_outbreak - x_stores)^2 + (y_outbreak - y_stores)^2))

# Print the likelihood for the raised incidence
print(paste("Example 1 likelihood (with adjusted weights):", prod(raised_incidence)))

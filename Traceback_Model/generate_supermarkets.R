# Single Chain Uniform ----


# Single Chain random ----
generate_random_supermarket_data <- function(chain_name, num_stores) {
  # Generate all possible cell coordinates
  all_cells <- expand.grid(x = seq(0, 1000, by = 1), y = seq(0, 1000, by = 1))
  
  # Randomly select a subset of cells
  selected_cells <- all_cells[sample(nrow(all_cells), num_stores), ]
  
  # Assign the chain name to the selected cells
  selected_cells$chain <- chain_name
  row.names(selected_cells) <- NULL
  
  return(selected_cells)
}

# single chain population based ----

# Two clustered ----

# Two opposing ----

# Two mixed ----

# Two random ----


# Generate supermarket data
supermarket_data <- generate_random_supermarket_data("Chain1", 2)

# Create a blank plot
plot(1, 1, xlim = c(0, 1000), ylim = c(0, 1000), type = "n", xlab = "X", ylab = "Y", main = "Supermarket Locations")

# Add the supermarket locations
points(supermarket_data$x, supermarket_data$y, pch = 19, col = "blue")

# Add grid lines to represent the cells
abline(v = seq(0, 1000, by = 100), h = seq(0, 1000, by = 100), col = "lightgray", lty = "dotted")

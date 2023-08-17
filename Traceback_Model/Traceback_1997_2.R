library(readxl)
source("generate_population.R")

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

create_raisin_func <- function(x, y) {
  # Construct raisin expressions
  ls_all_raisins <- mapply(function(xi, yi) {
    paste0("(1 +  alpha * (exp(- ((sqrt((x- ", xi, ")^2 + (y- ", yi, ")^2)) / beta )^2)))")
  }, x, y, SIMPLIFY = FALSE)
  
  # Collapse into single string
  str_all_raisins <- paste(ls_all_raisins, collapse = "*")
  
  # Create raisin function
  eval(parse(text = paste("raisin_func <- function(x, y, alpha, beta) {(", str_all_raisins, ")}", sep = "")))
  
  return(raisin_func)
}
#### 1. collect variables ----
investigation_scenario <- 1
no_of_cells <- 100

scenarios_data_population <- read_excel("./Data/scenarios.xlsx", sheet = "Population")

### Population Data ----
N_i <- read_population_data(investigation_scenario, no_of_cells) # number of people at risk in each subregion

df_shops <- subset(read_excel("./Data/scenarios.xlsx", sheet = "Store_Locations"), scenario == investigation_scenario)
raisin_func <- create_raisin_func(df_shops$store_x, df_shops$store_y)



# (sigma, beta,Y,...) & make sure they're in the correct object class (matrix)

W <- nb2mat(nearest.six2) # weight-matrix

Y <- as.matrix(dat1$hp/100000) # house prices
X <- cbind(1, dat1$dlpop) # intercept & population growth
# it's good practice to name the columns
colnames(Y) <- "HP"
colnames(X) <- c("Intercept", "dlpop")

#### 2. define the likelihood function ---- 
likelihood <- function(theta, y, X){
  # theta - coefficient vector (rho, alpha & beta)
  rho <- theta[1]
  alpha <- theta[2]
  beta <- theta[3]
  
  
  
  mu <- rho * N_i 
  
  
  # log likelihood
  logl <- log(det(In - rho * W)) - (n/2) * log(2 * pi * sigma^2) - sum((1/(2 * sigma^2)) * (y - rho * W %*% y - X %*% beta)^2) 
  # W is a weight matrix and y is a vector, so we need matrix multiplication 
  return(-logl)
}


#### 3. optimize using optim() with starting values ----
fit2 <- optim(c(1, 1, 1, 0.5), likelihood, y = Y, X = X, method = "BFGS", control = list(maxit = 1000, trace = TRUE), hessian = TRUE) 

print(fit2$par)
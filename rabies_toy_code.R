# Optimizing Resource Allocation for Rabies Prevention in Brazil Using Spatial Prioritization tools

library(terra)
library(prioritizr)   
library(sf)           
library(terra)         
library(exactextractr) 
if (!require(remotes))
  install.packages("remotes")
remotes::install_github("dirkschumacher/rcbc")
require(rcbc)

# Simulate some rasters
set.seed(42)

# travel time to hospitals and clinics
skewed_values <- rexp(100, rate = 0.1) 

hospital_distance <- rast(nrows = 10, ncols = 10, vals = skewed_values)
crs(hospital_distance) <- "EPSG:4326"  

# Create toy raster for road density - think about it
road_density <- rast(nrows = 10, ncols = 10, vals = runif(100, 0, 1))
crs(road_density) <- "EPSG:4326" 

# Create toy raster for population density - think about it
population_density <- rast(nrows = 10, ncols = 10, vals = sample(1:1000, 100, replace = TRUE))
crs(population_density) <- "EPSG:4326"  

cost <- hospital_distance 

# Risk layers would be the benefit layer
# dog density
log_normal_values <- rlnorm(100, meanlog = 3, sdlog = 1.5)
dog_density <- rast(nrows = 10, ncols = 10, vals = log_normal_values)
crs(dog_density) <- "EPSG:4326" 

# vampire bat presence
bat_presence <- rast(nrows = 10, ncols = 10, vals = sample(0:1, 100, replace = TRUE))
crs(bat_presence) <- "EPSG:4326" 

# Create toy raster for rabies cases
clustered_values <- matrix(rnorm(100, mean = 10, sd = 10), nrow = 10, ncol = 10)
rabies_cases <-  rast(nrows = 10, ncols = 10, vals = clustered_values)

# Combine into a single benefit index just to think about them!
benefit <- (0.5 * dog_density) + (0.3 * bat_presence) + (0.2 * rabies_cases)
benefit <- benefit / max(values(benefit), na.rm = TRUE)
plot(benefit)

# Create cities
city_raster <- rast(nrows = 10, ncols = 10, vals = 1:100)
crs(city_raster) <- "EPSG:4326"  

extent(city_raster) <- ext(-10, 10, -10, 10)

city_polygons <- st_sfc(
  st_polygon(list(cbind(
    c(-8, -5, -5, -8, -8), 
    c(2, 2, 4, 4, 2)
  ))), 
  st_polygon(list(cbind(
    c(3, 6, 6, 3, 3),
    c(-3, -3, 0, 0, -3)
  ))),
  st_polygon(list(cbind(
    c(-6, -3, -3, -6, -6),
    c(-7, -7, -4, -4, -7)
  ))),
  st_polygon(list(cbind(
    c(7, 10, 10, 7, 7),
    c(-1, -1, 3, 3, -1)
  ))),
  st_polygon(list(cbind(
    c(-5, -2, -2, -5, -5),
    c(6, 6, 9, 9, 6)
  ))),
  crs = "EPSG:4326"
)

brazil_pu <- st_sf(city_id = 1:5, geometry = city_polygons)

# Check
plot(brazil_pu["city_id"], main = "Cities")
brazil_pu$cost <- exact_extract(cost, brazil_pu, fun = "mean")
brazil_pu$risk <- exact_extract(benefit$lyr.1, brazil_pu, fun = "mean")
brazil_pu$dog_density <- exact_extract(dog_density, brazil_pu, fun = "mean")
brazil_pu$bat_presence <- exact_extract(bat_presence, brazil_pu, fun = "mean")
brazil_pu$rabies_cases <- exact_extract(dog_density, brazil_pu, fun = "mean")

# Define a budget value 
# cell values denote costs in terms of travel time (minutes to hospital)
# how to interprete this? Allocating efforts for reducing time to healthcare?
# How to change land value to effort in time (~1%)?
budget_value <- terra::global(cost, "sum", na.rm = TRUE)[[1]] * 0.01

# Problem
p <- problem(brazil_pu, features = c('dog_density','bat_presence','rabies_cases') , cost_column = "cost") %>%
  add_max_utility_objective(budget = budget_value) %>%  # maximize total benefit
  add_binary_decisions() %>%                            # select (1) or not (0)
  add_default_solver(gap = 0.1, verbose = TRUE)         # use default solver with 10% gap

# Solve the problem with rcbc
n <- c()
s <- c()

if (require("rcbc")) {
  prcbc <- p %>% add_cbc_solver(verbose = FALSE)
  n <- c(n, "CBC")
  s <- c(s, solve(prcbc))
}

# Check
s

# Plot
plot(hospital_distance)
plot(s$geometry, col=s$solution_1, main = "Optimal prioritization", add=TRUE)
#--------------------------------------------------------------------
# toy data rabies - SpatMCDA -----------------------------------------

install.packages('SpatMCDA')
require(SpatMCDA)

sd_bat_presence <- SpatMCDA::sigmoid_fmf(raster = bat_presence,a = 0.1,c = 15)

sd_road_density <-  sigmoid_fmf(raster = road_density,a = 0.1,c = 12)

dog_density <-  sigmoid_fmf(raster = dog_density,a = 0.1,c = 12)


# Create weight matrix - pairwise comparison between factors (expert opinion)

variables <- c("sd_bat_presence", "sd_road_density", "dog_density")
n <- length(variables)

# Initialize matrix
pairwise_matrix <- matrix(1, nrow = n, ncol = n)
rownames(pairwise_matrix) <- colnames(pairwise_matrix) <- variables

# Fill upper triangle with random integer weights
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    rand_weight <- sample(1:9, 1)
    pairwise_matrix[i, j] <- rand_weight
    pairwise_matrix[j, i] <- 1 / rand_weight
  }
}

# View matrix
print(round(pairwise_matrix, 3))


lambda <- aw_weight(data = pairwise_matrix)

SpatMCDA::consist_test(lambda = 7,n = 3)

rasters <- c(bat_presence, road_density, dog_density)

weights <- c(0.5,0.2,0.3)
risk_map <- wlc(rasters = rasters,
                weight = weights)

raster::plot(risk_map)

#-------------------------------------------------------------------------------------------------------------
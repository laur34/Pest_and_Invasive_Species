# Perform a Mantel test for the different Malaise Trap locations used in this study.
# 10.IV.2019 LH

### Create a distance matrix of the spatial distances between traps
# Gauss-Krüger coordinates are provided, so first convert to latitude and longitudes using a website.
# Load these into geosphere.
setwd("/media/laur/wdhdd1/Pest_and_Invasive_Species")
loc <- read.table("Trap_locations.csv", header=T, sep=",", stringsAsFactors=F)
loc

#install.packages("geosphere")
library(geosphere)

# Create distance matrix:
dist_mat <- distm(loc[,c('Kommazahlen_Laenge', 'Kommazahlen_Breite')])
# Need class "dist"
dist_mat <- as.dist(dist_mat)


### Create a second matrix, consisting of the distances between measured outcomes at the given points (traps)
# which in this case is vegdist with method "jaccard"
# if this method is ok, and not using binary=T.
# run dissimilarity_indices.R
###
d_2016

### Perform Mantel test:
#install.packages("ade4")
library(ade4)

mantel.rtest(dist_mat, d_2016, nrepet = 99)

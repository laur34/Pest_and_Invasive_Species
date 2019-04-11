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
# uses binary = T (for pres/abs)
# run dissimilarity_indices.R
###
d_2016
#####

#### Use the Jaccard dissimilarity matrix created with vegdist in script Calculating_jaccard.R:
by_trap_dissim


### Perform Mantel test:
#install.packages("ade4")
library(ade4)

mantel.rtest(dist_mat, d_2016, nrepet = 99)

mantel.rtest(dist_mat, d_2018, nrepet = 99)

mantel(dist_mat, d_2016, method="pearson", permutations=999) #Mantel statistic r: 0.2264; Significance: 0.129
mantel(dist_mat, d_2018, method="pearson", permutations=999) #Mantel statistic r: 0.7712; Significance: 0.001

#### With vegdist (calculating_jaccard):
mantel.rtest(dist_mat, by_trap_dissim, nrepet=99) #Observation: 0.4575474, Simulated p-value: 0.05, alt=Greater.
#Next, individual years...
mantel.rtest(dist_mat, d2016_by_trap, nrepet=99) #0.4192263, Simulated p-value: 0.01, alt=Greater

mantel.rtest(dist_mat, d2018_by_trap, nrepet=99) #0.247135, Simulated p-value: 0.11, alt=Greater

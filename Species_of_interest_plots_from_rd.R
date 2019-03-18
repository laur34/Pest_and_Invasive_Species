# Create area plots for species of interest, 2016 and 2018 homognate.
setwd("/media/laur/wdhdd1/allNPBW/")
data <- read.table("ForR_5_newFeb.otusNPBW-AllApril2018BINsMegablast_VL.tsv", header=T, sep="\t", stringsAsFactors = F)

data$Species[data$Species == ""] <- NA
data$Family[data$Family == ""] <- NA
data$Subfamily[data$Subfamily == ""] <- NA
data$Genus[data$Genus == ""] <- NA
data$BIN[data$BIN == ""] <- NA
data$Phylum[data$Phylum == ""] <- NA
data$Class[data$Class == ""] <- NA
data$Order[data$Order == ""] <- NA
# Tissue powders only:
data <- data[, !grepl("_224", names(data))]
data <- data[, !grepl("_210", names(data))]
data <- data[, !grepl("Etoh", names(data))]
data <- data[, !grepl("semi", names(data))]
data <- data[, !grepl("filter", names(data))]

names(data)
# Change values greater than 0 to 1, so it will be pres-abs:
data[,c(14:ncol(data))] <- (data[,c(14:ncol(data))] != 0)*1

## Subset to contain columns 1 to (14) and 2016:
data2016 <- cbind.data.frame(data[,1:13], data[,grepl("2016", names(data))] )
## Columns 1 to 14 and 2018:
data2018 <- cbind.data.frame(data[,1:13], data[,grepl("2018", names(data))])


# This will exclude the semilysis, due to different naming scheme
summary2016 <- cbind.data.frame(data2016[1:13],
                                rowSums(data2016[ , which(grepl("1LMai", names(data2016)))]),
                                rowSums(data2016[ , which(grepl("2LMai", names(data2016)))]),
                                rowSums(data2016[ , which(grepl("1LJuni", names(data2016)))]),
                                rowSums(data2016[ , which(grepl("2LJuni", names(data2016)))]),
                                rowSums(data2016[ , which(grepl("1LJuli", names(data2016)))]),
                                rowSums(data2016[ , which(grepl("2LJuli", names(data2016)))]),
                                rowSums(data2016[ , which(grepl("1LAug", names(data2016)))]),
                                rowSums(data2016[ , which(grepl("2LAug", names(data2016)))]), 
                                rowSums(data2016[ , which(grepl("1LSep", names(data2016)))]), 
                                rowSums(data2016[ , which(grepl("2LSep", names(data2016)))])  )

names(summary2016)[14:23] <- c("x2016_May1", "x2016_May2", "x2016_June1", "x2016_June2", "x2016_July1", "x2016_July2", "x2016_Aug1", "x2016_Aug2", "x2016_Sep1", "x2016_Sep2")

summary2018 <- cbind.data.frame(data2018[1:13],
                                rowSums(data2018[ , which(grepl("1LMai", names(data2018)))]),
                                rowSums(data2018[ , which(grepl("2LMai", names(data2018)))]),
                                rowSums(data2018[ , which(grepl("1LJuni", names(data2018)))]),
                                rowSums(data2018[ , which(grepl("2LJuni", names(data2018)))]),
                                rowSums(data2018[ , which(grepl("1LJuli", names(data2018)))]),
                                rowSums(data2018[ , which(grepl("2LJuli", names(data2018)))]),
                                rowSums(data2018[ , which(grepl("1LAug", names(data2018)))]),
                                rowSums(data2018[ , which(grepl("2LAug", names(data2018)))]), 
                                rowSums(data2018[ , which(grepl("1LSep", names(data2018)))]), 
                                rowSums(data2018[ , which(grepl("2LSep", names(data2018)))])  )

names(summary2018)[14:23] <- c("x2018_May1", "x2018_May2", "x2018_June1", "x2018_June2", "x2018_July1", "x2018_July2", "x2018_Aug1", "x2018_Aug2", "x2018_Sep1", "x2018_Sep2")

#Lymantria dispar:
ld_2016 <- summary2016[which(grepl("Lymantria_dispar",summary2016$Species)),]
ld_2016 <- ld_2016[14:ncol(ld_2016)]
ld_2016 <- as.numeric(ld_2016)

ld_2018 <- summary2018[which(grepl("Lymantria_dispar",summary2018$Species)),]
ld_2018 <- ld_2018[14:ncol(ld_2018)]
ld_2018 <- as.numeric(ld_2018)



#Plot:
xvar <- seq(1,length(ld_2016))
xvar <- c("May I", "May II", "June I", "June II", "July I", "July II", "Aug. I", "Aug. II", "Sep. I", "Sep. II")
ax <- list(type="category", showgrid=TRUE, categoryorder="array", categoryarray=xvar )

p <- plot_ly(x=xvar, y=ld_2016, type="scatter", mode="markers", fill="tozeroy", name="2016 (tissue)")

p = add_trace(p, x=xvar, y=ld_2018, type="scatter", mode="markers", fill="tonexty", name="2018 (tissue)" ) %>%
  layout(title="<i>Lymantria dispar</i>", xaxis=ax, yaxis = list(title="Number of detections"))
p

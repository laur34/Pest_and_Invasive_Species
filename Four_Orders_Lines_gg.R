#For BIN-based DB results, create line graph of numbers of BINs of each of
# four major orders detected throughout the two years.
setwd("/media/laur/wdhdd1/allNPBW/")
data <- read.table("ForR_5_newFeb.otusNPBW-AllApril2018BINsMegablast_VL.tsv", header=T, sep="\t", stringsAsFactors = F)

head(data)
colnames(data)
#Convert blank spaces to "NA":
data$Species[data$Species == ""] <- NA
data$Family[data$Family == ""] <- NA
data$Subfamily[data$Subfamily == ""] <- NA
data$Genus[data$Genus == ""] <- NA
data$BIN[data$BIN == ""] <- NA
data$Phylum[data$Phylum == ""] <- NA
data$Class[data$Class == ""] <- NA
data$Order[data$Order == ""] <- NA

#Subset to only the powder ones:
data <- data[, !grepl("Etoh", names(data))]
data <- data[, !grepl("semi", names(data))]
data <- data[, !grepl("filter", names(data))]
data <- data[, !grepl("_224", names(data))]
data <- data[, !grepl("_210", names(data))]
names(data)
ncol(data)
names(data)
# Make it presence absence (if better for plots) -
# Change values greater than 0 to 1:
data[,c(14:ncol(data))] <- (data[,c(14:ncol(data))] != 0)*1

#Group column data by Month I and II (all traps together)
#first by year:
## Subset to contain columns 1 to (14) and 2016:
data2016 <- cbind.data.frame(data[,1:13], data[,grepl("2016", names(data))] )
## Columns 1 to 14 and 2018:
data2018 <- cbind.data.frame(data[,1:13], data[,grepl("2018", names(data))])

#Reorder by collection period
data2016_tax <- data2016[,1:13]
data2016_1Mai <- data2016[, grepl("1LMai", names(data2016))]
data2016_2Mai <- data2016[, grepl("2LMai", names(data2016))]
data2016_1Juni <- data2016[, grepl("1LJuni", names(data2016))]
data2016_2Juni <- data2016[, grepl("2LJuni", names(data2016))]
data2016_1Juli <- data2016[, grepl("1LJuli", names(data2016))]
data2016_2Juli <- data2016[, grepl("2LJuli", names(data2016))]
data2016_1Aug <- data2016[, grepl("1LAug", names(data2016))]
data2016_2Aug <- data2016[, grepl("2LAug", names(data2016))]
data2016_1Sep <- data2016[, grepl("1LSep", names(data2016))]
data2016_2Sep <- data2016[, grepl("2LSep", names(data2016))]

data2018_tax <- data2018[,1:13]
data2018_1Mai <- data2018[, grepl("1LMai", names(data2018))]
data2018_2Mai <- data2018[, grepl("2LMai", names(data2018))]
data2018_1Juni <- data2018[, grepl("1LJuni", names(data2018))]
data2018_2Juni <- data2018[, grepl("2LJuni", names(data2018))]
data2018_1Juli <- data2018[, grepl("1LJuli", names(data2018))]
data2018_2Juli <- data2018[, grepl("2LJuli", names(data2018))]
data2018_1Aug <- data2018[, grepl("1LAug", names(data2018))]
data2018_2Aug <- data2018[, grepl("2LAug", names(data2018))]
data2018_1Sep <- data2018[, grepl("1LSep", names(data2018))]
data2018_2Sep <- data2018[, grepl("2LSep", names(data2018))]


data2016_ro <- cbind.data.frame(data2016_tax, rowSums(data2016_1Mai), rowSums(data2016_2Mai),
                                rowSums(data2016_1Juni), rowSums(data2016_2Juni),
                                rowSums(data2016_1Juli), rowSums(data2016_2Juli),
                                rowSums(data2016_1Aug),  rowSums(data2016_2Aug),
                                rowSums(data2016_1Sep), rowSums(data2016_2Sep) )

data2018_ro <- cbind.data.frame(data2018_tax, rowSums(data2018_1Mai), rowSums(data2018_2Mai),
                                rowSums(data2018_1Juni), rowSums(data2018_2Juni),
                                rowSums(data2018_1Juli), rowSums(data2018_2Juli),
                                rowSums(data2018_1Aug),  rowSums(data2018_2Aug),
                                rowSums(data2018_1Sep), rowSums(data2018_2Sep) )


#df_2016 <- data2016_ro[ , c(4,9,14:23)]
#df_2018 <- data2018_ro[ , c(4,9,14:23)]
bindata_2016 <- data2016_ro[,c(9,14:23)]
bindata_2018 <- data2018_ro[,c(9,14:23)]

agg2016 <- aggregate(bindata_2016[2:11], by=list(bindata_2016$BIN), FUN=sum )
agg2018 <- aggregate(bindata_2016[2:11], by=list(bindata_2018$BIN), FUN=sum )

agg2016 <- agg2016[which(rowSums(agg2016[2:11])>0), ]
agg2018 <- agg2018[which(rowSums(agg2018[2:11])>0), ]

#write.table(agg2016, file="AggBIN_2016_by_collection.tsv", quote=F, sep="\t", row.names = F)
#write.table(agg2018, file="AggBIN_2018_by_collection.tsv", quote=F, sep="\t", row.names = F)
#in LO, VLOOKUP to get Order data back, then import new ones.
agg2016 <- read.table("AggBIN_2016_by_collection_wOrders.csv", header=T)
agg2018 <- read.table("AggBIN_2018_by_collection_wOrders.csv", header=T)

colnames(agg2016) <- c("BIN", "Order", "MayI2016", "MayII2016", "JuneI2016", "JuneII2016", "JulyI2016", "JulyII2016",
                      "AugI2016", "AugII2016", "SepI2016", "SepII2016")

colnames(agg2018) <- c("BIN", "Order", "MayI2018", "MayII2018", "JuneI2018", "JuneII2018", "JulyI2018", "JulyII2018",
                       "AugI2018", "AugII2018", "SepI2018", "SepII2018")

# Aggregate again, this time by order:
agg2016 <- aggregate(agg2016[3:12], by=list(agg2016$Order), FUN=sum )
agg2018 <- aggregate(agg2018[3:12], by=list(agg2018$Order), FUN=sum )

# Need something like transpose of these ^, as input to ggplot2.
n2016 <- agg2016$Group.1
n2018 <- agg2018$Group.1

# transpose the rest of it is as a data frame:
dft2016 <- as.data.frame(t(agg2016[,-1]))
dft2018 <- as.data.frame(t(agg2018[,-1]))

# make the Orders into colnames:
colnames(dft2016) <- n2016
colnames(dft2018) <- n2018

# Plotting
# Make the row names in to a column:
library(data.table)
setDT(dft2016, keep.rownames = T)[]
#Melt that, with the rn column--the samples names--as the id:
dfm2016 <- melt(dft2016, id='rn')
head(dfm2016)
# Specify order of factor levels of rn (samples names):
dfm2016$rn <- factor(dfm2016$rn, levels = dfm2016$rn[1:10])

# Plot a subset of the data:
# As tutorial here: http://www.sthda.com/english/articles/32-r-graphics-essentials/128-plot-time-series-data-using-ggplot/
library(ggplot2)
ss <- subset(dfm2016, variable=="Diptera")

p <-ggplot(data=ss, aes(x=rn, y=value, group=1)) +
  geom_line(color="#00AFBB", size=2)


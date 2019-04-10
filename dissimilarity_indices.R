# Create a matrix of taxa in the rows and sites in the columns.
# Its the finished OTU table with the colunmns reordered from earlier.
# From it, calculate Sorensen diversity, and/or other metrics.

setwd("/media/laur/wdhdd1/allNPBW/")
data <- read.table("Reordered_for_pest_R_5_newFebNPBW_VL.csv", header=T, sep=",", stringsAsFactors = F)

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

bindata <- data[,c(9,14:ncol(data))]

#(getting rid of the column 1 name) ##combine by BIN to avoid duplicate row names.
bindata1 <- aggregate(bindata[,2:ncol(bindata)], by=list(bindata[,1]), FUN=sum )
#Transpose it for BiodiversityR package.
#(first get rid of the column 1 name, then make it the row names)
bindata2 <- bindata1[,-1]
rownames(bindata2) <- bindata1[,1]
bindata2

tbindata2 <- data.frame(t(bindata2))

#Make it presence-absence
tbindata2 <- (tbindata2 != 0)*1

#Subset it to desired study year:
#bindata2016 <- cbind.data.frame(bindata$BIN, bindata[,which(grepl("2016", names(bindata)))])
bindata2016 <- tbindata2[which(grepl("2016", row.names(tbindata2))) ,]
bindata2018 <- tbindata2[which(grepl("2018", row.names(tbindata2))) ,]

library(vegan)
#install.packages("betapart")
library(betapart)
#Group the trap (community) data by category:
length(grep("Jos", row.names(bindata2016)))
factor(c(rep("Igg",10),rep("Jos",10),rep("Sal", 10),rep("T1_02B",10),rep("T1_34B",10),rep("T1_52B",10),rep("T1_63B",10),rep("T3_50B",10),rep("T4_64B",10) ))
groups <- factor(c(rep(1,10),rep(2,10),rep(3, 10),rep(4,10),rep(5,10),rep(6,10),rep(7,10),rep(8,10),rep(9,10)), labels=c("Igg","Jos","Sal25", "T1_02B", "T1_34B","T1_52B", "T1_63B", "T3_50B", "T4_64B") )

#Create the matrix, first aggregating samples into traps by sum:
dat2016 <- as.data.frame(bindata2016)
dat2016$trap <- groups

in_2016 <- aggregate(dat2016[1:150], by=list(as.vector(groups)), FUN=sum )

in_2016_2 <- in_2016[,-1]
rownames(in_2016_2) <- in_2016[,1]
in_2016_2

d_2016 <- vegdist(in_2016_2, method="jaccard")


# Also do 2018...


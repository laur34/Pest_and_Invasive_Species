# Create a matrix of taxa in the rows and sites in the columns.
# Its the finished OTU table with the colunmns reordered from earlier.
# From it, calculate Sorensen diversity, and/or other metrics.

setwd("/media/laur/wdhdd1/allNPBW/")
data <- read.table("Reordered_for_pest_R_5_newFebNPBW_VL.csv", header=T, sep=",", stringsAsFactors = F)

bindata <- data[,c(9,14:ncol(data))]

#(getting rid of the column 1 name) ##combine by BIN to avoid duplicate row names.
bindata1 <- aggregate(bindata[,2:ncol(bindata)], by=list(bindata[,1]), FUN=sum )
#Transpose it for BiodiversityR package.
#(first getting rid of the column 1 name)
bindata2 <- bindata1[,-1]
rownames(bindata2) <- bindata1[,1]

tbindata2 <- data.frame(t(bindata2))

#Make it presence-absence (optional)
#tbindata2[,-1] = (tbindata2[,-1] !=0)*1    (oops, row names)
tbindata2 <- (tbindata2 != 0)*1

#Subset it to desired study year:
#bindata2016 <- cbind.data.frame(bindata$BIN, bindata[,which(grepl("2016", names(bindata)))])
bindata2016 <- tbindata2[which(grepl("2016", row.names(tbindata2))) ,]
bindata2018 <- tbindata2[which(grepl("2018", row.names(tbindata2))) ,]

#install.packages("vegan")
library(vegan)
#install.packages("betapart")
library(betapart)
#Group the trap (community) data by category:
length(grep("Jos", row.names(bindata2016)))
factor(c(rep("Igg",10),rep("Jos",10),rep("Sal", 10),rep("T1_02B",10),rep("T1_34B",10),rep("T1_52B",10),rep("T1_63B",10),rep("T3_50B",10),rep("T4_64B",10) ))
groups <- factor(c(rep(1,10),rep(2,10),rep(3, 10),rep(4,10),rep(5,10),rep(6,10),rep(7,10),rep(8,10),rep(9,10)), labels=c("Igg","Jos","Sal25", "T1_02B", "T1_34B","T1_52B", "T1_63B", "T3_50B", "T4_64B") )

## Jaccard and Sorensen indices do not consider the abundance of species. 
#So, they can only use the presence/absence matrix.
#Try removing row names:
row.names(bindata2016) <- NULL

dist <- beta.pair(bindata2016, index.family="jaccard")

bd <- betadisper(dist[[3]],groups)

plot(bd, main="Beta diversity 2016" )

#plot 2018:
#row.names(bindata2018) <- NULL
dist <- beta.pair(bindata2018, index.family = "jaccard")
#bd <- betadisper(dist[[3]], groups)
length(grep("Igg", row.names(bindata2018)))
length(grep("Jos", row.names(bindata2018)))
length(grep("Sal", row.names(bindata2018)))
length(grep("T1_02B", row.names(bindata2018)))
length(grep("T1_34B", row.names(bindata2018)))
length(grep("T1_52B", row.names(bindata2018)))
length(grep("T1_63B", row.names(bindata2018)))
length(grep("T3_50B", row.names(bindata2018)))
length(grep("T4_64B", row.names(bindata2018)))

groups <- factor(c(rep("Igg",10),rep("Jos",10),rep("Sal", 9),rep("T1_02B",10),rep("T1_34B",10),rep("T1_52B",10),rep("T1_63B",10),rep("T3_50B",10),rep("T4_64B",10) ))
bd <- betadisper(dist[[3]], groups)
plot(bd, main="Beta diversity 2018")

# Calculate Jaccard distances / dissimilarity based on BINs.
# 11.IV.2019 LH
library(vegan)
library(betapart)
setwd("/media/laur/wdhdd1/allNPBW/")
data <- read.table("Reordered_for_pest_R_5_newFebNPBW_VL2.tsv", header=T, sep="\t", stringsAsFactors = F)

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

tbindata2$year = c(rep("2016", 90), rep("2018", 90))
tbindata2$location =c(rep("outside", 30), rep("inside", 60), rep("outside", 30), rep("inside", 60))
tbindata2$trap = c(rep("Igg", 10), rep("Jos", 10), rep("Sal", 10), rep("T102B", 10), rep("T134B", 10), rep("T152B", 10), rep("T163B", 10), rep("T350B", 10), rep("T464B", 10),
                   rep("Igg", 10), rep("Jos", 10), rep("Sal", 10), rep("T102B", 10), rep("T134B", 10), rep("T152B", 10), rep("T163B", 10), rep("T350B", 10), rep("T464B", 10))
samplenames = read.table("trapnames", header=F, stringsAsFactors = F)
tbindata2$sample = samplenames$V1

rownames(tbindata2) <- c()
head(tbindata2[,c(4861:4864, 1:4860)])
mydf <- tbindata2[,c(4861:4864, 1:4860)]

jac <- vegdist(mydf[,5:4864], method="jaccard" )
as.matrix(jac)[mydf$location=="outside",mydf$location=="outside"]
as.dist(as.matrix(jac)[mydf$location=="outside",mydf$location=="outside"])

meandist(jac, mydf$location)
#

meandist(jac, mydf$year)

d2016 <- as.dist(as.matrix(jac)[mydf$year=="2016", mydf$year=="2016"])
d2016_by_trap <- as.dist(meandist(d2016, mydf$trap))

d2018 <- as.dist(as.matrix(jac)[mydf$year=="2018", mydf$year=="2018"])
d2018_by_trap <- as.dist(meandist(d2018, mydf$trap))
#
as.dist(as.matrix(jac)[mydf$trap=="Igg", mydf$trap=="Igg"])
meandist(jac, mydf$trap) # these are DISsimilariries! (see this post: https://stackoverflow.com/questions/51200770/bray-curtis-pairwise-analysis-in-r)

#
by_trap_dissim <- as.dist(meandist(jac, mydf$trap))

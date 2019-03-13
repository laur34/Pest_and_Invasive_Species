# Create 3-column table of BINs for 2016 and 2018 homogenized samples,
#and overlaps (BINs in common between the 2 yrs).
setwd("/media/laur/wdhdd1/allNPBW/")
data <- read.table("ForR_5_newFeb.otusNPBW-AllApril2018BINsMegablast_VL.tsv", header=T, sep="\t", stringsAsFactors = F)
head(data)
colnames(data)
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
#data <- data[, !grepl("X2018_NPBWpowder", names(data))]
data <- data[, !grepl("_224", names(data))]
data <- data[, !grepl("_210", names(data))]
names(data)
ncol(data)
# Make it presence-absence
data[,14:ncol(data)] <- (data[,14:ncol(data)] >0)*1
# Create sum vectors for 2016 and 2018:
x2016_sums <- rowSums(data[14:103])
x2018_sums <- rowSums(data[104:ncol(data)])

datanew <- cbind.data.frame(data[1:13], x2016_sums, x2018_sums)

# Aggregate to combine multiple occurrences of some BINs:
aggBIN <- aggregate(datanew[14:15], by=list(datanew$BIN), FUN=sum)
# That left only those three columns. 
# Make it pres-abs again, so that each BIN will be present or absent in each year:
aggBIN[2:3] <- (aggBIN[2:3] >0)*1
head(aggBIN)
# Create an "Overlap" column:
aggBIN$Overlap <- (aggBIN[2] > 0) & (aggBIN[3] > 0)
aggBIN$Overlap <- as.integer(aggBIN$Overlap)

# Add back in the Phylum, Class, and Order information (based on BIN)
#write.table(aggBIN, file="for_Table1_3cols_aggBIN.csv", quote=F, sep=",", row.names=F)
# LO - VLookup, Sort by Phylum, Class, Order, import new file.
datanew1 <- read.table("for_Table1_3cols_aggBIN_with_Taxa.csv", header=T, sep=",", stringsAsFactors = F)
head(datanew1)
# Calculate the sums of BINs per order in each year and pct overlap.
# First get rid of rows of all 0:
datanew1 <- datanew1[which(rowSums(datanew1[5:6])>0), ]
#Make new table of what we want.
#Create a 2016 column, initializing with the first Order value:
sum(datanew1[which(datanew1$Order=="Haplotaxida"),5])
sum(datanew1[which(datanew1$Order=="Haplotaxida"),6])
sum(datanew1[which(datanew1$Order=="Haplotaxida"),7]) / nrow(datanew1[which(datanew1$Order=="Haplotaxida"),])

x2016 <- sum(datanew1[which(datanew1$Order=="Haplotaxida"),5])
#2018:
x2018 <- sum(datanew1[which(datanew1$Order=="Haplotaxida"),6])
#Overlap:
overlap <- sum(datanew1[which(datanew1$Order=="Haplotaxida"),7]) / nrow(datanew1[which(datanew1$Order=="Haplotaxida"),])

#Add other Orders:
for(i in 2:length(unique(datanew1$Order))){
  x2016 <- append(x2016, sum(datanew1[which(datanew1$Order==unique(datanew1$Order)[i]),5]) )
  x2018 <- append(x2018, sum(datanew1[which(datanew1$Order==unique(datanew1$Order)[i]),6]) )
  overlap <- append(overlap, sum(datanew1[which(datanew1$Order==unique(datanew1$Order)[i]),7]) / nrow(datanew1[which(datanew1$Order==unique(datanew1$Order)[i]),]) )
}

#
length(x2016)
length(x2018)
length(overlap)
length(unique(datanew1$Order))

datanew2 <- cbind.data.frame(unique(datanew1$Order), x2016, x2018, overlap)

setwd("/media/laur/wdhdd1/allNPBW/R_output_new/")
#write.table(datanew2, file="Table1_New_Summary_R5_R.tsv", quote=F, row.names=F, sep="\t")

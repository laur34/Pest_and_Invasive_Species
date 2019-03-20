# Create a table like "powd_eth_sums" for creation of plots.
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
#Subset to only the powder ones and the EtOH ones:
data <- data[, !grepl("semi", names(data))]
data <- data[, !grepl("filter", names(data))]
data <- data[, !grepl("_224", names(data))]
data <- data[, !grepl("_210", names(data))]
names(data)
#In order for a fair comparison, keep only the samples with both powder AND EtOH.
#write.table(names(data)[14:ncol(data)], file="data_column_names.txt", quote=F, row.names=F)
#In LO, re-order and keep only the desired column names. Read in new one:
colnamesnew <- read.table("data_column_names_powd_eth.csv", header=F, stringsAsFactors = F)

ncol(data)
colnames(data)

data1 <- cbind.data.frame(data[,c(1:13)], data[,colnamesnew$V1])
names(data1)
data1eth <- data1[,c(14:97)]
data1powd <- data1[,c(98:139)]

bindata <- cbind.data.frame(data1$BIN, data1powd, data1eth)
#Get rid of rows that are NA:
bindata <- bindata[complete.cases(bindata),]
#To combine BINs that occur in more than one row, aggregate:
Agg <- aggregate(bindata[2:127], by=list(bindata$`data1$BIN`), FUN=sum )
# Create a new summary table of presence and absence in powder and ethanol samples, by BIN.
colnames(Agg)
rowSums(Agg[,2:43]) >0
rowSums(Agg[,44:127]) >0
#AggSums <- cbind.data.frame(Agg[1], as.integer(rowSums(Agg[,2:43])>0), as.integer(rowSums(Agg[44:127])>0)  )
AggSums <- cbind.data.frame(Agg[1], rowSums(Agg[,2:43])>0, rowSums(Agg[44:127])>0  )

InBoth <- AggSums[2] & AggSums[3]
AggSums <- cbind.data.frame(AggSums, InBoth)
head(AggSums)
names(AggSums) <- c("BIN", "InTissue", "InEthanol", "InBoth")
AggSums$InTissue <- as.integer(AggSums$InTissue)
AggSums$InEthanol <- as.integer(AggSums$InEthanol)
AggSums$InBoth <- as.integer(AggSums$InBoth)
head(AggSums)

write.table(AggSums, file="powd_eth_sums_from_R.tsv", sep="\t", quote=F, row.names = F)

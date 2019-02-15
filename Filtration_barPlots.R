setwd("/media/laur/wdhdd1/allNPBW/")
data <- read.table("Filtration_spls_newFeb.otusNPBW-AllApril2018BINsMegablast_VL.tsv", header=T, sep="\t", stringsAsFactors = F)
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

#Create a subset for Coleoptera:
cole <- data[data$Order=="Coleoptera", ]
rowSums(cole[,9:ncol(cole)]) >0
cole <- cole[which(rowSums(cole[,9:ncol(cole)]) >0), ]
#Subset it to keep just Family and the sample columns;
#Aggregate this to make the Families into one row each (sums):
coleAgg <- aggregate(cole[,9:ncol(cole)], by=list(cole$Family), FUN=sum)

## To make stacked bar charts, first need something like a transpose of this ^
## where the rows are the samples; columns are families.

# make the Group.1 column (Families) in to row names:
n <- coleAgg$Group.1
# transpose the rest of it is as a data frame:
coleAgg <- as.data.frame(t(coleAgg[,-1]))
# make the families into colnames:
colnames(coleAgg) <- n


# Plotting
library(ggplot2)
library(reshape)
library(data.table)
#Make the row names in to a column:
setDT(coleAgg, keep.rownames = T)[]
#Melt that, with the rn column--the samples names--as the id:
df <- melt(coleAgg, id='rn')
head(df)

# Specify order of factor levels of rn (samples names):
df$rn <- factor(df$rn, levels = df$rn[1:10])

p <-ggplot(data=df, aes(x=rn, y=value, fill=factor(variable) )) +
  #  geom_col(position = "stack")
  geom_bar(stat="identity", position="fill") +
  scale_y_continuous(labels=waiver(), name="Percentage of reads") +
  scale_x_discrete(name=NULL) +
  theme_bw()

p + theme(axis.text.x = element_text(angle=45), legend.title=element_blank() )+ 
  ggtitle("Coleoptera")


##### Now Diptera:
#Create a subset for Diptera:
dip <- data[data$Order=="Diptera", ]
## keep only rows that are not all zero:
dip <- dip[which(rowSums(dip[,9:ncol(dip)]) >0), ]
#Subset it to keep just Family and the sample columns;
#Aggregate this to make the Families into one row each (sums):
dipAgg <- aggregate(dip[,9:ncol(dip)], by=list(dip$Family), FUN=sum)

## To make stacked bar charts, first need something like a transpose of this ^
## where the rows are the samples; columns are families.

# make the Group.1 column (Families) in to row names:
n <- dipAgg$Group.1
# transpose the rest of it is as a data frame:
dipAgg <- as.data.frame(t(dipAgg[,-1]))
# make the families into colnames:
colnames(dipAgg) <- n

# Plotting
#Make the row names in to a column:
setDT(dipAgg, keep.rownames = T)[]
#Melt that, with the rn column--the samples names--as the id:
df <- melt(dipAgg, id='rn')
head(df)

# Specify order of factor levels of rn (samples names):
df$rn <- factor(df$rn, levels = df$rn[1:10])

p <-ggplot(data=df, aes(x=rn, y=value, fill=factor(variable) )) +
  #  geom_col(position = "stack")
  geom_bar(stat="identity", position="fill") +
  scale_y_continuous(labels=waiver(), name="Percentage of reads") +
  scale_x_discrete(name=NULL) +
  theme_bw()

p + theme(axis.text.x = element_text(angle=45), legend.title=element_blank() )+ 
  ggtitle("Diptera")


#### By Orders (how do Diptera and Coleoptera and Haplotaxida EtOH compare?)
# Create Plots of Orders foundin Powder vs Filters:
## keep only rows that are not all zero:
data1 <- data[which(rowSums(data[,9:ncol(data)]) >0), ]
#Subset it to keep just Order and the sample columns;
#Aggregate this to make the Orders into one row each (sums):
Agg <- aggregate(data1[,9:ncol(data1)], by=list(data1$Order), FUN=sum)

## To make stacked bar charts, first need something like a transpose of this ^
## where the rows are the samples; columns are Orders.

# make the Group.1 column (Orders) in to row names:
n <- Agg$Group.1
# transpose the rest of it is as a data frame:
Agg <- as.data.frame(t(Agg[,-1]))
# make the Orders into colnames:
colnames(Agg) <- n

# Plotting
#Make the row names in to a column:
setDT(Agg, keep.rownames = T)[]
#Melt that, with the rn column--the samples names--as the id:
df <- melt(Agg, id='rn')
head(df)

# Specify order of factor levels of rn (samples names):
df$rn <- factor(df$rn, levels = df$rn[1:10])

p <-ggplot(data=df, aes(x=rn, y=value, fill=factor(variable) )) +
  #  geom_col(position = "stack")
  geom_bar(stat="identity", position="fill") +
  scale_y_continuous(labels=waiver(), name="Percentage of reads") +
  scale_x_discrete(name=NULL) +
  theme_bw()

p + theme(axis.text.x = element_text(angle=45), legend.title=element_blank() )

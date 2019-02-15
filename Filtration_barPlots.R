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
# make the families in to colnames:
colnames(coleAgg) <- n
# make a myFactor column:
coleAgg$myFactor <- factor(row.names(coleAgg))
# Get rid of it
coleAgg <- coleAgg[ ,-(ncol(coleAgg))]

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


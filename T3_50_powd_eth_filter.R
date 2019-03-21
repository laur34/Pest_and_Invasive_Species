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

# Subset it to only T3-50B (and left columns of taxa, etc):
cols2keep <- c("Hit_description", "Phylum", "Class", "Order", "Family", "Subfamily", "Genus", "Species", "BIN",
               "Identity", "Align_len", "E.value", "OTU_ID", "X2018_T3_50B_1LJuli", "X2018_T3_50B_2LJuli", 
               "X2018_T3_50_1LAug_filter", "X2018_T3_50_1LSep_filter", "X2018_T3_50_2LAug_filter", "X2018_T3_50_2LJuli_filter", 
               "X2018_T3_50_2LSep_filter", "X2018_Etoh_T3_50_2L_Juli_1", "X2018_Etoh_T3_50_2L_Juni_1")

data <- data[,cols2keep]

#### By Orders (how do Diptera and Coleoptera and Haplotaxida EtOH compare?)
# Create Plots of Orders found in Powder vs Filters vs EtOH:
# Subset to keep columns Order, Family, and the data ones:
d_ord <- data[,c(4,15,19,21,22)]
head(d_ord)
#Sum the last two columns (Ethanol):
d_eth <- d_ord[4] + d_ord[5]
colnames(d_eth) <- "X2018_Etoh_T3_50_2L_Juli"
#New df:
dford <- cbind.data.frame(d_ord[1:3], d_eth)
head(dford)
#Aggregate this to make the Orders into one row each (sums):
Agg <- aggregate(dford[,c(2:4)], by=list(dford$Order), FUN=sum)
#Get rid of very minor orders:
Agg <- Agg[which(rowSums(Agg[-1])>50), ]
# make the Group.1 column (Orders) in to row names:
n <- Agg$Group.1
# transpose the rest of it is as a data frame:
Agg <- as.data.frame(t(Agg[,-1]))
# make the Orders into colnames:
colnames(Agg) <- n
# Plotting
library(ggplot2)
library(reshape)
library(data.table)
#Make the row names in to a column:
setDT(Agg, keep.rownames = T)[]
#Melt it, with the rn column--the samples names--as the id:
df <- melt(Agg, id='rn')
head(df)
# Specify order of factor levels of rn (samples names):
df$rn <- factor(df$rn, levels = df$rn[c(1,3,2)])
# Plot:
p <-ggplot(data=df, aes(x=rn, y=value, fill=factor(variable) )) +
  #  geom_col(position = "stack")
  geom_bar(stat="identity", position="fill") +
  scale_y_continuous(labels=waiver(), name="Proportion of reads") +
  scale_x_discrete(name=NULL, labels=c("July II tissue", "July II filtered EtOH", "July II EtOH")) +
  theme_bw()

p + theme(axis.text.x = element_text(angle=0), legend.title=element_blank() ) + ggtitle("Order representation by method, trap T3-50B")

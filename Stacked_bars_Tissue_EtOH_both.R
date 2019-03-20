# Create stacked bar charts showing how many BINs were detected in 
# homogenized tissue, ethanol, or both.
setwd("/media/laur/wdhdd1/allNPBW/")
#data <- read.table("powd_eth_sums_tissue_eth_both1.tsv", header=T, sep="\t", stringsAsFactors = F)
data <- read.table("powd_eth_sums_from_R_VLOrd.csv", header=T, sep="\t", stringsAsFactors=F)

head(data)
colnames(data)

# Each row should be an order.
Agg <- aggregate(data[3:5], by=list(data$Order), FUN=sum)
###
big5 <- c("Coleoptera", "Diptera", "Hemiptera", "Hymenoptera", "Lepidoptera")
Agg <- Agg[which(Agg$Group.1 %in% big5), ]
###
n <- Agg$Group.1

Agg_df <- Agg[2:4]
row.names(Agg_df) <- n
head(Agg_df)

library(ggplot2)
library(reshape)
library(data.table)
library(RColorBrewer)
#Make the row names in to a column:
setDT(Agg_df, keep.rownames = T)[]

#Melt that, with the rn column--the samples names--as the id:
df <- melt(Agg_df, id='rn')
head(df)
# Specify order of factor levels of rn (Orders):
df$rn <- factor(df$rn, levels = df$rn[1:5])
#df$variable <- factor(df$variable, levels = c("In_tissue", "In_ethanol", "In_both"))
df$variable <- factor(df$variable, levels = c("InTissue", "InEthanol", "InBoth"))
#Plot:
p <- ggplot(data=df, aes(x=rn, y=value, fill=factor(variable)) ) +
  geom_bar(stat="identity", position="fill") +
  scale_y_continuous(labels=waiver(), name="BINs detected" ) +
  scale_fill_manual(values = rev(brewer.pal(6, "Purples")))
  scale_x_discrete(name=NULL) + theme_bw()
  

q <- ggplot(data=df, aes(x=rn, fill=variable)) +
  geom_bar(data=df[1:5,], aes(y=value), position = "stack", stat="identity") +
  geom_bar(data=df[6:15,], aes(y=-value), stat="identity", position = "stack") +
  scale_fill_manual(values=c("mediumorchid","plum2","darkorchid4") ) +
  scale_x_discrete(name=NULL) + theme_bw() +
  scale_y_continuous("", breaks = NULL)


q + theme(legend.title=element_blank())  + ggtitle("BINs detected in orders by method")


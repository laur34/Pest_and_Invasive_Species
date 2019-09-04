# Create a new clustered bar chart of 4 orders' BIN counts for 3 years.
setwd("/media/laur/wdhdd1/allNPBW/")
# Calculate total BINs for 4 Orders 2016 and 2018:
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
## Subset to contain columns 1 to (14) and 2016:
data2016 <- cbind.data.frame(data[,1:13], data[,grepl("2016", names(data))] )
## Columns 1 to 14 and 2018:
data2018 <- cbind.data.frame(data[,1:13], data[,grepl("2018", names(data))])
## Subset by Order:
data2016Cole <- data2016[which(data2016$Order=="Coleoptera"),]
data2018Cole <- data2018[which(data2018$Order=="Coleoptera"),]

data2016Dip <- data2016[which(data2016$Order=="Diptera"),]
data2018Dip <- data2018[which(data2018$Order=="Diptera"),]

data2016Hym <- data2016[which(data2016$Order=="Hymenoptera"),]
data2018Hym <- data2018[which(data2018$Order=="Hymenoptera"),]

data2016Lep <- data2016[which(data2016$Order=="Lepidoptera"),]
data2018Lep <- data2018[which(data2018$Order=="Lepidoptera"),]
## Keep only rows that are not 0 (have detections):
data2016Cole <- data2016Cole[which(rowSums(data2016Cole[14:ncol(data2016Cole)])>0),]
data2016Dip <- data2016Dip[which(rowSums(data2016Dip[14:ncol(data2016Dip)])>0),]
data2016Hym <- data2016Hym[which(rowSums(data2016Hym[14:ncol(data2016Hym)])>0),]
data2016Lep <- data2016Lep[which(rowSums(data2016Lep[14:ncol(data2016Lep)])>0),]

data2018Cole <- data2018Cole[which(rowSums(data2018Cole[14:ncol(data2018Cole)])>0),]
data2018Dip <- data2018Dip[which(rowSums(data2018Dip[14:ncol(data2018Dip)])>0),]
data2018Hym <- data2018Hym[which(rowSums(data2018Hym[14:ncol(data2018Hym)])>0),]
data2018Lep <- data2018Lep[which(rowSums(data2018Lep[14:ncol(data2018Lep)])>0),]
## Extract the BINs:
lc2016 <- length(unique(na.omit(data2016Cole$BIN))) #268
ld2016 <- length(unique(na.omit(data2016Dip$BIN))) #2118
lh2016 <- length(unique(na.omit(data2016Hym$BIN))) #730
ll2016 <- length(unique(na.omit(data2016Lep$BIN))) #328

lc2018 <- length(unique(na.omit(data2018Cole$BIN))) #234
ld2018 <- length(unique(na.omit(data2018Dip$BIN))) #1903
lh2018 <- length(unique(na.omit(data2018Hym$BIN))) #709
ll2018 <- length(unique(na.omit(data2018Lep$BIN))) #351

#2012:
lc2012 <- 94
ld2012 <- 1566
lh2012 <- 669
ll2012 <- 64

df <- cbind.data.frame(c(lc2012,ld2012,lh2012,ll2012), c(lc2016,ld2016,lh2016,ll2016), c(lc2018,ld2018,lh2018,ll2018) )
names(df) <- c("x2012", "x2016", "x2018")
Order <- (c("Coleoptera", "Diptera", "Hymenoptera", "Lepidoptera") )
df <- cbind.data.frame(Order,df)

#barplot(df$x2012)
library(ggplot2)
library(reshape)
library(RColorBrewer)

mdf <- melt(df, id='Order')

ggplot(data=mdf, aes(x=Order, y=value, fill=factor(variable)) ) +
  geom_bar(stat="identity", position=position_dodge() ) +
  scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"), name=NULL, labels=c("2012","2016","2018") ) +
  theme_bw() +
  scale_x_discrete(name=NULL) +
  scale_y_continuous(labels=waiver(), name=NULL) +
  ggtitle("BINs Detected")


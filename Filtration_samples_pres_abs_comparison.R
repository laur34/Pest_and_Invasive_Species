# Using Filtration and corresponding powder samples, make as presence-absence,
# So that data says in how many filter and how many powder samples
# was each BIN/sp. detected
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
# Filtration-related samples only:
cols2keep <- c("Phylum", "Class", "Order", "Subfamily", "Genus", "Species", "Family", "BIN", "X2018_T3_50B_2LJuli", "X2018_T3_50_2LJuli_filter", "X2018_T3_50B_1LAug", "X2018_T3_50_1LAug_filter", "X2018_T3_50B_2LAug", "X2018_T3_50_2LAug_filter", "X2018_T3_50B_1LSep", "X2018_T3_50_1LSep_filter", "X2018_T3_50B_2LSep", "X2018_T3_50_2LSep_filter")
data <- data[,cols2keep]


#Make it presence-absence:
data[, 9:ncol(data)] <- (data[, 9:ncol(data)] >0)*1
length(data$BIN)
length(unique(data$BIN)) #BINs do not occur uniquely.
#Subset to remove rowsums of zero:
data <- data[which(rowSums(data[9:ncol(data)])>0), ]

# Keep Order and data columns:
data <- data[,c(3,9:ncol(data))]
#Combine BINs that show up more than once, with sum:
#data <- aggregate(data[,4:ncol(data)], by=list(data$BIN), FUN=sum)
#Maybe should do pres-abs again, since now there are 2's, etc. in the table were BINs were detected as more than one sequence in the reference db.
#
#aggregate by order:
data_agord <- aggregate(data[,2:ncol(data)], by=list(data$Order), FUN=sum)
write.table(data_agord, file="Filtration_samples_pres_abs_by_Order.tsv", quote=F, row.names=F, sep="\t")
###### Plot presence-absence stacked bar charts of reads by order in tissue vs filtered ethanol:
Agg <- data_agord
#Eliminate Orders with really small amounts--actually combine them into an "other" ...?
#First eliminate row sums of zero:
Agg <- Agg[which(rowSums(Agg[-1])>0), ]
#And import file which was edited in LO so that small ones are in "other" Order:
Agg <- read.table("Filtration_samples_pres_abs_by_Order_withOthers.csv", header=T, stringsAsFactors = F)
head(Agg, n=12)
Agg <- Agg[1:9,]

## To make stacked bar charts, first need something like a transpose of this ^
## where the rows are the samples; columns are Orders.

# make the Group.1 column (Orders) in to row names:
n <- Agg$Group.1
# transpose the rest of it is as a data frame:
Agg <- as.data.frame(t(Agg[,-1]))

# make the Orders into colnames:
#n <- Agg$Group.1
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
  scale_y_continuous(labels=waiver(), name="Hits with BINs") +
  scale_x_discrete(name=NULL) +
  theme_bw()

p + theme(axis.text.x = element_text(angle=90), legend.title=element_blank() ) + ggtitle("Detections of orders in tissue vs filtered ethanol")


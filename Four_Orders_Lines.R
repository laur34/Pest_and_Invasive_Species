#For BIN-based DB results, create line graph of numbers of BINs of each of
# four major orders detected throughout the two years.
# Like Jerome's for Lepidoptera.
setwd("/media/laur/wdhdd1/allNPBW/")
#data <- read.table("For_R_2-all.otusNPBW-AllApril2018BINsMegablast.tsv", header=T, sep="\t", stringsAsFactors = F)
#data <- read.table("For_R_4-all.otusNPBW-AllApril2018BINsMegablast.tsv", header=T, sep="\t", stringsAsFactors = F)
data <- read.table("ForR_5_newFeb.otusNPBW-AllApril2018BINsMegablast_VL.tsv", header=T, sep="\t", stringsAsFactors = F)

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

#Subset to only the powder ones:
data <- data[, !grepl("Etoh", names(data))]
# remove the last two columns (unidentified names)
#data <- data[, c(1:(ncol(data)-2))]
names(data)
# Make it presence absence (if better for plots) -
# Change values greater than 0 to 1:
data[,c(14:ncol(data))] <- (data[,c(14:ncol(data))] != 0)*1

#Group column data by Month I and II (all traps together)
#first by year:
## Subset to contain columns 1 to (14) and 2016:
data2016 <- cbind.data.frame(data[,1:13], data[,grepl("2016", names(data))] )
## Columns 1 to 14 and 2018:
data2018 <- cbind.data.frame(data[,1:13], data[,grepl("2018", names(data))])

#Then by collection period
data2016_1Mai <- cbind.data.frame(data2016[,1:13], data2016[, grepl("1LMai", names(data2016))])
data2016_2Mai <- cbind.data.frame(data2016[,1:13], data2016[, grepl("2LMai", names(data2016))])
data2016_1Juni <- cbind.data.frame(data2016[,1:13], data2016[, grepl("1LJuni", names(data2016))])
data2016_2Juni <- cbind.data.frame(data2016[,1:13], data2016[, grepl("2LJuni", names(data2016))])
data2016_1Juli <- cbind.data.frame(data2016[,1:13], data2016[, grepl("1LJuli", names(data2016))])
data2016_2Juli <- cbind.data.frame(data2016[,1:13], data2016[, grepl("2LJuli", names(data2016))])
data2016_1Aug <- cbind.data.frame(data2016[,1:13], data2016[, grepl("1LAug", names(data2016))])
data2016_2Aug <- cbind.data.frame(data2016[,1:13], data2016[, grepl("2LAug", names(data2016))])
data2016_1Sep <- cbind.data.frame(data2016[,1:13], data2016[, grepl("1LSep", names(data2016))])
data2016_2Sep <- cbind.data.frame(data2016[,1:13], data2016[, grepl("2LSep", names(data2016))])


#### Subset by Order, e.g. Lepidoptera
data2016_1Mai_lep <- data2016_1Mai[which(data2016_1Mai$Order=="Lepidoptera"), ]
data2016_2Mai_lep <- data2016_2Mai[which(data2016_2Mai$Order=="Lepidoptera"), ]
data2016_1Juni_lep <- data2016_1Juni[which(data2016_1Juni$Order=="Lepidoptera"), ]
data2016_2Juni_lep <- data2016_2Juni[which(data2016_2Juni$Order=="Lepidoptera"), ]
data2016_1Juli_lep <- data2016_1Juli[which(data2016_1Juli$Order=="Lepidoptera"), ]
data2016_2Juli_lep <- data2016_2Juli[which(data2016_2Juli$Order=="Lepidoptera"), ]
data2016_1Aug_lep <- data2016_1Aug[which(data2016_1Aug$Order=="Lepidoptera"), ]
data2016_2Aug_lep <- data2016_2Aug[which(data2016_2Aug$Order=="Lepidoptera"), ]
data2016_1Sep_lep <- data2016_1Sep[which(data2016_1Sep$Order=="Lepidoptera"), ]
data2016_2Sep_lep <- data2016_2Sep[which(data2016_2Sep$Order=="Lepidoptera"), ]

lep1Mai2016 <- sum(rowSums(data2016_1Mai_lep[14:ncol(data2016_1Mai_lep)]))
lep2Mai2016 <- sum(rowSums(data2016_2Mai_lep[14:ncol(data2016_2Mai_lep)]))
lep1Juni2016 <- sum(rowSums(data2016_1Juni_lep[14:ncol(data2016_1Juni_lep)]))
lep2Juni2016 <- sum(rowSums(data2016_2Juni_lep[14:ncol(data2016_2Juni_lep)]))
lep1Juli2016 <- sum(rowSums(data2016_1Juli_lep[14:ncol(data2016_1Juli_lep)]))
lep2Juli2016 <- sum(rowSums(data2016_2Juli_lep[14:ncol(data2016_2Juli_lep)]))
lep1Aug2016 <- sum(rowSums(data2016_1Aug_lep[14:ncol(data2016_1Aug_lep)]))
lep2Aug2016 <- sum(rowSums(data2016_2Aug_lep[14:ncol(data2016_2Aug_lep)]))
lep1Sep2016 <- sum(rowSums(data2016_1Sep_lep[14:ncol(data2016_1Sep_lep)]))
lep2Sep2016 <- sum(rowSums(data2016_2Sep_lep[14:ncol(data2016_2Sep_lep)]))


##### Coleoptera
data2016_1Mai_col <- data2016_1Mai[which(data2016_1Mai$Order=="Coleoptera"), ]
data2016_2Mai_col <- data2016_2Mai[which(data2016_2Mai$Order=="Coleoptera"), ]
data2016_1Juni_col <- data2016_1Juni[which(data2016_1Juni$Order=="Coleoptera"), ]
data2016_2Juni_col <- data2016_2Juni[which(data2016_2Juni$Order=="Coleoptera"), ]
data2016_1Juli_col <- data2016_1Juli[which(data2016_1Juli$Order=="Coleoptera"), ]
data2016_2Juli_col <- data2016_2Juli[which(data2016_2Juli$Order=="Coleoptera"), ]
data2016_1Aug_col <- data2016_1Aug[which(data2016_1Aug$Order=="Coleoptera"), ]
data2016_2Aug_col <- data2016_2Aug[which(data2016_2Aug$Order=="Coleoptera"), ]
data2016_1Sep_col <- data2016_1Sep[which(data2016_1Sep$Order=="Coleoptera"), ]
data2016_2Sep_col <- data2016_2Sep[which(data2016_2Sep$Order=="Coleoptera"), ]

col1Mai2016 <- sum(rowSums(data2016_1Mai_col[14:ncol(data2016_1Mai_col)]))
col2Mai2016 <- sum(rowSums(data2016_2Mai_col[14:ncol(data2016_2Mai_col)]))
col1Juni2016 <- sum(rowSums(data2016_1Juni_col[14:ncol(data2016_1Juni_col)]))
col2Juni2016 <- sum(rowSums(data2016_2Juni_col[14:ncol(data2016_2Juni_col)]))
col1Juli2016 <- sum(rowSums(data2016_1Juli_col[14:ncol(data2016_1Juli_col)]))
col2Juli2016 <- sum(rowSums(data2016_2Juli_col[14:ncol(data2016_2Juli_col)]))
col1Aug2016 <- sum(rowSums(data2016_1Aug_col[14:ncol(data2016_1Aug_col)]))
col2Aug2016 <- sum(rowSums(data2016_2Aug_col[14:ncol(data2016_2Aug_col)]))
col1Sep2016 <- sum(rowSums(data2016_1Sep_col[14:ncol(data2016_1Sep_col)]))
col2Sep2016 <- sum(rowSums(data2016_2Sep_col[14:ncol(data2016_2Sep_col)]))



##### Hymenoptera
data2016_1Mai_hym <- data2016_1Mai[which(data2016_1Mai$Order=="Hymenoptera"), ]
data2016_2Mai_hym <- data2016_2Mai[which(data2016_2Mai$Order=="Hymenoptera"), ]
data2016_1Juni_hym <- data2016_1Juni[which(data2016_1Juni$Order=="Hymenoptera"), ]
data2016_2Juni_hym <- data2016_2Juni[which(data2016_2Juni$Order=="Hymenoptera"), ]
data2016_1Juli_hym <- data2016_1Juli[which(data2016_1Juli$Order=="Hymenoptera"), ]
data2016_2Juli_hym <- data2016_2Juli[which(data2016_2Juli$Order=="Hymenoptera"), ]
data2016_1Aug_hym <- data2016_1Aug[which(data2016_1Aug$Order=="Hymenoptera"), ]
data2016_2Aug_hym <- data2016_2Aug[which(data2016_2Aug$Order=="Hymenoptera"), ]
data2016_1Sep_hym <- data2016_1Sep[which(data2016_1Sep$Order=="Hymenoptera"), ]
data2016_2Sep_hym <- data2016_2Sep[which(data2016_2Sep$Order=="Hymenoptera"), ]

hym1Mai2016 <- sum(rowSums(data2016_1Mai_hym[14:ncol(data2016_1Mai_hym)]))
hym2Mai2016 <- sum(rowSums(data2016_2Mai_hym[14:ncol(data2016_2Mai_hym)]))
hym1Juni2016 <- sum(rowSums(data2016_1Juni_hym[14:ncol(data2016_1Juni_hym)]))
hym2Juni2016 <- sum(rowSums(data2016_2Juni_hym[14:ncol(data2016_2Juni_hym)]))
hym1Juli2016 <- sum(rowSums(data2016_1Juli_hym[14:ncol(data2016_1Juli_hym)]))
hym2Juli2016 <- sum(rowSums(data2016_2Juli_hym[14:ncol(data2016_2Juli_hym)]))
hym1Aug2016 <- sum(rowSums(data2016_1Aug_hym[14:ncol(data2016_1Aug_hym)]))
hym2Aug2016 <- sum(rowSums(data2016_2Aug_hym[14:ncol(data2016_2Aug_hym)]))
hym1Sep2016 <- sum(rowSums(data2016_1Sep_hym[14:ncol(data2016_1Sep_hym)]))
hym2Sep2016 <- sum(rowSums(data2016_2Sep_hym[14:ncol(data2016_2Sep_hym)]))


###### Diptera
data2016_1Mai_dip <- data2016_1Mai[which(data2016_1Mai$Order=="Diptera"), ]
data2016_2Mai_dip <- data2016_2Mai[which(data2016_2Mai$Order=="Diptera"), ]
data2016_1Juni_dip <- data2016_1Juni[which(data2016_1Juni$Order=="Diptera"), ]
data2016_2Juni_dip <- data2016_2Juni[which(data2016_2Juni$Order=="Diptera"), ]
data2016_1Juli_dip <- data2016_1Juli[which(data2016_1Juli$Order=="Diptera"), ]
data2016_2Juli_dip <- data2016_2Juli[which(data2016_2Juli$Order=="Diptera"), ]
data2016_1Aug_dip <- data2016_1Aug[which(data2016_1Aug$Order=="Diptera"), ]
data2016_2Aug_dip <- data2016_2Aug[which(data2016_2Aug$Order=="Diptera"), ]
data2016_1Sep_dip <- data2016_1Sep[which(data2016_1Sep$Order=="Diptera"), ]
data2016_2Sep_dip <- data2016_2Sep[which(data2016_2Sep$Order=="Diptera"), ]

dip1Mai2016 <- sum(rowSums(data2016_1Mai_dip[14:ncol(data2016_1Mai_dip)]))
dip2Mai2016 <- sum(rowSums(data2016_2Mai_dip[14:ncol(data2016_2Mai_dip)]))
dip1Juni2016 <- sum(rowSums(data2016_1Juni_dip[14:ncol(data2016_1Juni_dip)]))
dip2Juni2016 <- sum(rowSums(data2016_2Juni_dip[14:ncol(data2016_2Juni_dip)]))
dip1Juli2016 <- sum(rowSums(data2016_1Juli_dip[14:ncol(data2016_1Juli_dip)]))
dip2Juli2016 <- sum(rowSums(data2016_2Juli_dip[14:ncol(data2016_2Juli_dip)]))
dip1Aug2016 <- sum(rowSums(data2016_1Aug_dip[14:ncol(data2016_1Aug_dip)]))
dip2Aug2016 <- sum(rowSums(data2016_2Aug_dip[14:ncol(data2016_2Aug_dip)]))
dip1Sep2016 <- sum(rowSums(data2016_1Sep_dip[14:ncol(data2016_1Sep_dip)]))
dip2Sep2016 <- sum(rowSums(data2016_2Sep_dip[14:ncol(data2016_2Sep_dip)]))


################## 2018 ############################################
#Then by collection period
data2018_1Mai <- cbind.data.frame(data2018[,1:13], data2018[, grepl("1LMai", names(data2018))])
data2018_2Mai <- cbind.data.frame(data2018[,1:13], data2018[, grepl("2LMai", names(data2018))])
data2018_1Juni <- cbind.data.frame(data2018[,1:13], data2018[, grepl("1LJuni", names(data2018))])
data2018_2Juni <- cbind.data.frame(data2018[,1:13], data2018[, grepl("2LJuni", names(data2018))])
data2018_1Juli <- cbind.data.frame(data2018[,1:13], data2018[, grepl("1LJuli", names(data2018))])
data2018_2Juli <- cbind.data.frame(data2018[,1:13], data2018[, grepl("2LJuli", names(data2018))])
data2018_1Aug <- cbind.data.frame(data2018[,1:13], data2018[, grepl("1LAug", names(data2018))])
data2018_2Aug <- cbind.data.frame(data2018[,1:13], data2018[, grepl("2LAug", names(data2018))])
data2018_1Sep <- cbind.data.frame(data2018[,1:13], data2018[, grepl("1LSep", names(data2018))])
data2018_2Sep <- cbind.data.frame(data2018[,1:13], data2018[, grepl("2LSep", names(data2018))])


#### Subset by Order, e.g. Lepidoptera
data2018_1Mai_lep <- data2018_1Mai[which(data2018_1Mai$Order=="Lepidoptera"), ]
data2018_2Mai_lep <- data2018_2Mai[which(data2018_2Mai$Order=="Lepidoptera"), ]
data2018_1Juni_lep <- data2018_1Juni[which(data2018_1Juni$Order=="Lepidoptera"), ]
data2018_2Juni_lep <- data2018_2Juni[which(data2018_2Juni$Order=="Lepidoptera"), ]
data2018_1Juli_lep <- data2018_1Juli[which(data2018_1Juli$Order=="Lepidoptera"), ]
data2018_2Juli_lep <- data2018_2Juli[which(data2018_2Juli$Order=="Lepidoptera"), ]
data2018_1Aug_lep <- data2018_1Aug[which(data2018_1Aug$Order=="Lepidoptera"), ]
data2018_2Aug_lep <- data2018_2Aug[which(data2018_2Aug$Order=="Lepidoptera"), ]
data2018_1Sep_lep <- data2018_1Sep[which(data2018_1Sep$Order=="Lepidoptera"), ]
data2018_2Sep_lep <- data2018_2Sep[which(data2018_2Sep$Order=="Lepidoptera"), ]

lep1Mai2018 <- sum(rowSums(data2018_1Mai_lep[14:ncol(data2018_1Mai_lep)]))
lep2Mai2018 <- sum(rowSums(data2018_2Mai_lep[14:ncol(data2018_2Mai_lep)]))
lep1Juni2018 <- sum(rowSums(data2018_1Juni_lep[14:ncol(data2018_1Juni_lep)]))
lep2Juni2018 <- sum(rowSums(data2018_2Juni_lep[14:ncol(data2018_2Juni_lep)]))
lep1Juli2018 <- sum(rowSums(data2018_1Juli_lep[14:ncol(data2018_1Juli_lep)]))
lep2Juli2018 <- sum(rowSums(data2018_2Juli_lep[14:ncol(data2018_2Juli_lep)]))
lep1Aug2018 <- sum(rowSums(data2018_1Aug_lep[14:ncol(data2018_1Aug_lep)]))
lep2Aug2018 <- sum(rowSums(data2018_2Aug_lep[14:ncol(data2018_2Aug_lep)]))
lep1Sep2018 <- sum(rowSums(data2018_1Sep_lep[14:ncol(data2018_1Sep_lep)]))
lep2Sep2018 <- sum(rowSums(data2018_2Sep_lep[14:ncol(data2018_2Sep_lep)]))

#Plot lep:
plot(c(lep1Mai2018, lep2Mai2018, lep1Juni2018, lep2Juni2018, lep1Juli2018, lep2Juli2018, lep1Aug2018, lep2Aug2018, lep1Sep2018, lep2Sep2018), type="l", xlab="collection", ylab="Count of BINs detected", col="blue" )
lines(c(lep1Mai2016, lep2Mai2016, lep1Juni2016, lep2Juni2016, lep1Juli2016, lep2Juli2016, lep1Aug2016, lep2Aug2016, lep1Sep2016, lep2Sep2016), col="red")
title(main="Lepidoptera")
legend("topright", legend=c("2018", "2016"), col=c("blue", "red"), pch=14)

##### Coleoptera
data2018_1Mai_col <- data2018_1Mai[which(data2018_1Mai$Order=="Coleoptera"), ]
data2018_2Mai_col <- data2018_2Mai[which(data2018_2Mai$Order=="Coleoptera"), ]
data2018_1Juni_col <- data2018_1Juni[which(data2018_1Juni$Order=="Coleoptera"), ]
data2018_2Juni_col <- data2018_2Juni[which(data2018_2Juni$Order=="Coleoptera"), ]
data2018_1Juli_col <- data2018_1Juli[which(data2018_1Juli$Order=="Coleoptera"), ]
data2018_2Juli_col <- data2018_2Juli[which(data2018_2Juli$Order=="Coleoptera"), ]
data2018_1Aug_col <- data2018_1Aug[which(data2018_1Aug$Order=="Coleoptera"), ]
data2018_2Aug_col <- data2018_2Aug[which(data2018_2Aug$Order=="Coleoptera"), ]
data2018_1Sep_col <- data2018_1Sep[which(data2018_1Sep$Order=="Coleoptera"), ]
data2018_2Sep_col <- data2018_2Sep[which(data2018_2Sep$Order=="Coleoptera"), ]

col1Mai2018 <- sum(rowSums(data2018_1Mai_col[14:ncol(data2018_1Mai_col)]))
col2Mai2018 <- sum(rowSums(data2018_2Mai_col[14:ncol(data2018_2Mai_col)]))
col1Juni2018 <- sum(rowSums(data2018_1Juni_col[14:ncol(data2018_1Juni_col)]))
col2Juni2018 <- sum(rowSums(data2018_2Juni_col[14:ncol(data2018_2Juni_col)]))
col1Juli2018 <- sum(rowSums(data2018_1Juli_col[14:ncol(data2018_1Juli_col)]))
col2Juli2018 <- sum(rowSums(data2018_2Juli_col[14:ncol(data2018_2Juli_col)]))
col1Aug2018 <- sum(rowSums(data2018_1Aug_col[14:ncol(data2018_1Aug_col)]))
col2Aug2018 <- sum(rowSums(data2018_2Aug_col[14:ncol(data2018_2Aug_col)]))
col1Sep2018 <- sum(rowSums(data2018_1Sep_col[14:ncol(data2018_1Sep_col)]))
col2Sep2018 <- sum(rowSums(data2018_2Sep_col[14:ncol(data2018_2Sep_col)]))

#Plot col:
plot(c(col1Mai2018, col2Mai2018, col1Juni2018, col2Juni2018, col1Juli2018, col2Juli2018, col1Aug2018, col2Aug2018, col1Sep2018, col2Sep2018), type="l", xlab="collection", ylab="Count of BINs detected", col="blue" )
lines(c(col1Mai2016, col2Mai2016, col1Juni2016, col2Juni2016, col1Juli2016, col2Juli2016, col1Aug2016, col2Aug2016, col1Sep2016, col2Sep2016), col="red")
title(main="Coleoptera")
legend("topright", legend=c("2018", "2016"), col=c("blue", "red"), pch=14)



##### Hymenoptera
data2018_1Mai_hym <- data2018_1Mai[which(data2018_1Mai$Order=="Hymenoptera"), ]
data2018_2Mai_hym <- data2018_2Mai[which(data2018_2Mai$Order=="Hymenoptera"), ]
data2018_1Juni_hym <- data2018_1Juni[which(data2018_1Juni$Order=="Hymenoptera"), ]
data2018_2Juni_hym <- data2018_2Juni[which(data2018_2Juni$Order=="Hymenoptera"), ]
data2018_1Juli_hym <- data2018_1Juli[which(data2018_1Juli$Order=="Hymenoptera"), ]
data2018_2Juli_hym <- data2018_2Juli[which(data2018_2Juli$Order=="Hymenoptera"), ]
data2018_1Aug_hym <- data2018_1Aug[which(data2018_1Aug$Order=="Hymenoptera"), ]
data2018_2Aug_hym <- data2018_2Aug[which(data2018_2Aug$Order=="Hymenoptera"), ]
data2018_1Sep_hym <- data2018_1Sep[which(data2018_1Sep$Order=="Hymenoptera"), ]
data2018_2Sep_hym <- data2018_2Sep[which(data2018_2Sep$Order=="Hymenoptera"), ]

hym1Mai2018 <- sum(rowSums(data2018_1Mai_hym[14:ncol(data2018_1Mai_hym)]))
hym2Mai2018 <- sum(rowSums(data2018_2Mai_hym[14:ncol(data2018_2Mai_hym)]))
hym1Juni2018 <- sum(rowSums(data2018_1Juni_hym[14:ncol(data2018_1Juni_hym)]))
hym2Juni2018 <- sum(rowSums(data2018_2Juni_hym[14:ncol(data2018_2Juni_hym)]))
hym1Juli2018 <- sum(rowSums(data2018_1Juli_hym[14:ncol(data2018_1Juli_hym)]))
hym2Juli2018 <- sum(rowSums(data2018_2Juli_hym[14:ncol(data2018_2Juli_hym)]))
hym1Aug2018 <- sum(rowSums(data2018_1Aug_hym[14:ncol(data2018_1Aug_hym)]))
hym2Aug2018 <- sum(rowSums(data2018_2Aug_hym[14:ncol(data2018_2Aug_hym)]))
hym1Sep2018 <- sum(rowSums(data2018_1Sep_hym[14:ncol(data2018_1Sep_hym)]))
hym2Sep2018 <- sum(rowSums(data2018_2Sep_hym[14:ncol(data2018_2Sep_hym)]))

xtick <-c("May I", "May II", "June I", "June II", "July I", "July II", "Aug. I", "Aug. II", "Sep. I", "Sep. II")
plot(c(hym1Mai2018, hym2Mai2018, hym1Juni2018, hym2Juni2018, hym1Juli2018, hym2Juli2018, hym1Aug2018, hym2Aug2018, hym1Sep2018, hym2Sep2018), type="l",
     xlab="collection", ylab="Count of BINs detected", col="blue", ylim=c(10,600), xaxt="n")
lines(c(hym1Mai2016, hym2Mai2016, hym1Juni2016, hym2Juni2016, hym1Juli2016, hym2Juli2016, hym1Aug2016, hym2Aug2016, hym1Sep2016, hym2Sep2016), col="red")
axis(side=1, labels=F, at=c(1:10))
text(x=c(1:10), labels=xtick, srt=45)
title(main="Hymenoptera")
legend("topright", legend=c("2018", "2016"), col=c("blue", "red"), pch=14)

###### Diptera
data2018_1Mai_dip <- data2018_1Mai[which(data2018_1Mai$Order=="Diptera"), ]
data2018_2Mai_dip <- data2018_2Mai[which(data2018_2Mai$Order=="Diptera"), ]
data2018_1Juni_dip <- data2018_1Juni[which(data2018_1Juni$Order=="Diptera"), ]
data2018_2Juni_dip <- data2018_2Juni[which(data2018_2Juni$Order=="Diptera"), ]
data2018_1Juli_dip <- data2018_1Juli[which(data2018_1Juli$Order=="Diptera"), ]
data2018_2Juli_dip <- data2018_2Juli[which(data2018_2Juli$Order=="Diptera"), ]
data2018_1Aug_dip <- data2018_1Aug[which(data2018_1Aug$Order=="Diptera"), ]
data2018_2Aug_dip <- data2018_2Aug[which(data2018_2Aug$Order=="Diptera"), ]
data2018_1Sep_dip <- data2018_1Sep[which(data2018_1Sep$Order=="Diptera"), ]
data2018_2Sep_dip <- data2018_2Sep[which(data2018_2Sep$Order=="Diptera"), ]

dip1Mai2018 <- sum(rowSums(data2018_1Mai_dip[14:ncol(data2018_1Mai_dip)]))
dip2Mai2018 <- sum(rowSums(data2018_2Mai_dip[14:ncol(data2018_2Mai_dip)]))
dip1Juni2018 <- sum(rowSums(data2018_1Juni_dip[14:ncol(data2018_1Juni_dip)]))
dip2Juni2018 <- sum(rowSums(data2018_2Juni_dip[14:ncol(data2018_2Juni_dip)]))
dip1Juli2018 <- sum(rowSums(data2018_1Juli_dip[14:ncol(data2018_1Juli_dip)]))
dip2Juli2018 <- sum(rowSums(data2018_2Juli_dip[14:ncol(data2018_2Juli_dip)]))
dip1Aug2018 <- sum(rowSums(data2018_1Aug_dip[14:ncol(data2018_1Aug_dip)]))
dip2Aug2018 <- sum(rowSums(data2018_2Aug_dip[14:ncol(data2018_2Aug_dip)]))
dip1Sep2018 <- sum(rowSums(data2018_1Sep_dip[14:ncol(data2018_1Sep_dip)]))
dip2Sep2018 <- sum(rowSums(data2018_2Sep_dip[14:ncol(data2018_2Sep_dip)]))

plot(c(dip1Mai2018, dip2Mai2018, dip1Juni2018, dip2Juni2018, dip1Juli2018, dip2Juli2018, dip1Aug2018, dip2Aug2018, dip1Sep2018, dip2Sep2018), type="l", xlab="collection", ylab="Count of BINs detected", col="blue", ylim=c(10,2400) )
lines(c(dip1Mai2016, dip2Mai2016, dip1Juni2016, dip2Juni2016, dip1Juli2016, dip2Juli2016, dip1Aug2016, dip2Aug2016, dip1Sep2016, dip2Sep2016), col="red")
title(main="Diptera")
legend("topright", legend=c("2018", "2016"), col=c("blue", "red"), pch=14)

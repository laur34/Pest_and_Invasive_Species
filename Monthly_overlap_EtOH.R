setwd("/media/laur/wdhdd1/allNPBW/")
data <- read.table("For_R_all.otusNPBW-AllApril2018BINsMegablast.csv", header=T, sep=",", stringsAsFactors = F)
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

#Presence-absence:
bindata <- data[,c(9,15:319)]
bindata[,-1] = (bindata[,-1] !=0)*1

# Create a sub-df of Ethanol samples (BIN data); group by Month
bindata_etoh <- cbind.data.frame(bindata[1], bindata[, grepl("Etoh", names(bindata))])

may_etoh <- cbind.data.frame(bindata_etoh[1], bindata_etoh[, grepl("Mai", names(bindata_etoh))])
june_etoh <- cbind.data.frame(bindata_etoh[1], bindata_etoh[, grepl("Juni", names(bindata_etoh))])
july_etoh <- cbind.data.frame(bindata_etoh[1], bindata_etoh[, grepl("Juli", names(bindata_etoh))])

# Create a sub-df of Powder samples (BIN data); group by Month


# Create lists of BINs in each EtOh sample, by Months
rowSums(may_etoh[2:ncol(may_etoh)]) >0
tf <- cbind(may_etoh[1], rowSums(may_etoh[2:ncol(may_etoh)])>0)
maybins <- tf[tf[2]==TRUE ,1]

rowSums(june_etoh[2:ncol(june_etoh)]) >0
tf <- cbind(june_etoh[1], rowSums(june_etoh[2:ncol(june_etoh)])>0)
junebins <- tf[tf[2]==TRUE ,1]

rowSums(july_etoh[2:ncol(july_etoh)]) >0
tf <- cbind(july_etoh[1], rowSums(july_etoh[2:ncol(july_etoh)])>0)
julybins <- tf[tf[2]==TRUE ,1]

# Create lists of BINs in each Powdered sample, by Months

# For Powder BINs, get the overlap between May and every other month.
length(intersect(maybins, maybins))  #322
length(intersect(maybins, junebins)) #209
length(intersect(maybins, julybins)) #167
# except that the numbers of samples are unequal, so not fair comparison:
names(may_etoh)
names(june_etoh)
names(july_etoh)

#namesetoh <- names(bindata_etoh[2:ncol(bindata_etoh)])
#names(bindata[, grepl("IGG", namesetoh)])
###################IGG
igg_etoh <- cbind.data.frame(bindata_etoh[1], bindata_etoh[, grepl("IGG", names(bindata_etoh))])
names(igg_etoh)
igg_etoh[, c(1,6,7,10,11,4,5,8,9,2,3)]
igg_etoh_may <- igg_etoh[, c(1,6,7,10,11)]
igg_etoh_may1 <- igg_etoh[, c(1,6,7)]
igg_etoh_may2 <- igg_etoh[, c(1,10,11)]
igg_etoh_may1_bins <- igg_etoh_may1[which(rowSums(igg_etoh_may1[2:3])>0), 1]
igg_etoh_may2_bins <- igg_etoh_may2[which(rowSums(igg_etoh_may2[2:3])>0), 1]

igg_etoh_june <- igg_etoh[, c(1,4,5,8,9)]
igg_etoh_june1 <- igg_etoh[, c(1,4,5)]
igg_etoh_june2 <- igg_etoh[, c(1,8,9)]
igg_etoh_june1_bins <- igg_etoh_june1[which(rowSums(igg_etoh_june1[2:3]) >0), 1]
igg_etoh_june2_bins <- igg_etoh_june2[which(rowSums(igg_etoh_june2[2:3]) >0), 1]

igg_etoh_july <- igg_etoh[, c(1,2,3)]
igg_etoh_july1 <- igg_etoh[, c(1,2,3)]
igg_etoh_july1_bins <- igg_etoh_july1[which(rowSums(igg_etoh_july1[2:3]) >0) , 1]

igg_m1m2 <- length(intersect(igg_etoh_may1_bins, igg_etoh_may2_bins))  #16
igg_m1j1 <- length(intersect(igg_etoh_may1_bins, igg_etoh_june1_bins)) #45
igg_m1j2 <- length(intersect(igg_etoh_may1_bins, igg_etoh_june2_bins)) #58
igg_m1jl1 <-length(intersect(igg_etoh_may1_bins, igg_etoh_july1_bins)) #52
plot(c(igg_m1m2,igg_m1j1,igg_m1j2,igg_m1jl1))
title(main="Igg Ethanol overlap in BINs")

#################Jos
names(bindata[, grepl("Jos", namesetoh)])

jos_etoh <- cbind.data.frame(bindata_etoh[1], bindata_etoh[, grepl("Jos", names(bindata_etoh))])
names(jos_etoh)
jos_etoh[, c(1,6,7,10,11,4,5,8,9,2,3)]
jos_etoh_may <- jos_etoh[, c(1,6,7,10,11)]
jos_etoh_may1 <- jos_etoh[, c(1,6,7)]
jos_etoh_may2 <- jos_etoh[, c(1,10,11)]
jos_etoh_may1_bins <- jos_etoh_may1[which(rowSums(jos_etoh_may1[2:3])>0), 1]
jos_etoh_may2_bins <- jos_etoh_may2[which(rowSums(jos_etoh_may2[2:3])>0), 1]

jos_etoh_june <- jos_etoh[, c(1,4,5,8,9)]
jos_etoh_june1 <- jos_etoh[, c(1,4,5)]
jos_etoh_june2 <- jos_etoh[, c(1,8,9)]
jos_etoh_june1_bins <- jos_etoh_june1[which(rowSums(jos_etoh_june1[2:3]) >0), 1]
jos_etoh_june2_bins <- jos_etoh_june2[which(rowSums(jos_etoh_june2[2:3]) >0), 1]

jos_etoh_july <- jos_etoh[, c(1,2,3)]
jos_etoh_july1 <- jos_etoh[, c(1,2,3)]
jos_etoh_july1_bins <- jos_etoh_july1[which(rowSums(jos_etoh_july1[2:3]) >0) , 1]

jos_m1m2 <- length(intersect(jos_etoh_may1_bins, jos_etoh_may2_bins))  #46
jos_m1j1 <- length(intersect(jos_etoh_may1_bins, jos_etoh_june1_bins)) #42
jos_m1j2 <-length(intersect(jos_etoh_may1_bins, jos_etoh_june2_bins)) #35
jos_m1jl1 <-length(intersect(jos_etoh_may1_bins, jos_etoh_july1_bins)) #36
plot(c(jos_m1m2,jos_m1j1,jos_m1j2,jos_m1jl1))
title(main="Jos Ethanol overlap in BINs")

####################Sal
sal_etoh <- cbind.data.frame(bindata_etoh[1], bindata_etoh[, grepl("Sal", names(bindata_etoh))])
names(sal_etoh)
sal_etoh[, c(1,6,7,10,11,4,5,8,9,2,3)]
sal_etoh_may <- sal_etoh[, c(1,6,7,10,11)]
sal_etoh_may1 <- sal_etoh[, c(1,6,7)]
sal_etoh_may2 <- sal_etoh[, c(1,10,11)]
sal_etoh_may1_bins <- sal_etoh_may1[which(rowSums(sal_etoh_may1[2:3])>0), 1]
sal_etoh_may2_bins <- sal_etoh_may2[which(rowSums(sal_etoh_may2[2:3])>0), 1]

sal_etoh_june <- sal_etoh[, c(1,4,5,8,9)]
sal_etoh_june1 <- sal_etoh[, c(1,4,5)]
sal_etoh_june2 <- sal_etoh[, c(1,8,9)]
sal_etoh_june1_bins <- sal_etoh_june1[which(rowSums(sal_etoh_june1[2:3]) >0), 1]
sal_etoh_june2_bins <- sal_etoh_june2[which(rowSums(sal_etoh_june2[2:3]) >0), 1]

sal_etoh_july <- sal_etoh[, c(1,2,3)]
sal_etoh_july1 <- sal_etoh[, c(1,2,3)]
sal_etoh_july1_bins <- sal_etoh_july1[which(rowSums(sal_etoh_july1[2:3]) >0) , 1]

sal_m1m2 <- length(intersect(sal_etoh_may1_bins, sal_etoh_may2_bins))  # 42
sal_m1j1 <- length(intersect(sal_etoh_may1_bins, sal_etoh_june1_bins)) # 48
sal_m1j2 <- length(intersect(sal_etoh_may1_bins, sal_etoh_june2_bins)) # 46
sal_m1jl1 <-length(intersect(sal_etoh_may1_bins, sal_etoh_july1_bins)) # 49
plot(c(sal_m1m2,sal_m1j1,sal_m1j2,sal_m1jl1))
title(main="Sal-25 Ethanol overlap in BINs")

##########################
grepl("T1_02", namesetoh)

T1_02_etoh <- cbind.data.frame(bindata_etoh[1], bindata_etoh[, grepl("T1_02", names(bindata_etoh))])
names(T1_02_etoh)
T1_02_etoh[, c(1,4,5,8,9,3,6,7,2)]
T1_02_etoh_may <- T1_02_etoh[, c(1,4,5,8,9)]
T1_02_etoh_may1 <- T1_02_etoh[, c(1,4,5)]
T1_02_etoh_may2 <- T1_02_etoh[, c(1,8,9)]
T1_02_etoh_may1_bins <- T1_02_etoh_may1[which(rowSums(T1_02_etoh_may1[2:3])>0), 1]
T1_02_etoh_may2_bins <- T1_02_etoh_may2[which(rowSums(T1_02_etoh_may2[2:3])>0), 1]

T1_02_etoh_june <- T1_02_etoh[, c(1,3,6,7)]
T1_02_etoh_june1 <- T1_02_etoh[, c(1,3)]
T1_02_etoh_june2 <- T1_02_etoh[, c(1,6,7)]
T1_02_etoh_june1_bins <- T1_02_etoh_june1[which(rowSums(T1_02_etoh_june1[2]) >0), 1]
T1_02_etoh_june2_bins <- T1_02_etoh_june2[which(rowSums(T1_02_etoh_june2[2:3]) >0), 1]

T1_02_etoh_july <- T1_02_etoh[, c(1,2)]
T1_02_etoh_july1 <- T1_02_etoh[, c(1,2)]
T1_02_etoh_july1_bins <- T1_02_etoh_july1[which(rowSums(T1_02_etoh_july1[2]) >0) , 1]

t102_m1m2 <- length(intersect(T1_02_etoh_may1_bins, T1_02_etoh_may2_bins))  # 38
t102_m1j1 <- length(intersect(T1_02_etoh_may1_bins, T1_02_etoh_june1_bins)) # 27
t102_m1j2 <- length(intersect(T1_02_etoh_may1_bins, T1_02_etoh_june2_bins)) # 45
t102_m1jl1 <-length(intersect(T1_02_etoh_may1_bins, T1_02_etoh_july1_bins)) # 19
plot(c(t102_m1m2,t102_m1j1,t102_m1j2,t102_m1jl1))
title(main="T1-02B Ethanol overlap in BINs")

###########################
grepl("T1_34", namesetoh)
T1_34_etoh <- cbind.data.frame(bindata_etoh[1], bindata_etoh[, grepl("T1_34", names(bindata_etoh))])
names(T1_34_etoh)
T1_34_etoh[, c(1,6,7,10,11,4,5,8,9,2,3)]
T1_34_etoh_may <- T1_34_etoh[, c(1,6,7,10,11)]
T1_34_etoh_may1 <- T1_34_etoh[, c(1,6,7)]
T1_34_etoh_may2 <- T1_34_etoh[, c(1,10,11)]
T1_34_etoh_may1_bins <- T1_34_etoh_may1[which(rowSums(T1_34_etoh_may1[2:3])>0), 1]
T1_34_etoh_may2_bins <- T1_34_etoh_may2[which(rowSums(T1_34_etoh_may2[2:3])>0), 1]

T1_34_etoh_june <- T1_34_etoh[, c(1,4,5,8,9)]
T1_34_etoh_june1 <- T1_34_etoh[, c(1,4,5)]
T1_34_etoh_june2 <- T1_34_etoh[, c(1,8,9)]
T1_34_etoh_june1_bins <- T1_34_etoh_june1[which(rowSums(T1_34_etoh_june1[2:3]) >0), 1]
T1_34_etoh_june2_bins <- T1_34_etoh_june2[which(rowSums(T1_34_etoh_june2[2:3]) >0), 1]

T1_34_etoh_july <- T1_34_etoh[, c(1,2,3)]
T1_34_etoh_july1 <- T1_34_etoh[, c(1,2,3)]
T1_34_etoh_july1_bins <- T1_34_etoh_july1[which(rowSums(T1_34_etoh_july1[2:3]) >0) , 1]

t134_m1m2 <- length(intersect(T1_34_etoh_may1_bins, T1_34_etoh_may2_bins))  # 40
t134_m1j1 <- length(intersect(T1_34_etoh_may1_bins, T1_34_etoh_june1_bins)) # 30
t134_m1j2 <- length(intersect(T1_34_etoh_may1_bins, T1_34_etoh_june2_bins)) # 41
t134_m1jl1 <-length(intersect(T1_34_etoh_may1_bins, T1_34_etoh_july1_bins)) # 38

plot(c(t134_m1m2,t134_m1j1,t134_m1j2,t134_m1jl1))
title(main="T1-34B Ethanol overlap in BINs")

############################
grepl("T1_52", namesetoh)
T1_52_etoh <- cbind.data.frame(bindata_etoh[1], bindata_etoh[, grepl("T1_52", names(bindata_etoh))])
names(T1_52_etoh)
T1_52_etoh[, c(1,6,7,10,11,4,5,8,9,2,3)]
T1_52_etoh_may <- T1_52_etoh[, c(1,6,7,10,11)]
T1_52_etoh_may1 <- T1_52_etoh[, c(1,6,7)]
T1_52_etoh_may2 <- T1_52_etoh[, c(1,10,11)]
T1_52_etoh_may1_bins <- T1_52_etoh_may1[which(rowSums(T1_52_etoh_may1[2:3])>0), 1]
T1_52_etoh_may2_bins <- T1_52_etoh_may2[which(rowSums(T1_52_etoh_may2[2:3])>0), 1]

T1_52_etoh_june <- T1_52_etoh[, c(1,4,5,8,9)]
T1_52_etoh_june1 <- T1_52_etoh[, c(1,4,5)]
T1_52_etoh_june2 <- T1_52_etoh[, c(1,8,9)]
T1_52_etoh_june1_bins <- T1_52_etoh_june1[which(rowSums(T1_52_etoh_june1[2:3]) >0), 1]
T1_52_etoh_june2_bins <- T1_52_etoh_june2[which(rowSums(T1_52_etoh_june2[2:3]) >0), 1]

T1_52_etoh_july <- T1_52_etoh[, c(1,2,3)]
T1_52_etoh_july1 <- T1_52_etoh[, c(1,2,3)]
T1_52_etoh_july1_bins <- T1_52_etoh_july1[which(rowSums(T1_52_etoh_july1[2:3]) >0) , 1]

t152_m1m2<- length(intersect(T1_52_etoh_may1_bins, T1_52_etoh_may2_bins))  # 37
t152_m1j1<- length(intersect(T1_52_etoh_may1_bins, T1_52_etoh_june1_bins)) # 35
t152_m1j2<- length(intersect(T1_52_etoh_may1_bins, T1_52_etoh_june2_bins)) # 42
t152_m1jl1<-length(intersect(T1_52_etoh_may1_bins, T1_52_etoh_july1_bins)) # 16
plot(c(t152_m1m2,t152_m1j1,t152_m1j2,t152_m1jl1))
title(main="T1-52B Ethanol overlap in BINs")

##########################
grepl("T1_63", namesetoh)
T1_63_etoh <- cbind.data.frame(bindata_etoh[1], bindata_etoh[, grepl("T1_63", names(bindata_etoh))])
names(T1_63_etoh)
T1_63_etoh[, c(1,6,7,10,11,4,5,8,9,2,3)]
T1_63_etoh_may <- T1_63_etoh[, c(1,6,7,10,11)]
T1_63_etoh_may1 <- T1_63_etoh[, c(1,6,7)]
T1_63_etoh_may2 <- T1_63_etoh[, c(1,10,11)]
T1_63_etoh_may1_bins <- T1_63_etoh_may1[which(rowSums(T1_63_etoh_may1[2:3])>0), 1]
T1_63_etoh_may2_bins <- T1_63_etoh_may2[which(rowSums(T1_63_etoh_may2[2:3])>0), 1]

T1_63_etoh_june <- T1_63_etoh[, c(1,4,5,8,9)]
T1_63_etoh_june1 <- T1_63_etoh[, c(1,4,5)]
T1_63_etoh_june2 <- T1_63_etoh[, c(1,8,9)]
T1_63_etoh_june1_bins <- T1_63_etoh_june1[which(rowSums(T1_63_etoh_june1[2:3]) >0), 1]
T1_63_etoh_june2_bins <- T1_63_etoh_june2[which(rowSums(T1_63_etoh_june2[2:3]) >0), 1]

T1_63_etoh_july <- T1_63_etoh[, c(1,2,3)]
T1_63_etoh_july1 <- T1_63_etoh[, c(1,2,3)]
T1_63_etoh_july1_bins <- T1_63_etoh_july1[which(rowSums(T1_63_etoh_july1[2:3]) >0) , 1]

t163_m1m2<- length(intersect(T1_63_etoh_may1_bins, T1_63_etoh_may2_bins))  # 35
t163_m1j1<- length(intersect(T1_63_etoh_may1_bins, T1_63_etoh_june1_bins)) # 41
t163_m1j2<- length(intersect(T1_63_etoh_may1_bins, T1_63_etoh_june2_bins)) # 40
t163_m1jl1<-length(intersect(T1_63_etoh_may1_bins, T1_63_etoh_july1_bins)) # 35
plot(c(t163_m1m2,t163_m1j1,t163_m1j2,t163_m1jl1))
title(main="T1-63B Ethanol overlap in BINs")

#########################
grepl("T3_50", namesetoh)
T3_50_etoh <- cbind.data.frame(bindata_etoh[1], bindata_etoh[, grepl("T3_50", names(bindata_etoh))])
names(T3_50_etoh)
T3_50_etoh[, c(1,6,7,10,11,4,5,8,9,2,3)] ##Naming problem in column 8?
T3_50_etoh_may <- T3_50_etoh[, c(1,6,7,10,11)]
T3_50_etoh_may1 <- T3_50_etoh[, c(1,6,7)]
T3_50_etoh_may2 <- T3_50_etoh[, c(1,10,11)]
T3_50_etoh_may1_bins <- T3_50_etoh_may1[which(rowSums(T3_50_etoh_may1[2:3])>0), 1]
T3_50_etoh_may2_bins <- T3_50_etoh_may2[which(rowSums(T3_50_etoh_may2[2:3])>0), 1]

#T3_50_etoh_june <- T3_50_etoh[, c(1,4,5,8,9)]
#T3_50_etoh_june1 <- T3_50_etoh[, c(1,4,5)]
#T3_50_etoh_june2 <- T3_50_etoh[, c(1,8,9)]
#T3_50_etoh_june1_bins <- T3_50_etoh_june1[which(rowSums(T3_50_etoh_june1[2:3]) >0), 1]
#T3_50_etoh_june2_bins <- T3_50_etoh_june2[which(rowSums(T3_50_etoh_june2[2:3]) >0), 1]

T3_50_etoh_july <- T3_50_etoh[, c(1,2,3)]
T3_50_etoh_july1 <- T3_50_etoh[, c(1,2,3)]
T3_50_etoh_july1_bins <- T3_50_etoh_july1[which(rowSums(T3_50_etoh_july1[2:3]) >0) , 1]

t350_m1m2<- length(intersect(T3_50_etoh_may1_bins, T3_50_etoh_may2_bins))  # 43
t350_m1j1<-length(intersect(T3_50_etoh_may1_bins, T3_50_etoh_june1_bins)) #
t350_m1j2<-length(intersect(T3_50_etoh_may1_bins, T3_50_etoh_june2_bins)) #
t350_m1jl1<-length(intersect(T3_50_etoh_may1_bins, T3_50_etoh_july1_bins)) # 46
plot(c(t350_m1m2,t350_m1jl1))
title(main="T3-50B Ethanol overlap in BINs")

########################
grepl("T4_64", namesetoh)
T4_64_etoh <- cbind.data.frame(bindata_etoh[1], bindata_etoh[, grepl("T4_64", names(bindata_etoh))])
names(T4_64_etoh)
T4_64_etoh[, c(1,8,9,2,3,6,7,10,11,4,5)]
T4_64_etoh_may <- T4_64_etoh[, c(1,8,9,2,3)]
T4_64_etoh_may1 <- T4_64_etoh[, c(1,8,9)]
T4_64_etoh_may2 <- T4_64_etoh[, c(1,2,3)]
T4_64_etoh_may1_bins <- T4_64_etoh_may1[which(rowSums(T4_64_etoh_may1[2:3])>0), 1]
T4_64_etoh_may2_bins <- T4_64_etoh_may2[which(rowSums(T4_64_etoh_may2[2:3])>0), 1]

T4_64_etoh_june <- T4_64_etoh[, c(1,6,7,10,11)]
T4_64_etoh_june1 <- T4_64_etoh[, c(1,6,7)]
T4_64_etoh_june2 <- T4_64_etoh[, c(1,10,11)]
T4_64_etoh_june1_bins <- T4_64_etoh_june1[which(rowSums(T4_64_etoh_june1[2:3]) >0), 1]
T4_64_etoh_june2_bins <- T4_64_etoh_june2[which(rowSums(T4_64_etoh_june2[2:3]) >0), 1]

T4_64_etoh_july <- T4_64_etoh[, c(1,4,5)]
T4_64_etoh_july1 <- T4_64_etoh[, c(1,4,5)]
T4_64_etoh_july1_bins <- T4_64_etoh_july1[which(rowSums(T4_64_etoh_july1[2:3]) >0) , 1]

t464_m1m2<- length(intersect(T4_64_etoh_may1_bins, T4_64_etoh_may2_bins))  # 56
t464_m1j1<- length(intersect(T4_64_etoh_may1_bins, T4_64_etoh_june1_bins)) # 54
t464_m1j2<- length(intersect(T4_64_etoh_may1_bins, T4_64_etoh_june2_bins)) # 54
t464_m1jl1<-length(intersect(T4_64_etoh_may1_bins, T4_64_etoh_july1_bins)) # 54
plot(c(t464_m1m2,t464_m1j1,t464_m1j2,t464_m1jl1))
title(main="T4-64B Ethanol overlap in BINs")

#grepl("T1_64", namesetoh)

write.table(bindata_etoh, file="bindata_etoh.tsv", sep="\t", row.names = F)


may_etoh[, grepl("IGG", names(may_etoh))]

# For Ethanol BINs, same ^.

# Plot the...percent differences?
plot(c(igg_m1m2,igg_m1j1,igg_m1j2,igg_m1jl1),type="l")
lines(c(jos_m1j1,jos_m1j1,jos_m1j2,jos_m1jl1))
lines(c(sal_m1m2,sal_m1j1,sal_m1j2,sal_m1jl1))
lines(c(t102_m1m2,t102_m1j1,t102_m1j2,t102_m1jl1))
lines(c(t134_m1m2,t134_m1j1,t134_m1j2,t134_m1jl1))
lines(c(t152_m1m2,t152_m1j1,t152_m1j2,t152_m1jl1))
lines(c(t163_m1m2,t163_m1j1,t163_m1j2,t163_m1jl1))
#lines(c(t350_m1m2,t350_m1jl1))
lines(c(t464_m1m2,t464_m1j1,t464_m1j2,t464_m1jl1))
title(main="Overlap of BINs in Ethanol")

# For BioVenn:
write.table(as.vector(igg_etoh_may1_bins), file="igg_etoh_may1_bins.txt", quote=F, row.names=F)
write.table(na.omit(as.vector(igg_etoh_may2_bins)), file="igg_etoh_may2_bins.txt", quote=F, row.names=F)
write.table(na.omit(as.vector(igg_etoh_june1_bins)), file="igg_etoh_june1_bins.txt", quote=F, row.names=F)
write.table(na.omit(as.vector(igg_etoh_june2_bins)), file="igg_etoh_june2_bins.txt", quote=F, row.names=F)
write.table(na.omit(as.vector(igg_etoh_july1_bins)), file="igg_etoh_july1_bins.txt", quote=F, row.names=F)

#By pct:
p1<-length(intersect(igg_etoh_may1_bins, igg_etoh_may2_bins))/length(igg_etoh_may2_bins)
p2<-length(intersect(igg_etoh_may1_bins, igg_etoh_june1_bins))/length(igg_etoh_june1_bins)
p3<-length(intersect(igg_etoh_may1_bins, igg_etoh_june2_bins))/length(igg_etoh_june2_bins)
p4<-length(intersect(igg_etoh_may1_bins, igg_etoh_july1_bins))/length(igg_etoh_july1_bins)
plot(c(p1,p2,p3,p4),type="l")

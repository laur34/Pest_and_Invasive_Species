powdETOH <- read.table("EtOH_vs_Powder_famETOH.tsv", header=T, sep="\t", stringsAsFactors = F)
arth <- powdETOH[powdETOH$Phylum=="Arthropoda", ]
## Here, we want to keep not just the family column, but other variables, such as BIN and Order.
famBINord <- arth[, c(4,5,9,10,15:ncol(arth))]
#famBINord <- arth[, c(4,5,9,15:ncol(arth))]
famBINord <- famBINord[, 1:92] #we'll leave out semilyis for now.
#famBINord <- famBINord[, 1:91] #we'll leave out semilyis for now.
famBINordETOH <- cbind.data.frame(famBINord[, c(1:4)], famBINord[ ,grepl("ETOH", names(famBINord))] )
#famBINordETOH <- cbind.data.frame(famBINord[, c(1:3)], famBINord[ ,grepl("ETOH", names(famBINord))] )
famBINordPowd <- famBINord[, !grepl("ETOH", names(famBINord))]
# Need to collapse multiple occurrences of BINs into single rows.
# For this, need a single df, or else row numbers could change and two won't recombine.
write.table(famBINord, file="famBINord_powd_eth.tsv", quote=F, row.names = F, sep="\t")
## Sort it by BIN, and then descending by percent identity:
famBINord <- famBINord[order(famBINord$BIN , -famBINord$Identity), ]
## Combine:
attach(famBINord)
binsuniq <- aggregate(famBINord[,5:ncol(famBINord)], by=list(BIN), FUN=sum, na.rm=T)
## That worked ^ except it got rid of the family and order columns. They can be re-added in LO / BOLD, though.
write.table(binsuniq, file="BIN_powd_eth.tsv", quote=F, row.names = F, sep="\t")
## Import that into LO, vlookup d/l's from BOLD to add Family and Order data, name it BIN_powd_eth_BOLD.ods
## Imort the new tsv from it:
bins_ord_fam <- read.table("BIN_powd_eth_BOLD.tsv", header=T, sep="\t", stringsAsFactors = F)
# Change numbers >0 to 1 (TRUE or FALSE, detected)
bins_ord_fam[,-c(1:3)] = (bins_ord_fam[,-c(1:3)] !=0)*1

# Get rid of the ones where row sums are 0 all across -- BIN detected neither in eth nor jeweilige powders:
bins_ord_fam <- bins_ord_fam[rowSums(bins_ord_fam[4:47])>0, ]

# Now break it down into powder and ethanol:
bins_etoh <- cbind.data.frame(bins_ord_fam[1:3], bins_ord_fam[, grepl("ETOH", names(bins_ord_fam))])
bins_powd <- bins_ord_fam[, !grepl("ETOH", names(bins_ord_fam))]
# Take the row sums of the BINS, and also for the
sum(rowSums(bins_etoh[4:47])>0) #425
sum(rowSums(bins_powd[4:47])>0) #2038 These numbers changed.


etoh_sum_tbl <- cbind.data.frame(bins_etoh[1:3], rowSums(bins_etoh[4:47]))
powd_sum_tbl <- cbind.data.frame(bins_powd[1:3], rowSums(bins_powd[4:47]))
tbl <- cbind.data.frame(etoh_sum_tbl, powd_sum_tbl[,3:4])
### sort by orders and then by families:
tbl <- tbl[, !duplicated(colnames(tbl))]
tbl[order(tbl$Order, tbl$Family),]
## export this, and also,
write.table(tbl, file="etoh_powd_sums.tsv", quote=F, row.names=F, sep="\t" )
## group by families (see line 73)

######################## (With just the family and data:)
fams <- arth[, c(5,15:ncol(arth))]
ncol(fams) #94
fams <- fams[, 1:89]
famsETOH <- cbind.data.frame(fams[1], fams[ ,grepl("ETOH", names(fams))])
famsPowd <- fams[ , !grepl("ETOH", names(fams))]

sum(rowSums(famsPowd[2:45]) >0)  #2197
sum(rowSums(famsETOH[2:45]) >0)  #583
# Create df's for powder and ethanol, of whether each row was detected in at least one sample:
famspowdtf <- cbind.data.frame(famsPowd[1], as.vector(rowSums(famsPowd[2:45])>0) )
famsetohtf <- cbind.data.frame(famsETOH[1], as.vector(rowSums(famsETOH[2:45])>0) )
#write.table(famspowdtf, file="families_powder_yes_no", quote=F, row.names=F, sep="\t")
#write.table(famsetohtf, file="families_etoh_yes_no", quote=F, row.names=F, sep="\t")

### Do stuff in LO. ###
families <- read.table("families_powder_etoh_yes_no.csv", header=T, sep="\t", stringsAsFactors = F)
table(families)
by(families$powder, families$FamilyVL, sum)
by(families$ethanol, families$FamilyVL, sum)
tapply(families$powder, families$FamilyVL, sum)
tapply(families$ethanol, families$FamilyVL, sum)
#aggregate
attach(families)
famdata <- aggregate(families$powder, by=list(FamilyVL), FUN=sum, na.rm=T)

library(dplyr)
families %>%
  group_by(FamilyVL) %>%
  summarize(sum_detectedp = sum(powder, na.rm=T),
            sum_detecedeth = sum(ethanol, na.rm=T))

detach(families)


######## just playing:
#Geo <- subset(fams, fams$FamilyVL=="Geometridae")
Geo_powder <- subset(famsPowd, famsPowd$FamilyVL=="Geometridae")
rowSums(Geo_powder[2:45])
rowSums(Geo_powder[2:45])>0
sum(rowSums(Geo_powder[2:45])>0) #33
Geo_etoh <- subset(famsETOH, famsETOH$FamilyVL=="Geometridae")
rowSums(Geo_etoh[2:45])
rowSums(Geo_etoh[2:45])>0
sum(rowSums(Geo_etoh[2:45])>0) #2

Tortricidae_powder <- subset(famsPowd, famsPowd$FamilyVL=="Tortricidae")
sum(rowSums(Tortricidae_powder[2:45])>0)  #39
Tortricidae_etoh <- subset(famsETOH, famsETOH$FamilyVL=="Tortricidae")
sum(rowSums(Tortricidae_etoh[2:45])>0)  #5

Erebidae_powder <- subset(famsPowd, famsPowd$FamilyVL=="Erebidae")
sum(rowSums(Erebidae_powder[2:45])>0) #18
Erebidae_etoh <- subset(famsETOH, famsETOH$FamilyVL=="Erebidae")
sum(rowSums(Erebidae_etoh[2:45])>0) #3


#Diptera
Phoridae_powder <- subset(famsPowd, famsPowd$FamilyVL=="Phoridae")
sum(rowSums(Phoridae_powder[2:45])>0) #110
Phoridae_etoh <- subset(famsETOH, famsETOH$FamilyVL=="Phoridae")
sum(rowSums(Phoridae_etoh[2:45])>0)  #69






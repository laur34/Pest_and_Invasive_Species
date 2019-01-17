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

#Rename columns to be easier to take only the ones needed:
write.table(names(bindata), file="PowderVsEtOH_names.txt", quote=F, row.names=F)

## In gedit, rename manually with Find & Replace. ##
##  Save as ..._new.txt ##
# Read in new names and rename df:
newcolnames <- read.table("PowderVsEtOH_names_new.txt", header=F, stringsAsFactors=F)
newcolnames <- as.vector(newcolnames$V1)
names(bindata) <- newcolnames

#keep only the ones for which we have data for homogenate AND ethanol.
##First subset to 2018, since we only have ethanol for that year:
bindata2018 <- bindata[,c(1,92:306)]

#which(grepl("Igg_35B_1LJuli", names(bindata2018)))
oneswith <- read.table("oneswithetoh.txt", header=F, stringsAsFactors = F)
oneswith <- unique(as.vector(oneswith$V1))

colnums <- vector()

for(i in 1:length(oneswith)){
  colnums <- append(colnums, (which(grepl(oneswith[i], names(bindata2018)))))
}

withEtOH <- bindata2018[,c(1,colnums)]
#write.table(withEtOH, file="EtOH_and_Powders.tsv", sep="\t", row.names = F)

newEtOhTable <- cbind.data.frame(data[,c(1:8,10:14)], withEtOH)
write.table(newEtOhTable, file="EtOH_and_Powders.tsv", sep="\t", row.names=F)

# Combine Homogenate, Ethanol, and (semilysis) by summing these sets of columns
# can also do this in Excel.

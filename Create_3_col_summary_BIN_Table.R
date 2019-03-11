# Create 3-column table of BINs for 2016 and 2018 homogenized samples,
#and overlaps (BINs in common between the 2 yrs).
setwd("/media/laur/wdhdd1/allNPBW/")
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
data <- data[, !grepl("X2018_NPBWpowder", names(data))]
names(data)
# Make it presence-absence
data[,14:ncol(data)] <- (data[,14:ncol(data)] >0)*1
# Create sum columns for 2016 and 2018:
data$x2016_sums <- rowSums(data[14:103])
#There are still two unidentifiable sample names, so skip them here
data$x2018_sums <- rowSums(data[106:194])

datanew <- cbind.data.frame(data[1:13],data[195:196])

# Aggregate to combine multiple occurrences of some BINs:
aggBIN <- aggregate(datanew[14:15], by=list(datanew$BIN), FUN=sum)
# That left only those three columns. 
# Make it pres-abs again, so that each BIN will be present or absent in each year:
aggBIN[2:3] <- (aggBIN[2:3] >0)*1
head(aggBIN)
# Create an "Overlap" column:
aggBIN$Overlap <- (aggBIN[2] > 0) & (aggBIN[3] > 0)
aggBIN$Overlap <- as.integer(aggBIN$Overlap)

# Add back in the Phylum, Class, and Order information (based on BIN)
#write.table(aggBIN, file="for_Table1_3cols_aggBIN.csv", quote=F, sep=",", row.names=F)
# LO - VLookup, Sort by Phylum, Class, Order, import new file.
datanew1 <- read.table("for_Table1_3cols_aggBIN_with_Taxa.csv", header=T, sep=",", stringsAsFactors = F)
head(datanew1)
# Calculate the sums of BINs per order in each year and pct overlap.
# First get rid of rows of all 0:
datanew1 <- datanew1[which(rowSums(datanew1[5:6])>0), ]

sum(datanew1[which(datanew1$Order=="Haplotaxida"),5])
sum(datanew1[which(datanew1$Order=="Haplotaxida"),6])
sum(datanew1[which(datanew1$Order=="Haplotaxida"),7]) / nrow(datanew1[which(datanew1$Order=="Haplotaxida"),])

#Make new table of what we want.
#Create a 2016 column:
x2016 <- sum(datanew1[which(datanew1$Order=="Haplotaxida"),5])
#2018:
x2018 <- sum(datanew1[which(datanew1$Order=="Haplotaxida"),6])
#Overlap:
overlap <- sum(datanew1[which(datanew1$Order=="Haplotaxida"),7]) / nrow(datanew1[which(datanew1$Order=="Haplotaxida"),])

#Add other Orders:
x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Araneae"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Araneae"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Araneae"),7]) / nrow(datanew1[which(datanew1$Order=="Araneae"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Ixodida"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Ixodida"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Ixodida"),7]) / nrow(datanew1[which(datanew1$Order=="Ixodida"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Mesostigmata"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Mesostigmata"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Mesostigmata"),7]) / nrow(datanew1[which(datanew1$Order=="Mesostigmata"),]))

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Opiliones"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Opiliones"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Opiliones"),7]) / nrow(datanew1[which(datanew1$Order=="Opiliones"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Pseudoscorpiones"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Pseudoscropiones"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Pseudoscorpiones"),7]) / nrow(datanew1[which(datanew1$Order=="Pseudoscorpiones"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Sarcoptiformes"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Sarcoptiformes"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Sarcoptiformes"),7]) / nrow(datanew1[which(datanew1$Order=="Sarcoptiformes"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Trombidiformes"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Trombidiformes"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Trombidiformes"),7]) / nrow(datanew1[which(datanew1$Order=="Trombidiformes"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Entomobryomorpha"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Entomobryomorpha"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Entomobryomorpha"),7]) / nrow(datanew1[which(datanew1$Order=="Entomobryomorpha"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Symphypleona"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Symphypleona"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Symphypleona"),7]) / nrow(datanew1[which(datanew1$Order=="Symphypleona"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Polyxenida"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Polyxenida"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Polyxenida"),7]) / nrow(datanew1[which(datanew1$Order=="Polyxenida"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Archaeognatha"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Archaeognatha"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Archaeognatha"),7]) / nrow(datanew1[which(datanew1$Order=="Archaeognatha"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Blattodea"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Blattodea"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Blattodea"),7]) / nrow(datanew1[which(datanew1$Order=="Blattodea"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Coleoptera"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Coleoptera"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Coleoptera"),7]) / nrow(datanew1[which(datanew1$Order=="Coleoptera"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Dermaptera"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Dermaptera"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Dermaptera"),7]) / nrow(datanew1[which(datanew1$Order=="Dermaptera"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Diptera"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Diptera"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Diptera"),7]) / nrow(datanew1[which(datanew1$Order=="Diptera"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Ephemeroptera"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Ephemeroptera"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Ephemeroptera"),7]) / nrow(datanew1[which(datanew1$Order=="Ephemeroptera"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Hemiptera"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Hemiptera"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Hemiptera"),7]) / nrow(datanew1[which(datanew1$Order=="Hemiptera"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Hymenoptera"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Hymenoptera"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Hymenoptera"),7]) / nrow(datanew1[which(datanew1$Order=="Hymenoptera"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Lepidoptera"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Lepidoptera"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Lepidoptera"),7]) / nrow(datanew1[which(datanew1$Order=="Lepidoptera"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Mercoptera"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Mercoptera"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Mercoptera"),7]) / nrow(datanew1[which(datanew1$Order=="Mercoptera"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Neuroptera"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Neuroptera"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Neuroptera"),7]) / nrow(datanew1[which(datanew1$Order=="Neuroptera"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Odonata"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Odonata"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Odonata"),7]) / nrow(datanew1[which(datanew1$Order=="Odonata"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Orthoptera"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Orthoptera"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Orthoptera"),7]) / nrow(datanew1[which(datanew1$Order=="Orthoptera"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Plecoptera"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Plecoptera"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Plecoptera"),7]) / nrow(datanew1[which(datanew1$Order=="Plecoptera"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Psocodea"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Psocodea"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Psocodea"),7]) / nrow(datanew1[which(datanew1$Order=="Psocodea"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Raphidioptera"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Raphidioptera"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Raphidioptera"),7]) / nrow(datanew1[which(datanew1$Order=="Raphidioptera"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Thysanoptera"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Thysanoptera"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Thysanoptera"),7]) / nrow(datanew1[which(datanew1$Order=="Thysanoptera"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Trichoptera"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Trichoptera"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Trichoptera"),7]) / nrow(datanew1[which(datanew1$Order=="Trichoptera"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Isopoda"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Isopoda"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Isopoda"),7]) / nrow(datanew1[which(datanew1$Order=="Isopoda"),]) )

x2016 <- append(x2016, sum(datanew1[which(datanew1$Order=="Stylommatophora"),5]) )
x2018 <- append(x2018, sum(datanew1[which(datanew1$Order=="Stylommatophora"),6]) )
overlap <- append(overlap, sum(datanew1[which(datanew1$Order=="Stylommatophora"),7]) / nrow(datanew1[which(datanew1$Order=="Stylommatophora"),]) )
#
length(x2016)
length(x2018)
length(overlap)
length(unique(datanew1$Order))

datanew2 <- cbind.data.frame(unique(datanew1$Order), x2016, x2018, overlap)

setwd("/media/laur/wdhdd1/allNPBW/R_output_new/")
#write.table(datanew2, file="Table1_New_Summary_R5_R.tsv", quote=F, row.names=F, sep="\t")

#Create a PCOA or NM plot comparing sites with species.
#install.packages("remotes")
library(remotes)
#remotes::install_github("MadsAlbertsen/ampvis2")
library(ampvis2)
#Load data for ampvis functions, like example:
#amp_load(otutable, metadata = NULL, fasta = NULL, tree = NULL)
#Create appropriate OTU table from current version of data:
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
# Tissue powders only:
data <- data[, !grepl("_224", names(data))]
data <- data[, !grepl("_210", names(data))]
data <- data[, !grepl("Etoh", names(data))]
data <- data[, !grepl("semi", names(data))]
data <- data[, !grepl("filter", names(data))]
head(data)
#The rows are OTU IDs, and the cols are samples:
#(see https://madsalbertsen.github.io/ampvis2/reference/amp_load.html)
df <- data[13:ncol(data)]
#The OTU ID's are expected to be in eiher the rownames of the data frame or in a column called "OTU". 
colnames(df)[1] <- "OTU"
#The last 7 columns are the corresponding taxonomy assigned to the OTUs, named "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species".
df$Kingdom <- rep("Animalia", length(df$OTU))
otutbl <- cbind.data.frame(df, data$Phylum, data$Class, data$Order, data$Family, data$Genus, data$Species)
colnames(otutbl)[183:188] <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")

#2016 first:
otutbl2016 <- otutbl[,  !grepl("2018", names(otutbl))]
head(otutbl2016)

# Combine the data with amp_load() to make it compatible with ampvis2 functions.
d <- amp_load(otutable=otutbl)
d
#Use a custom distance matrix
# see https://madsalbertsen.github.io/ampvis2/reference/amp_ordinate.html
# oops I can't because this uses species, etc. and my matrix from before was all BINs.

amp_ordinate(d, type="PCA", transform="hellinger", sample_color_by = "DummyVariable", sample_colorframe = T)

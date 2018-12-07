setwd("/media/laur/wdhdd1/GBOL9_2018/fwd/2018Powderv2016/NPBW2yrsPowders/")
data <- read.table(file="Results_table_NPBW2yrsPowder-AllApril2018BINs-Megablast_speciesforR.csv", header=T, sep=",", stringsAsFactors = F)
head(data)

data$Species[data$Species == ""] <- NA
data$Family[data$Family == ""] <- NA
data$Subfamily[data$Subfamily == ""] <- NA
data$Genus[data$Genus == ""] <- NA
data$BIN[data$BIN == ""] <- NA

colnames(data)
x2016 <- data[,1:103]
x2018 <- data[,c(1:13,104:ncol(data))]

################################### 2106 ##############################################
############### May ######################
May2016 <- cbind.data.frame(x2016[ , c(2,5,9)], x2016[ , grepl("Mai", names(x2016))])
#Make May2016 sub-data frames by trap:
####### Igg-35B ############
May2016Igg <- cbind.data.frame(May2016[,1:3], May2016[, grepl("Igg", names(May2016))] )
#Filter to keep rows with Wert > 0 (are present):
May2016Igg <- May2016Igg[which(rowSums(May2016Igg[4:5])>0), ]
#Make into lists:
nrow(May2016Igg)
rep("May 2016", nrow(May2016Igg))

df <- cbind.data.frame(May2016Igg[2:3], rep("May 2016", nrow(May2016Igg)), rep("Igg-35B", nrow(May2016Igg)))

write.table(df, file="May_2016_Igg-35B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Jos-21 ############
May2016Jos <- cbind.data.frame(May2016[,1:3], May2016[, grepl("Jos", names(May2016))] )
#Filter to keep rows with Wert > 0 (are present):
May2016Jos <- May2016Jos[which(rowSums(May2016Jos[4:5])>0), ]
#Make into lists:
nrow(May2016Jos)

df <- cbind.data.frame(May2016Jos[2:3], rep("May 2016", nrow(May2016Jos)), rep("Jos-21", nrow(May2016Jos)))

write.table(df, file="May_2016_Jos-21_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Sal-25 ############
May2016Sal <- cbind.data.frame(May2016[,1:3], May2016[, grepl("Sal", names(May2016))] )
#Filter to keep rows with Wert > 0 (are present):
May2016Sal <- May2016Sal[which(rowSums(May2016Sal[4:5])>0), ]
#Make into lists:
nrow(May2016Sal)

df <- cbind.data.frame(May2016Sal[2:3], rep("May 2016", nrow(May2016Sal)), rep("Sal-25", nrow(May2016Sal)))

write.table(df, file="May_2016_Sal-25_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-02B ############
May2016T102B <- cbind.data.frame(May2016[,1:3], May2016[, grepl("T1_02B", names(May2016))] )
#Filter to keep rows with Wert > 0 (are present):
May2016T102B <- May2016T102B[which(rowSums(May2016T102B[4:5])>0), ]
#Make into lists:
nrow(May2016T102B)

df <- cbind.data.frame(May2016T102B[2:3], rep("May 2016", nrow(May2016T102B)), rep("T1-02B", nrow(May2016T102B)))

write.table(df, file="May_2016_T1-02B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")


####### T1-34B ############
May2016T134B <- cbind.data.frame(May2016[,1:3], May2016[, grepl("T1_34B", names(May2016))] )
nrow(May2016T134B)
#Filter to keep rows with Wert > 0 (are present):
May2016T134B <- May2016T134B[which(rowSums(May2016T134B[4:5])>0), ]
#Make into lists:
nrow(May2016T134B)

df <- cbind.data.frame(May2016T134B[2:3], rep("May 2016", nrow(May2016T134B)), rep("T1-34B", nrow(May2016T134B)))

write.table(df, file="May_2016_T1-34B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")


####### T1-52B ############
May2016T152B <- cbind.data.frame(May2016[,1:3], May2016[, grepl("T1_52B", names(May2016))] )
nrow(May2016T152B)
#Filter to keep rows with Wert > 0 (are present):
May2016T152B <- May2016T152B[which(rowSums(May2016T152B[4:5])>0), ]
#Make into lists:
nrow(May2016T152B)

df <- cbind.data.frame(May2016T152B[2:3], rep("May 2016", nrow(May2016T152B)), rep("T1-52B", nrow(May2016T152B)))

write.table(df, file="May_2016_T1-52B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-63B ############
May2016T163B <- cbind.data.frame(May2016[,1:3], May2016[, grepl("T1_63B", names(May2016))] )
nrow(May2016T163B)
#Filter to keep rows with Wert > 0 (are present):
May2016T163B <- May2016T163B[which(rowSums(May2016T163B[4:5])>0), ]
#Make into lists:
nrow(May2016T163B)

df <- cbind.data.frame(May2016T163B[2:3], rep("May 2016", nrow(May2016T163B)), rep("T1-63B", nrow(May2016T163B)))

write.table(df, file="May_2016_T1-63B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T4-64B ############
May2016T464B <- cbind.data.frame(May2016[,1:3], May2016[, grepl("64B", names(May2016))] )
nrow(May2016T464B)
#Filter to keep rows with Wert > 0 (are present):
May2016T464B <- May2016T464B[which(rowSums(May2016T464B[4:5])>0), ]
#Make into lists:
nrow(May2016T464B)

df <- cbind.data.frame(May2016T464B[2:3], rep("May 2016", nrow(May2016T464B)), rep("T4-64B", nrow(May2016T464B)))

write.table(df, file="May_2016_T4-64B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T3-50B ############
May2016T350B <- cbind.data.frame(May2016[,1:3], May2016[, grepl("T3_50B", names(May2016))] )
nrow(May2016T350B)
#Filter to keep rows with Wert > 0 (are present):
May2016T350B <- May2016T350B[which(rowSums(May2016T350B[4:5])>0), ]
#Make into lists:
nrow(May2016T350B)

df <- cbind.data.frame(May2016T350B[2:3], rep("May 2016", nrow(May2016T350B)), rep("T3-50B", nrow(May2016T350B)))

write.table(df, file="May_2016_T3-50B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")


############### June ######################
June2016 <- cbind.data.frame(x2016[ , c(2,5,9)], x2016[ , grepl("Juni", names(x2016))])
#Make June2016 sub-data frames by trap:
####### Igg-35B ############
June2016Igg <- cbind.data.frame(June2016[,1:3], June2016[, grepl("Igg", names(June2016))] )
nrow(June2016Igg)
#Filter to keep rows with Wert > 0 (are present):
June2016Igg <- June2016Igg[which(rowSums(June2016Igg[4:5])>0), ]
nrow(June2016Igg)
#Make into lists:
df <- cbind.data.frame(June2016Igg[2:3], rep("June 2016", nrow(June2016Igg)), rep("Igg-35B", nrow(June2016Igg)))

write.table(df, file="June_2016_Igg-35B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Jos-21 ############
June2016Jos <- cbind.data.frame(June2016[,1:3], June2016[, grepl("Jos", names(June2016))] )
nrow(June2016Jos)
#Filter to keep rows with Wert > 0 (are present):
June2016Jos <- June2016Jos[which(rowSums(June2016Jos[4:5])>0), ]
nrow(June2016Jos)
#Make into lists:
df <- cbind.data.frame(June2016Jos[2:3], rep("June 2016", nrow(June2016Jos)), rep("Jos-21", nrow(June2016Jos)))

write.table(df, file="June_2016_Jos-21_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Sal-25 ############
June2016Sal <- cbind.data.frame(June2016[,1:3], June2016[, grepl("Sal", names(June2016))] )
#Filter to keep rows with Wert > 0 (are present):
June2016Sal <- June2016Sal[which(rowSums(June2016Sal[4:5])>0), ]
nrow(June2016Sal)
#Make into lists:
df <- cbind.data.frame(June2016Sal[2:3], rep("June 2016", nrow(June2016Sal)), rep("Sal-25", nrow(June2016Sal)))

write.table(df, file="June_2016_Sal-25_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-02B ############
June2016T102B <- cbind.data.frame(June2016[,1:3], June2016[, grepl("T1_02B", names(June2016))] )
nrow(June2016T102B)
#Filter to keep rows with Wert > 0 (are present):
June2016T102B <- June2016T102B[which(rowSums(June2016T102B[4:5])>0), ]
nrow(June2016T102B)
#Make into lists:
df <- cbind.data.frame(June2016T102B[2:3], rep("June 2016", nrow(June2016T102B)), rep("T1-02B", nrow(June2016T102B)))

write.table(df, file="June_2016_T1-02B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-34B ############
June2016T134B <- cbind.data.frame(June2016[,1:3], June2016[, grepl("T1_34B", names(June2016))] )
nrow(June2016T134B)
#Filter to keep rows with Wert > 0 (are present):
June2016T134B <- June2016T134B[which(rowSums(June2016T134B[4:5])>0), ]
#Make into lists:
nrow(June2016T134B)

df <- cbind.data.frame(June2016T134B[2:3], rep("June 2016", nrow(June2016T134B)), rep("T1-34B", nrow(June2016T134B)))

write.table(df, file="June_2016_T1-34B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-52B ############
June2016T152B <- cbind.data.frame(June2016[,1:3], June2016[, grepl("T1_52B", names(June2016))] )
nrow(June2016T152B)
#Filter to keep rows with Wert > 0 (are present):
June2016T152B <- June2016T152B[which(rowSums(June2016T152B[4:5])>0), ]
#Make into lists:
nrow(June2016T152B)

df <- cbind.data.frame(June2016T152B[2:3], rep("June 2016", nrow(June2016T152B)), rep("T1-52B", nrow(June2016T152B)))

write.table(df, file="June_2016_T1-52B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-63B ############
June2016T163B <- cbind.data.frame(June2016[,1:3], June2016[, grepl("T1_63B", names(June2016))] )
nrow(June2016T163B)
#Filter to keep rows with Wert > 0 (are present):
June2016T163B <- June2016T163B[which(rowSums(June2016T163B[4:5])>0), ]
#Make into lists:
nrow(June2016T163B)

df <- cbind.data.frame(June2016T163B[2:3], rep("June 2016", nrow(June2016T163B)), rep("T1-63B", nrow(June2016T163B)))

write.table(df, file="June_2016_T1-63B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T4-64B ############
June2016T464B <- cbind.data.frame(June2016[,1:3], June2016[, grepl("64B", names(June2016))] )
nrow(June2016T464B)
#Filter to keep rows with Wert > 0 (are present):
June2016T464B <- June2016T464B[which(rowSums(June2016T464B[4:5])>0), ]
#Make into lists:
nrow(June2016T464B)

df <- cbind.data.frame(June2016T464B[2:3], rep("June 2016", nrow(June2016T464B)), rep("T4-64B", nrow(June2016T464B)))

write.table(df, file="June_2016_T4-64B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T3-50B ############
June2016T350B <- cbind.data.frame(June2016[,1:3], June2016[, grepl("T3_50B", names(June2016))] )
nrow(June2016T350B)
#Filter to keep rows with Wert > 0 (are present):
June2016T350B <- June2016T350B[which(rowSums(June2016T350B[4:5])>0), ]
#Make into lists:
nrow(June2016T350B)

df <- cbind.data.frame(June2016T350B[2:3], rep("June 2016", nrow(June2016T350B)), rep("T3-50B", nrow(June2016T350B)))

write.table(df, file="June_2016_T3-50B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")


############### July ######################
July2016 <- cbind.data.frame(x2016[ , c(2,5,9)], x2016[ , grepl("Juli", names(x2016))])
head(July2016)
#Make July2016 sub-data frames by trap:
####### Igg-35B ############
July2016Igg <- cbind.data.frame(July2016[,1:3], July2016[, grepl("Igg", names(July2016))] )
nrow(July2016Igg)
#Filter to keep rows with Wert > 0 (are present):
July2016Igg <- July2016Igg[which(rowSums(July2016Igg[4:5])>0), ]
nrow(July2016Igg)
#Make into lists:
df <- cbind.data.frame(July2016Igg[2:3], rep("July 2016", nrow(July2016Igg)), rep("Igg-35B", nrow(July2016Igg)))

write.table(df, file="July_2016_Igg-35B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Jos-21 ############
July2016Jos <- cbind.data.frame(July2016[,1:3], July2016[, grepl("Jos", names(July2016))] )
nrow(July2016Jos)
#Filter to keep rows with Wert > 0 (are present):
July2016Jos <- July2016Jos[which(rowSums(July2016Jos[4:5])>0), ]
nrow(July2016Jos)
#Make into lists:
df <- cbind.data.frame(July2016Jos[2:3], rep("July 2016", nrow(July2016Jos)), rep("Jos-21", nrow(July2016Jos)))

write.table(df, file="July_2016_Jos-21_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Sal-25 ############
July2016Sal <- cbind.data.frame(July2016[,1:3], July2016[, grepl("Sal", names(July2016))] )
#Filter to keep rows with Wert > 0 (are present):
July2016Sal <- July2016Sal[which(rowSums(July2016Sal[4:5])>0), ]
nrow(July2016Sal)
#Make into lists:
df <- cbind.data.frame(July2016Sal[2:3], rep("July 2016", nrow(July2016Sal)), rep("Sal-25", nrow(July2016Sal)))

write.table(df, file="July_2016_Sal-25_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-02B ############
July2016T102B <- cbind.data.frame(July2016[,1:3], July2016[, grepl("T1_02B", names(July2016))] )
nrow(July2016T102B)
#Filter to keep rows with Wert > 0 (are present):
July2016T102B <- July2016T102B[which(rowSums(July2016T102B[4:5])>0), ]
nrow(July2016T102B)
#Make into lists:
df <- cbind.data.frame(July2016T102B[2:3], rep("July 2016", nrow(July2016T102B)), rep("T1-02B", nrow(July2016T102B)))

write.table(df, file="July_2016_T1-02B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-34B ############
July2016T134B <- cbind.data.frame(July2016[,1:3], July2016[, grepl("T1_34B", names(July2016))] )
nrow(July2016T134B)
#Filter to keep rows with Wert > 0 (are present):
July2016T134B <- July2016T134B[which(rowSums(July2016T134B[4:5])>0), ]
#Make into lists:
nrow(July2016T134B)

df <- cbind.data.frame(July2016T134B[2:3], rep("July 2016", nrow(July2016T134B)), rep("T1-34B", nrow(July2016T134B)))

write.table(df, file="July_2016_T1-34B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-52B ############
July2016T152B <- cbind.data.frame(July2016[,1:3], July2016[, grepl("T1_52B", names(July2016))] )
nrow(July2016T152B)
#Filter to keep rows with Wert > 0 (are present):
July2016T152B <- July2016T152B[which(rowSums(July2016T152B[4:5])>0), ]
#Make into lists:
nrow(July2016T152B)

df <- cbind.data.frame(July2016T152B[2:3], rep("July 2016", nrow(July2016T152B)), rep("T1-52B", nrow(July2016T152B)))

write.table(df, file="July_2016_T1-52B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-63B ############
July2016T163B <- cbind.data.frame(July2016[,1:3], July2016[, grepl("T1_63B", names(July2016))] )
nrow(July2016T163B)
#Filter to keep rows with Wert > 0 (are present):
July2016T163B <- July2016T163B[which(rowSums(July2016T163B[4:5])>0), ]
#Make into lists:
nrow(July2016T163B)

df <- cbind.data.frame(July2016T163B[2:3], rep("July 2016", nrow(July2016T163B)), rep("T1-63B", nrow(July2016T163B)))

write.table(df, file="July_2016_T1-63B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T4-64B ############
July2016T464B <- cbind.data.frame(July2016[,1:3], July2016[, grepl("64B", names(July2016))] )
nrow(July2016T464B)
#Filter to keep rows with Wert > 0 (are present):
July2016T464B <- July2016T464B[which(rowSums(July2016T464B[4:5])>0), ]
#Make into lists:
nrow(July2016T464B)

df <- cbind.data.frame(July2016T464B[2:3], rep("July 2016", nrow(July2016T464B)), rep("T4-64B", nrow(July2016T464B)))

write.table(df, file="July_2016_T4-64B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T3-50B ############
July2016T350B <- cbind.data.frame(July2016[,1:3], July2016[, grepl("T3_50B", names(July2016))] )
nrow(July2016T350B)
#Filter to keep rows with Wert > 0 (are present):
July2016T350B <- July2016T350B[which(rowSums(July2016T350B[4:5])>0), ]
#Make into lists:
nrow(July2016T350B)

df <- cbind.data.frame(July2016T350B[2:3], rep("July 2016", nrow(July2016T350B)), rep("T3-50B", nrow(July2016T350B)))

write.table(df, file="July_2016_T3-50B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")


############### August ######################
August2016 <- cbind.data.frame(x2016[ , c(2,5,9)], x2016[ , grepl("Aug", names(x2016))])
head(August2016)
#Make August2016 sub-data frames by trap:
####### Igg-35B ############
August2016Igg <- cbind.data.frame(August2016[,1:3], August2016[, grepl("Igg", names(August2016))] )
nrow(August2016Igg)
#Filter to keep rows with Wert > 0 (are present):
August2016Igg <- August2016Igg[which(rowSums(August2016Igg[4:5])>0), ]
nrow(August2016Igg)
#Make into lists:
df <- cbind.data.frame(August2016Igg[2:3], rep("August 2016", nrow(August2016Igg)), rep("Igg-35B", nrow(August2016Igg)))

write.table(df, file="August_2016_Igg-35B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Jos-21 ############
August2016Jos <- cbind.data.frame(August2016[,1:3], August2016[, grepl("Jos", names(August2016))] )
nrow(August2016Jos)
#Filter to keep rows with Wert > 0 (are present):
August2016Jos <- August2016Jos[which(rowSums(August2016Jos[4:5])>0), ]
nrow(August2016Jos)
#Make into lists:
df <- cbind.data.frame(August2016Jos[2:3], rep("August 2016", nrow(August2016Jos)), rep("Jos-21", nrow(August2016Jos)))

write.table(df, file="August_2016_Jos-21_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Sal-25 ############
August2016Sal <- cbind.data.frame(August2016[,1:3], August2016[, grepl("Sal", names(August2016))] )
#Filter to keep rows with Wert > 0 (are present):
August2016Sal <- August2016Sal[which(rowSums(August2016Sal[4:5])>0), ]
nrow(August2016Sal)
#Make into lists:
df <- cbind.data.frame(August2016Sal[2:3], rep("August 2016", nrow(August2016Sal)), rep("Sal-25", nrow(August2016Sal)))

write.table(df, file="August_2016_Sal-25_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-02B ############
August2016T102B <- cbind.data.frame(August2016[,1:3], August2016[, grepl("T1_02B", names(August2016))] )
nrow(August2016T102B)
#Filter to keep rows with Wert > 0 (are present):
August2016T102B <- August2016T102B[which(rowSums(August2016T102B[4:5])>0), ]
nrow(August2016T102B)
#Make into lists:
df <- cbind.data.frame(August2016T102B[2:3], rep("August 2016", nrow(August2016T102B)), rep("T1-02B", nrow(August2016T102B)))

write.table(df, file="August_2016_T1-02B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-34B ############
August2016T134B <- cbind.data.frame(August2016[,1:3], August2016[, grepl("T1_34B", names(August2016))] )
nrow(August2016T134B)
#Filter to keep rows with Wert > 0 (are present):
August2016T134B <- August2016T134B[which(rowSums(August2016T134B[4:5])>0), ]
#Make into lists:
nrow(August2016T134B)

df <- cbind.data.frame(August2016T134B[2:3], rep("August 2016", nrow(August2016T134B)), rep("T1-34B", nrow(August2016T134B)))

write.table(df, file="August_2016_T1-34B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-52B ############
August2016T152B <- cbind.data.frame(August2016[,1:3], August2016[, grepl("T1_52B", names(August2016))] )
nrow(August2016T152B)
#Filter to keep rows with Wert > 0 (are present):
August2016T152B <- August2016T152B[which(rowSums(August2016T152B[4:5])>0), ]
#Make into lists:
nrow(August2016T152B)

df <- cbind.data.frame(August2016T152B[2:3], rep("August 2016", nrow(August2016T152B)), rep("T1-52B", nrow(August2016T152B)))

write.table(df, file="August_2016_T1-52B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-63B ############
August2016T163B <- cbind.data.frame(August2016[,1:3], August2016[, grepl("T1_63B", names(August2016))] )
nrow(August2016T163B)
#Filter to keep rows with Wert > 0 (are present):
August2016T163B <- August2016T163B[which(rowSums(August2016T163B[4:5])>0), ]
#Make into lists:
nrow(August2016T163B)

df <- cbind.data.frame(August2016T163B[2:3], rep("August 2016", nrow(August2016T163B)), rep("T1-63B", nrow(August2016T163B)))

write.table(df, file="August_2016_T1-63B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T4-64B ############
August2016T464B <- cbind.data.frame(August2016[,1:3], August2016[, grepl("64B", names(August2016))] )
nrow(August2016T464B)
#Filter to keep rows with Wert > 0 (are present):
August2016T464B <- August2016T464B[which(rowSums(August2016T464B[4:5])>0), ]
#Make into lists:
nrow(August2016T464B)

df <- cbind.data.frame(August2016T464B[2:3], rep("August 2016", nrow(August2016T464B)), rep("T4-64B", nrow(August2016T464B)))

write.table(df, file="August_2016_T4-64B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T3-50B ############
August2016T350B <- cbind.data.frame(August2016[,1:3], August2016[, grepl("T3_50B", names(August2016))] )
nrow(August2016T350B)
#Filter to keep rows with Wert > 0 (are present):
August2016T350B <- August2016T350B[which(rowSums(August2016T350B[4:5])>0), ]
#Make into lists:
nrow(August2016T350B)

df <- cbind.data.frame(August2016T350B[2:3], rep("August 2016", nrow(August2016T350B)), rep("T3-50B", nrow(August2016T350B)))

write.table(df, file="August_2016_T3-50B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

############### September ######################
September2016 <- cbind.data.frame(x2016[ , c(2,5,9)], x2016[ , grepl("Sep", names(x2016))])
head(September2016)
#Make September2016 sub-data frames by trap:
####### Igg-35B ############
September2016Igg <- cbind.data.frame(September2016[,1:3], September2016[, grepl("Igg", names(September2016))] )
nrow(September2016Igg)
#Filter to keep rows with Wert > 0 (are present):
September2016Igg <- September2016Igg[which(rowSums(September2016Igg[4:5])>0), ]
nrow(September2016Igg)
#Make into lists:
df <- cbind.data.frame(September2016Igg[2:3], rep("September 2016", nrow(September2016Igg)), rep("Igg-35B", nrow(September2016Igg)))

write.table(df, file="September_2016_Igg-35B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Jos-21 ############
September2016Jos <- cbind.data.frame(September2016[,1:3], September2016[, grepl("Jos", names(September2016))] )
nrow(September2016Jos)
#Filter to keep rows with Wert > 0 (are present):
September2016Jos <- September2016Jos[which(rowSums(September2016Jos[4:5])>0), ]
nrow(September2016Jos)
#Make into lists:
df <- cbind.data.frame(September2016Jos[2:3], rep("September 2016", nrow(September2016Jos)), rep("Jos-21", nrow(September2016Jos)))

write.table(df, file="September_2016_Jos-21_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Sal-25 ############
September2016Sal <- cbind.data.frame(September2016[,1:3], September2016[, grepl("Sal", names(September2016))] )
#Filter to keep rows with Wert > 0 (are present):
September2016Sal <- September2016Sal[which(rowSums(September2016Sal[4:5])>0), ]
nrow(September2016Sal)
#Make into lists:
df <- cbind.data.frame(September2016Sal[2:3], rep("September 2016", nrow(September2016Sal)), rep("Sal-25", nrow(September2016Sal)))

write.table(df, file="September_2016_Sal-25_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-02B ############
September2016T102B <- cbind.data.frame(September2016[,1:3], September2016[, grepl("T1_02B", names(September2016))] )
nrow(September2016T102B)
#Filter to keep rows with Wert > 0 (are present):
September2016T102B <- September2016T102B[which(rowSums(September2016T102B[4:5])>0), ]
nrow(September2016T102B)
#Make into lists:
df <- cbind.data.frame(September2016T102B[2:3], rep("September 2016", nrow(September2016T102B)), rep("T1-02B", nrow(September2016T102B)))

write.table(df, file="September_2016_T1-02B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-34B ############
September2016T134B <- cbind.data.frame(September2016[,1:3], September2016[, grepl("T1_34B", names(September2016))] )
nrow(September2016T134B)
#Filter to keep rows with Wert > 0 (are present):
September2016T134B <- September2016T134B[which(rowSums(September2016T134B[4:5])>0), ]
#Make into lists:
nrow(September2016T134B)

df <- cbind.data.frame(September2016T134B[2:3], rep("September 2016", nrow(September2016T134B)), rep("T1-34B", nrow(September2016T134B)))

write.table(df, file="September_2016_T1-34B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-52B ############
September2016T152B <- cbind.data.frame(September2016[,1:3], September2016[, grepl("T1_52B", names(September2016))] )
nrow(September2016T152B)
#Filter to keep rows with Wert > 0 (are present):
September2016T152B <- September2016T152B[which(rowSums(September2016T152B[4:5])>0), ]
#Make into lists:
nrow(September2016T152B)

df <- cbind.data.frame(September2016T152B[2:3], rep("September 2016", nrow(September2016T152B)), rep("T1-52B", nrow(September2016T152B)))

write.table(df, file="September_2016_T1-52B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-63B ############
September2016T163B <- cbind.data.frame(September2016[,1:3], September2016[, grepl("T1_63B", names(September2016))] )
nrow(September2016T163B)
#Filter to keep rows with Wert > 0 (are present):
September2016T163B <- September2016T163B[which(rowSums(September2016T163B[4:5])>0), ]
#Make into lists:
nrow(September2016T163B)

df <- cbind.data.frame(September2016T163B[2:3], rep("September 2016", nrow(September2016T163B)), rep("T1-63B", nrow(September2016T163B)))

write.table(df, file="September_2016_T1-63B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T4-64B ############
September2016T464B <- cbind.data.frame(September2016[,1:3], September2016[, grepl("64B", names(September2016))] )
nrow(September2016T464B)
#Filter to keep rows with Wert > 0 (are present):
September2016T464B <- September2016T464B[which(rowSums(September2016T464B[4:5])>0), ]
#Make into lists:
nrow(September2016T464B)

df <- cbind.data.frame(September2016T464B[2:3], rep("September 2016", nrow(September2016T464B)), rep("T4-64B", nrow(September2016T464B)))

write.table(df, file="September_2016_T4-64B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T3-50B ############
September2016T350B <- cbind.data.frame(September2016[,1:3], September2016[, grepl("T3_50B", names(September2016))] )
nrow(September2016T350B)
#Filter to keep rows with Wert > 0 (are present):
September2016T350B <- September2016T350B[which(rowSums(September2016T350B[4:5])>0), ]
#Make into lists:
nrow(September2016T350B)

df <- cbind.data.frame(September2016T350B[2:3], rep("September 2016", nrow(September2016T350B)), rep("T3-50B", nrow(September2016T350B)))

write.table(df, file="September_2016_T3-50B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")


################################### 2018 ##############################################
############### May ######################
May2018 <- cbind.data.frame(x2018[ , c(2,5,9)], x2018[ , grepl("Mai", names(x2018))])
head(May2018)
nrow(May2018)
#Make May2018 sub-data frames by trap:
####### Igg-35B ############
May2018Igg <- cbind.data.frame(May2018[,1:3], May2018[, grepl("Igg", names(May2018))] )
head(May2018Igg)
nrow(May2018Igg)
#Filter to keep rows with Wert > 0 (are present):
May2018Igg <- May2018Igg[which(rowSums(May2018Igg[4:5])>0), ]
#Make into lists:
nrow(May2018Igg)
rep("May 2018", nrow(May2018Igg))

df <- cbind.data.frame(May2018Igg[2:3], rep("May 2018", nrow(May2018Igg)), rep("Igg-35B", nrow(May2018Igg)))

write.table(df, file="May_2018_Igg-35B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Jos-21 ############
May2018Jos <- cbind.data.frame(May2018[,1:3], May2018[, grepl("Jos", names(May2018))] )
#Filter to keep rows with Wert > 0 (are present):
May2018Jos <- May2018Jos[which(rowSums(May2018Jos[4:5])>0), ]
head(May2018Jos)
nrow(May2018Jos)
#Make into lists:
df <- cbind.data.frame(May2018Jos[2:3], rep("May 2018", nrow(May2018Jos)), rep("Jos-21", nrow(May2018Jos)))

write.table(df, file="May_2018_Jos-21_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Sal-25 ############
May2018Sal <- cbind.data.frame(May2018[,1:3], May2018[, grepl("Sal", names(May2018))] )
head(May2018Sal)
nrow(May2018Sal)
#Filter to keep rows with Wert > 0 (are present):
May2018Sal <- May2018Sal[which(rowSums(May2018Sal[4:5])>0), ]
#Make into lists:
nrow(May2018Sal)

df <- cbind.data.frame(May2018Sal[2:3], rep("May 2018", nrow(May2018Sal)), rep("Sal-25", nrow(May2018Sal)))

write.table(df, file="May_2018_Sal-25_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-02B ############
May2018T102B <- cbind.data.frame(May2018[,1:3], May2018[, grepl("T1_02B", names(May2018))] )
head(May2018T102B)
nrow(May2018T102B)
#Filter to keep rows with Wert > 0 (are present):
May2018T102B <- May2018T102B[which(rowSums(May2018T102B[4:5])>0), ]
#Make into lists:
nrow(May2018T102B)

df <- cbind.data.frame(May2018T102B[2:3], rep("May 2018", nrow(May2018T102B)), rep("T1-02B", nrow(May2018T102B)))

write.table(df, file="May_2018_T1-02B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")


####### T1-34B ############
May2018T134B <- cbind.data.frame(May2018[,1:3], May2018[, grepl("T1_34B", names(May2018))] )
head(May2018T134B) ######### Here is a missing sample ##########
nrow(May2018T134B)
#Filter to keep rows with Wert > 0 (are present):
May2018T134B <- May2018T134B[which(rowSums(May2018T134B[4])>0), ]
#Make into lists:
nrow(May2018T134B)

df <- cbind.data.frame(May2018T134B[2:3], rep("May 2018", nrow(May2018T134B)), rep("T1-34B", nrow(May2018T134B)))

write.table(df, file="May_2018_T1-34B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")


####### T1-52B ############
May2018T152B <- cbind.data.frame(May2018[,1:3], May2018[, grepl("T1_52B", names(May2018))] )
nrow(May2018T152B)
#Filter to keep rows with Wert > 0 (are present):
May2018T152B <- May2018T152B[which(rowSums(May2018T152B[4:5])>0), ]
#Make into lists:
nrow(May2018T152B)

df <- cbind.data.frame(May2018T152B[2:3], rep("May 2018", nrow(May2018T152B)), rep("T1-52B", nrow(May2018T152B)))

write.table(df, file="May_2018_T1-52B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-63B ############
May2018T163B <- cbind.data.frame(May2018[,1:3], May2018[, grepl("T1_63B", names(May2018))] )
nrow(May2018T163B)
#Filter to keep rows with Wert > 0 (are present):
May2018T163B <- May2018T163B[which(rowSums(May2018T163B[4:5])>0), ]
#Make into lists:
nrow(May2018T163B)

df <- cbind.data.frame(May2018T163B[2:3], rep("May 2018", nrow(May2018T163B)), rep("T1-63B", nrow(May2018T163B)))

write.table(df, file="May_2018_T1-63B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T4-64B ############
May2018T464B <- cbind.data.frame(May2018[,1:3], May2018[, grepl("64B", names(May2018))] )
head(May2018T464B)   ########### Here is another missing sample. ################
nrow(May2018T464B)
#Filter to keep rows with Wert > 0 (are present):
May2018T464B <- May2018T464B[which(rowSums(May2018T464B[4])>0), ]
#Make into lists:
nrow(May2018T464B)

df <- cbind.data.frame(May2018T464B[2:3], rep("May 2018", nrow(May2018T464B)), rep("T4-64B", nrow(May2018T464B)))

write.table(df, file="May_2018_T4-64B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T3-50B ############
May2018T350B <- cbind.data.frame(May2018[,1:3], May2018[, grepl("T3_50B", names(May2018))] )
nrow(May2018T350B)
#Filter to keep rows with Wert > 0 (are present):
May2018T350B <- May2018T350B[which(rowSums(May2018T350B[4:5])>0), ]
#Make into lists:
nrow(May2018T350B)

df <- cbind.data.frame(May2018T350B[2:3], rep("May 2018", nrow(May2018T350B)), rep("T3-50B", nrow(May2018T350B)))

write.table(df, file="May_2018_T3-50B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")


############### June ######################
June2018 <- cbind.data.frame(x2018[ , c(2,5,9)], x2018[ , grepl("Juni", names(x2018))])
#Make June2018 sub-data frames by trap:
####### Igg-35B ############
June2018Igg <- cbind.data.frame(June2018[,1:3], June2018[, grepl("Igg", names(June2018))] )
nrow(June2018Igg)
#Filter to keep rows with Wert > 0 (are present):
June2018Igg <- June2018Igg[which(rowSums(June2018Igg[4:5])>0), ]
nrow(June2018Igg)
#Make into lists:
df <- cbind.data.frame(June2018Igg[2:3], rep("June 2018", nrow(June2018Igg)), rep("Igg-35B", nrow(June2018Igg)))

write.table(df, file="June_2018_Igg-35B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Jos-21 ############
June2018Jos <- cbind.data.frame(June2018[,1:3], June2018[, grepl("Jos", names(June2018))] )
nrow(June2018Jos)
#Filter to keep rows with Wert > 0 (are present):
June2018Jos <- June2018Jos[which(rowSums(June2018Jos[4:5])>0), ]
nrow(June2018Jos)
#Make into lists:
df <- cbind.data.frame(June2018Jos[2:3], rep("June 2018", nrow(June2018Jos)), rep("Jos-21", nrow(June2018Jos)))

write.table(df, file="June_2018_Jos-21_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Sal-25 ############
June2018Sal <- cbind.data.frame(June2018[,1:3], June2018[, grepl("Sal", names(June2018))] )
head(June2018Sal)    ###### Another missing sample ################
nrow(June2018Sal)
#Filter to keep rows with Wert > 0 (are present):
June2018Sal <- June2018Sal[which(rowSums(June2018Sal[4])>0), ]
nrow(June2018Sal)
#Make into lists:
df <- cbind.data.frame(June2018Sal[2:3], rep("June 2018", nrow(June2018Sal)), rep("Sal-25", nrow(June2018Sal)))

write.table(df, file="June_2018_Sal-25_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-02B ############
June2018T102B <- cbind.data.frame(June2018[,1:3], June2018[, grepl("T1_02B", names(June2018))] )
nrow(June2018T102B)
#Filter to keep rows with Wert > 0 (are present):
June2018T102B <- June2018T102B[which(rowSums(June2018T102B[4:5])>0), ]
nrow(June2018T102B)
#Make into lists:
df <- cbind.data.frame(June2018T102B[2:3], rep("June 2018", nrow(June2018T102B)), rep("T1-02B", nrow(June2018T102B)))

write.table(df, file="June_2018_T1-02B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-34B ############
June2018T134B <- cbind.data.frame(June2018[,1:3], June2018[, grepl("T1_34B", names(June2018))] )
nrow(June2018T134B)
#Filter to keep rows with Wert > 0 (are present):
June2018T134B <- June2018T134B[which(rowSums(June2018T134B[4:5])>0), ]
#Make into lists:
nrow(June2018T134B)

df <- cbind.data.frame(June2018T134B[2:3], rep("June 2018", nrow(June2018T134B)), rep("T1-34B", nrow(June2018T134B)))

write.table(df, file="June_2018_T1-34B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-52B ############
June2018T152B <- cbind.data.frame(June2018[,1:3], June2018[, grepl("T1_52B", names(June2018))] )
head(June2018T152B)   ########### Another missing sample ##################
nrow(June2018T152B)
#Filter to keep rows with Wert > 0 (are present):
June2018T152B <- June2018T152B[which(rowSums(June2018T152B[4])>0), ]
#Make into lists:
nrow(June2018T152B)

df <- cbind.data.frame(June2018T152B[2:3], rep("June 2018", nrow(June2018T152B)), rep("T1-52B", nrow(June2018T152B)))

write.table(df, file="June_2018_T1-52B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-63B ############
June2018T163B <- cbind.data.frame(June2018[,1:3], June2018[, grepl("T1_63B", names(June2018))] )
nrow(June2018T163B)
#Filter to keep rows with Wert > 0 (are present):
June2018T163B <- June2018T163B[which(rowSums(June2018T163B[4:5])>0), ]
#Make into lists:
nrow(June2018T163B)

df <- cbind.data.frame(June2018T163B[2:3], rep("June 2018", nrow(June2018T163B)), rep("T1-63B", nrow(June2018T163B)))

write.table(df, file="June_2018_T1-63B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T4-64B ############
June2018T464B <- cbind.data.frame(June2018[,1:3], June2018[, grepl("64B", names(June2018))] )
nrow(June2018T464B)
#Filter to keep rows with Wert > 0 (are present):
June2018T464B <- June2018T464B[which(rowSums(June2018T464B[4:5])>0), ]
#Make into lists:
nrow(June2018T464B)

df <- cbind.data.frame(June2018T464B[2:3], rep("June 2018", nrow(June2018T464B)), rep("T4-64B", nrow(June2018T464B)))

write.table(df, file="June_2018_T4-64B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T3-50B ############
June2018T350B <- cbind.data.frame(June2018[,1:3], June2018[, grepl("T3_50B", names(June2018))] )
nrow(June2018T350B)
#Filter to keep rows with Wert > 0 (are present):
June2018T350B <- June2018T350B[which(rowSums(June2018T350B[4:5])>0), ]
#Make into lists:
nrow(June2018T350B)

df <- cbind.data.frame(June2018T350B[2:3], rep("June 2018", nrow(June2018T350B)), rep("T3-50B", nrow(June2018T350B)))

write.table(df, file="June_2018_T3-50B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")


############### July ######################
July2018 <- cbind.data.frame(x2018[ , c(2,5,9)], x2018[ , grepl("Juli", names(x2018))])
head(July2018)
#Make July2018 sub-data frames by trap:
####### Igg-35B ############
July2018Igg <- cbind.data.frame(July2018[,1:3], July2018[, grepl("Igg", names(July2018))] )
nrow(July2018Igg)
#Filter to keep rows with Wert > 0 (are present):
July2018Igg <- July2018Igg[which(rowSums(July2018Igg[4:5])>0), ]
nrow(July2018Igg)
#Make into lists:
df <- cbind.data.frame(July2018Igg[2:3], rep("July 2018", nrow(July2018Igg)), rep("Igg-35B", nrow(July2018Igg)))

write.table(df, file="July_2018_Igg-35B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Jos-21 ############
July2018Jos <- cbind.data.frame(July2018[,1:3], July2018[, grepl("Jos", names(July2018))] )
nrow(July2018Jos)
#Filter to keep rows with Wert > 0 (are present):
July2018Jos <- July2018Jos[which(rowSums(July2018Jos[4:5])>0), ]
nrow(July2018Jos)
#Make into lists:
df <- cbind.data.frame(July2018Jos[2:3], rep("July 2018", nrow(July2018Jos)), rep("Jos-21", nrow(July2018Jos)))

write.table(df, file="July_2018_Jos-21_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Sal-25 ############
July2018Sal <- cbind.data.frame(July2018[,1:3], July2018[, grepl("Sal", names(July2018))] )
head(July2018Sal)   ##################### Another missing sample #########
#Filter to keep rows with Wert > 0 (are present):
July2018Sal <- July2018Sal[which(rowSums(July2018Sal[4])>0), ]
nrow(July2018Sal)
#Make into lists:
df <- cbind.data.frame(July2018Sal[2:3], rep("July 2018", nrow(July2018Sal)), rep("Sal-25", nrow(July2018Sal)))

write.table(df, file="July_2018_Sal-25_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-02B ############
July2018T102B <- cbind.data.frame(July2018[,1:3], July2018[, grepl("T1_02B", names(July2018))] )
nrow(July2018T102B)
#Filter to keep rows with Wert > 0 (are present):
July2018T102B <- July2018T102B[which(rowSums(July2018T102B[4:5])>0), ]
nrow(July2018T102B)
#Make into lists:
df <- cbind.data.frame(July2018T102B[2:3], rep("July 2018", nrow(July2018T102B)), rep("T1-02B", nrow(July2018T102B)))

write.table(df, file="July_2018_T1-02B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-34B ############
July2018T134B <- cbind.data.frame(July2018[,1:3], July2018[, grepl("T1_34B", names(July2018))] )
nrow(July2018T134B)
#Filter to keep rows with Wert > 0 (are present):
July2018T134B <- July2018T134B[which(rowSums(July2018T134B[4:5])>0), ]
#Make into lists:
nrow(July2018T134B)

df <- cbind.data.frame(July2018T134B[2:3], rep("July 2018", nrow(July2018T134B)), rep("T1-34B", nrow(July2018T134B)))

write.table(df, file="July_2018_T1-34B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-52B ############
July2018T152B <- cbind.data.frame(July2018[,1:3], July2018[, grepl("T1_52B", names(July2018))] )
nrow(July2018T152B)
#Filter to keep rows with Wert > 0 (are present):
July2018T152B <- July2018T152B[which(rowSums(July2018T152B[4:5])>0), ]
#Make into lists:
nrow(July2018T152B)

df <- cbind.data.frame(July2018T152B[2:3], rep("July 2018", nrow(July2018T152B)), rep("T1-52B", nrow(July2018T152B)))

write.table(df, file="July_2018_T1-52B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-63B ############
July2018T163B <- cbind.data.frame(July2018[,1:3], July2018[, grepl("T1_63B", names(July2018))] )
nrow(July2018T163B)
#Filter to keep rows with Wert > 0 (are present):
July2018T163B <- July2018T163B[which(rowSums(July2018T163B[4:5])>0), ]
#Make into lists:
nrow(July2018T163B)

df <- cbind.data.frame(July2018T163B[2:3], rep("July 2018", nrow(July2018T163B)), rep("T1-63B", nrow(July2018T163B)))

write.table(df, file="July_2018_T1-63B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T4-64B ############
July2018T464B <- cbind.data.frame(July2018[,1:3], July2018[, grepl("64B", names(July2018))] )
nrow(July2018T464B)
#Filter to keep rows with Wert > 0 (are present):
July2018T464B <- July2018T464B[which(rowSums(July2018T464B[4:5])>0), ]
#Make into lists:
nrow(July2018T464B)

df <- cbind.data.frame(July2018T464B[2:3], rep("July 2018", nrow(July2018T464B)), rep("T4-64B", nrow(July2018T464B)))

write.table(df, file="July_2018_T4-64B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T3-50B ############
July2018T350B <- cbind.data.frame(July2018[,1:3], July2018[, grepl("T3_50B", names(July2018))] )
nrow(July2018T350B)
#Filter to keep rows with Wert > 0 (are present):
July2018T350B <- July2018T350B[which(rowSums(July2018T350B[4:5])>0), ]
#Make into lists:
nrow(July2018T350B)

df <- cbind.data.frame(July2018T350B[2:3], rep("July 2018", nrow(July2018T350B)), rep("T3-50B", nrow(July2018T350B)))

write.table(df, file="July_2018_T3-50B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")


############### August ######################
August2018 <- cbind.data.frame(x2018[ , c(2,5,9)], x2018[ , grepl("Aug", names(x2018))])
head(August2018)
#Make August2018 sub-data frames by trap:
####### Igg-35B ############
August2018Igg <- cbind.data.frame(August2018[,1:3], August2018[, grepl("Igg", names(August2018))] )
nrow(August2018Igg)
#Filter to keep rows with Wert > 0 (are present):
August2018Igg <- August2018Igg[which(rowSums(August2018Igg[4:5])>0), ]
nrow(August2018Igg)
#Make into lists:
df <- cbind.data.frame(August2018Igg[2:3], rep("August 2018", nrow(August2018Igg)), rep("Igg-35B", nrow(August2018Igg)))

write.table(df, file="August_2018_Igg-35B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Jos-21 ############
August2018Jos <- cbind.data.frame(August2018[,1:3], August2018[, grepl("Jos", names(August2018))] )
nrow(August2018Jos)
#Filter to keep rows with Wert > 0 (are present):
August2018Jos <- August2018Jos[which(rowSums(August2018Jos[4:5])>0), ]
nrow(August2018Jos)
#Make into lists:
df <- cbind.data.frame(August2018Jos[2:3], rep("August 2018", nrow(August2018Jos)), rep("Jos-21", nrow(August2018Jos)))

write.table(df, file="August_2018_Jos-21_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Sal-25 ############
August2018Sal <- cbind.data.frame(August2018[,1:3], August2018[, grepl("Sal", names(August2018))] )
#Filter to keep rows with Wert > 0 (are present):
August2018Sal <- August2018Sal[which(rowSums(August2018Sal[4:5])>0), ]
nrow(August2018Sal)
#Make into lists:
df <- cbind.data.frame(August2018Sal[2:3], rep("August 2018", nrow(August2018Sal)), rep("Sal-25", nrow(August2018Sal)))

write.table(df, file="August_2018_Sal-25_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-02B ############
August2018T102B <- cbind.data.frame(August2018[,1:3], August2018[, grepl("T1_02B", names(August2018))] )
nrow(August2018T102B)
#Filter to keep rows with Wert > 0 (are present):
August2018T102B <- August2018T102B[which(rowSums(August2018T102B[4:5])>0), ]
nrow(August2018T102B)
#Make into lists:
df <- cbind.data.frame(August2018T102B[2:3], rep("August 2018", nrow(August2018T102B)), rep("T1-02B", nrow(August2018T102B)))

write.table(df, file="August_2018_T1-02B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-34B ############
August2018T134B <- cbind.data.frame(August2018[,1:3], August2018[, grepl("T1_34B", names(August2018))] )
nrow(August2018T134B)
#Filter to keep rows with Wert > 0 (are present):
August2018T134B <- August2018T134B[which(rowSums(August2018T134B[4:5])>0), ]
#Make into lists:
nrow(August2018T134B)

df <- cbind.data.frame(August2018T134B[2:3], rep("August 2018", nrow(August2018T134B)), rep("T1-34B", nrow(August2018T134B)))

write.table(df, file="August_2018_T1-34B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-52B ############
August2018T152B <- cbind.data.frame(August2018[,1:3], August2018[, grepl("T1_52B", names(August2018))] )
nrow(August2018T152B)
#Filter to keep rows with Wert > 0 (are present):
August2018T152B <- August2018T152B[which(rowSums(August2018T152B[4:5])>0), ]
#Make into lists:
nrow(August2018T152B)

df <- cbind.data.frame(August2018T152B[2:3], rep("August 2018", nrow(August2018T152B)), rep("T1-52B", nrow(August2018T152B)))

write.table(df, file="August_2018_T1-52B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-63B ############
August2018T163B <- cbind.data.frame(August2018[,1:3], August2018[, grepl("T1_63B", names(August2018))] )
nrow(August2018T163B)
#Filter to keep rows with Wert > 0 (are present):
August2018T163B <- August2018T163B[which(rowSums(August2018T163B[4:5])>0), ]
#Make into lists:
nrow(August2018T163B)

df <- cbind.data.frame(August2018T163B[2:3], rep("August 2018", nrow(August2018T163B)), rep("T1-63B", nrow(August2018T163B)))

write.table(df, file="August_2018_T1-63B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T4-64B ############
August2018T464B <- cbind.data.frame(August2018[,1:3], August2018[, grepl("64B", names(August2018))] )
nrow(August2018T464B)
#Filter to keep rows with Wert > 0 (are present):
August2018T464B <- August2018T464B[which(rowSums(August2018T464B[4:5])>0), ]
#Make into lists:
nrow(August2018T464B)

df <- cbind.data.frame(August2018T464B[2:3], rep("August 2018", nrow(August2018T464B)), rep("T4-64B", nrow(August2018T464B)))

write.table(df, file="August_2018_T4-64B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T3-50B ############
August2018T350B <- cbind.data.frame(August2018[,1:3], August2018[, grepl("T3_50B", names(August2018))] )
nrow(August2018T350B)
#Filter to keep rows with Wert > 0 (are present):
August2018T350B <- August2018T350B[which(rowSums(August2018T350B[4:5])>0), ]
#Make into lists:
nrow(August2018T350B)

df <- cbind.data.frame(August2018T350B[2:3], rep("August 2018", nrow(August2018T350B)), rep("T3-50B", nrow(August2018T350B)))

write.table(df, file="August_2018_T3-50B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

############### September ######################
September2018 <- cbind.data.frame(x2018[ , c(2,5,9)], x2018[ , grepl("Sep", names(x2018))])
head(September2018)
#Make September2018 sub-data frames by trap:
####### Igg-35B ############
September2018Igg <- cbind.data.frame(September2018[,1:3], September2018[, grepl("Igg", names(September2018))] )
nrow(September2018Igg)
#Filter to keep rows with Wert > 0 (are present):
September2018Igg <- September2018Igg[which(rowSums(September2018Igg[4:5])>0), ]
nrow(September2018Igg)
#Make into lists:
df <- cbind.data.frame(September2018Igg[2:3], rep("September 2018", nrow(September2018Igg)), rep("Igg-35B", nrow(September2018Igg)))

write.table(df, file="September_2018_Igg-35B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Jos-21 ############
September2018Jos <- cbind.data.frame(September2018[,1:3], September2018[, grepl("Jos", names(September2018))] )
nrow(September2018Jos)
#Filter to keep rows with Wert > 0 (are present):
September2018Jos <- September2018Jos[which(rowSums(September2018Jos[4:5])>0), ]
nrow(September2018Jos)
#Make into lists:
df <- cbind.data.frame(September2018Jos[2:3], rep("September 2018", nrow(September2018Jos)), rep("Jos-21", nrow(September2018Jos)))

write.table(df, file="September_2018_Jos-21_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### Sal-25 ############
September2018Sal <- cbind.data.frame(September2018[,1:3], September2018[, grepl("Sal", names(September2018))] )
#Filter to keep rows with Wert > 0 (are present):
September2018Sal <- September2018Sal[which(rowSums(September2018Sal[4:5])>0), ]
nrow(September2018Sal)
#Make into lists:
df <- cbind.data.frame(September2018Sal[2:3], rep("September 2018", nrow(September2018Sal)), rep("Sal-25", nrow(September2018Sal)))

write.table(df, file="September_2018_Sal-25_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-02B ############
September2018T102B <- cbind.data.frame(September2018[,1:3], September2018[, grepl("T1_02B", names(September2018))] )
nrow(September2018T102B)
#Filter to keep rows with Wert > 0 (are present):
September2018T102B <- September2018T102B[which(rowSums(September2018T102B[4:5])>0), ]
nrow(September2018T102B)
#Make into lists:
df <- cbind.data.frame(September2018T102B[2:3], rep("September 2018", nrow(September2018T102B)), rep("T1-02B", nrow(September2018T102B)))

write.table(df, file="September_2018_T1-02B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-34B ############
September2018T134B <- cbind.data.frame(September2018[,1:3], September2018[, grepl("T1_34B", names(September2018))] )
nrow(September2018T134B)
#Filter to keep rows with Wert > 0 (are present):
September2018T134B <- September2018T134B[which(rowSums(September2018T134B[4:5])>0), ]
#Make into lists:
nrow(September2018T134B)

df <- cbind.data.frame(September2018T134B[2:3], rep("September 2018", nrow(September2018T134B)), rep("T1-34B", nrow(September2018T134B)))

write.table(df, file="September_2018_T1-34B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-52B ############
September2018T152B <- cbind.data.frame(September2018[,1:3], September2018[, grepl("T1_52B", names(September2018))] )
nrow(September2018T152B)
#Filter to keep rows with Wert > 0 (are present):
September2018T152B <- September2018T152B[which(rowSums(September2018T152B[4:5])>0), ]
#Make into lists:
nrow(September2018T152B)

df <- cbind.data.frame(September2018T152B[2:3], rep("September 2018", nrow(September2018T152B)), rep("T1-52B", nrow(September2018T152B)))

write.table(df, file="September_2018_T1-52B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T1-63B ############
September2018T163B <- cbind.data.frame(September2018[,1:3], September2018[, grepl("T1_63B", names(September2018))] )
nrow(September2018T163B)
#Filter to keep rows with Wert > 0 (are present):
September2018T163B <- September2018T163B[which(rowSums(September2018T163B[4:5])>0), ]
#Make into lists:
nrow(September2018T163B)

df <- cbind.data.frame(September2018T163B[2:3], rep("September 2018", nrow(September2018T163B)), rep("T1-63B", nrow(September2018T163B)))

write.table(df, file="September_2018_T1-63B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T4-64B ############
September2018T464B <- cbind.data.frame(September2018[,1:3], September2018[, grepl("64B", names(September2018))] )
nrow(September2018T464B)
#Filter to keep rows with Wert > 0 (are present):
September2018T464B <- September2018T464B[which(rowSums(September2018T464B[4:5])>0), ]
#Make into lists:
nrow(September2018T464B)

df <- cbind.data.frame(September2018T464B[2:3], rep("September 2018", nrow(September2018T464B)), rep("T4-64B", nrow(September2018T464B)))

write.table(df, file="September_2018_T4-64B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")

####### T3-50B ############
September2018T350B <- cbind.data.frame(September2018[,1:3], September2018[, grepl("T3_50B", names(September2018))] )
nrow(September2018T350B)
#Filter to keep rows with Wert > 0 (are present):
September2018T350B <- September2018T350B[which(rowSums(September2018T350B[4:5])>0), ]
#Make into lists:
nrow(September2018T350B)

df <- cbind.data.frame(September2018T350B[2:3], rep("September 2018", nrow(September2018T350B)), rep("T3-50B", nrow(September2018T350B)))

write.table(df, file="September_2018_T3-50B_list.txt", quote=F, row.names=F, col.names=F, sep="\t")


#Part I. help + bare Infinitive

#Step 1. Find the instances of help + bare Inf in the bigram data from COHA

#open the dataset with bigrams
h2 <- read.table("h_2grams.txt", sep =  "\t", quote = "")
colnames(h2) <- c("Freq", "Word1", "Word2", "POS1", "POS2", "DecadeID")

#make a subset of data with help as a base form + another infinitive:
help2 <-h2[h2$Word1 == "help"&h2$POS1 == "vv0"&h2$POS2 == "vvi", ]
summary(help2)
head(help2)

#remove the help and POS columns, which are no longer needed
help2 <- help2[, -c(2, 4, 5)]
head(help2)

#Step 2. Normalize the frequencies
#read the table with total frequencies for each decade
totals <- read.table("total_period.txt", header = T)
totals

#add the information about total frequency per decade by merging the data frames
help2 <- merge(help2, totals[, -2], by = "DecadeID")
head(help2)

#normalize the frequencies of infinitives per million words
help2$NormFreq <- 1000000*help2$Freq/help2$Total
head(help2)

#Part II. Now help + to + Infinitive...
#Step 1. Find the instances of help + to-Inf in the trigram data from COHA

#open the dataset with trigrams
h3 <- read.table("h_3grams.txt", sep =  "\t", quote = "")
head(h3)
colnames(h3) <- c("Freq", "Word1", "Word2", "Word3", "POS1", "POS2", "POS3", "DecadeID")

#make a subset of data with help as an infinitive or base form + to + another infinitive:
help3 <-h3[h3$Word1 == "help"&h3$POS1 == "vv0"&h3$POS3 == "vvi"&h3$Word2 == "to", ]
summary(help3)
head(help3)

#remove the help and POS columns, which are no longer needed
help3 <- help3[, c(1, 4, 8)]
head(help3)

#Step 2. Normalize the frequencies
#add the information about total frequency per decade by merging the data frames
help3 <- merge(help3, totals[, -2], by = "DecadeID")
head(help3)

#normalize the frequencies of infinitives per million words
help3$NormFreq <- 1000000*help3$Freq/help3$Total
head(help3)

#Part III. Put everything together
#First, rename the column with the infinitive in both data frames with help

colnames(help2)[3]
colnames(help3)[3]
colnames(help2)[3] <- "Infinitive"
colnames(help3)[3] <- "Infinitive"

#We also don't need the Freq and Total columns any more:
help2 <- help2[, -c(2, 4)]
help3 <- help3[, -c(2, 4)]

#Rename NormFreq in help2 to NormFreq_bare and Freq in help3 to NormFreq_to
colnames(help2)[3] <- "NormFreq_bare"
colnames(help3)[3] <- "NormFreq_to"

#Create one data frame
help_all <- merge(help2, help3, by = c("Infinitive", "DecadeID"), all = TRUE)
#replace NA with 0
help_all$NormFreq_bare[is.na(help_all$NormFreq_bare)] <- 0
help_all$NormFreq_to[is.na(help_all$NormFreq_to)] <- 0
head(help_all)
help_all$Infinitive <- factor(help_all$Infinitive)
help_all <- unique(help_all)

#Make a motion chart
library(googleVis)
help_mch <- gvisMotionChart(help_all, idvar = "Infinitive", timevar = "DecadeID", xvar = "NormFreq_to", yvar = "NormFreq_bare")
plot(help_mch)

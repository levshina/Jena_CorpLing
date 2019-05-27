#Session 5. How to cluster texts based on frequencies of words, lemmas, parts of speech, etc. 

#Step 1. We need to load the package udpipe and the model
library(udpipe)
eng_ud <- udpipe_load_model("english-ewt-ud-2.3-181115.udpipe")

#Step 2. Install and load the package readr
library(readr)

#Step 3. Read and clean the subtitles LoR1 (Lord of the Rings 1)
LoR1 <- read_file("LoR1.txt")

#remove all italics
LoR1 <- gsub("<i>", "", LoR1)
LoR1 <- gsub("</i>", "", LoR1)

#remove the timing information 
LoR1 <- gsub("\\b[0-9:,]+\\b", "", LoR1)
LoR1 <- gsub("-->", "", LoR1)

#Parse the text 
LoR1_ud <- udpipe(LoR1, eng_ud)

#How to get the frequency list of lemmas
LoR1_freq <- table(LoR1_ud$lemma)
sort(LoR1_freq, decreasing = TRUE)[1:20]

#However, it includes punctuation. Here's how to remove it:
LoR1_freq <- table(LoR1_ud$lemma[LoR1_ud$upos != "PUNCT"])
sort(LoR1_freq, decreasing = TRUE)[1:20]

#This is how you can include only the notional words:
LoR1_freq <- table(LoR1_ud$lemma[LoR1_ud$upos %in% c("NOUN", "PROPN", "ADJ", "ADV", "VERB")])
sort(LoR1_freq, decreasing = TRUE)[1:20]


####NEW: correction of mistakes
LoR1_ud$lemma[LoR1_ud$lemma == "...nor"] <- "nor"
LoR1_ud$lemma[LoR1_ud$lemma == "...perfect"] <- "perfect"
LoR1_freq <- table(LoR1_ud$lemma[LoR1_ud$upos %in% c("NOUN", "PROPN", "ADJ", "ADV", "VERB")])
sort(LoR1_freq, decreasing = TRUE)[1:20]



#Step 4. Repeat the same for the rest of the files.
LoR2 <- read_file("LoR2.txt")
LoR2 <- gsub("<i>", "", LoR2)
LoR2 <- gsub("</i>", "", LoR2)
LoR2 <- gsub("\\b[0-9:,]+\\b", "", LoR2)
LoR2 <- gsub("-->", "", LoR2)
LoR2_ud <- udpipe(LoR2, eng_ud)
LoR2_freq <- table(LoR2_ud$lemma[LoR2_ud$upos %in% c("NOUN", "PROPN", "ADJ", "ADV", "VERB")])
sort(LoR2_freq, decreasing = TRUE)[1:20]

###NEW: correction of parsing mistakes
LoR2_ud$lemma[LoR2_ud$lemma == "...thousand"] <- "thousand"
LoR2_ud$lemma[LoR2_ud$lemma == "...until"] <- "until"
LoR2_freq <- table(LoR2_ud$lemma[LoR2_ud$upos %in% c("NOUN", "PROPN", "ADJ", "ADV", "VERB")])
sort(LoR2_freq, decreasing = TRUE)[1:20]

LoR3 <- read_file("LoR3.txt")
LoR3 <- gsub("<i>", "", LoR3)
LoR3 <- gsub("</i>", "", LoR3)
LoR3 <- gsub("\\b[0-9:,]+\\b", "", LoR3)
LoR3 <- gsub("-->", "", LoR3)
LoR3_ud <- udpipe(LoR3, eng_ud)
LoR3_freq <- table(LoR3_ud$lemma[LoR3_ud$upos %in% c("NOUN", "PROPN", "ADJ", "ADV", "VERB")])
sort(LoR3_freq, decreasing = TRUE)[1:20]

###NEW: correction of parsing mistakes
LoR3_ud$lemma[LoR3_ud$lemma == "...from"] <- "from"
LoR3_ud$lemma[LoR3_ud$lemma == "...once"] <- "once"
LoR3_ud$lemma[LoR3_ud$lemma == "...traitor"] <- "traitor"
LoR3_freq <- table(LoR3_ud$lemma[LoR3_ud$upos %in% c("NOUN", "PROPN", "ADJ", "ADV", "VERB")])
sort(LoR3_freq, decreasing = TRUE)[1:20]

Hobbit1 <- read_file("Hobbit1.txt")
Hobbit1 <- gsub("<i>", "", Hobbit1)
Hobbit1 <- gsub("</i>", "", Hobbit1)
Hobbit1 <- gsub("\\b[0-9:,]+\\b", "", Hobbit1)
Hobbit1 <- gsub("-->", "", Hobbit1)
Hobbit1_ud <- udpipe(Hobbit1, eng_ud)
Hobbit1_freq <- table(Hobbit1_ud$lemma[Hobbit1_ud$upos %in% c("NOUN", "PROPN", "ADJ", "ADV", "VERB")])
sort(Hobbit1_freq, decreasing = TRUE)[1:20]

###NEW: correction of parsing mistakes (see Hobbit1_df)
Hobbit1_ud$lemma[Hobbit1_ud$lemma == "...drop"] <- "drop"
Hobbit1_ud$lemma[Hobbit1_ud$lemma == "...nor"] <- "nor"
Hobbit1_ud$lemma[Hobbit1_ud$lemma == "abandoned"] <- "abandon"

Hobbit1_freq <- table(Hobbit1_ud$lemma[Hobbit1_ud$upos %in% c("NOUN", "PROPN", "ADJ", "ADV", "VERB")])
sort(Hobbit1_freq, decreasing = TRUE)[1:20]



Hobbit2 <- read_file("Hobbit2.txt")
Hobbit2 <- gsub("<i>", "", Hobbit2)
Hobbit2 <- gsub("</i>", "", Hobbit2)
Hobbit2 <- gsub("\\b[0-9:,]+\\b", "", Hobbit2)
Hobbit2 <- gsub("-->", "", Hobbit2)
Hobbit2_ud <- udpipe(Hobbit2, eng_ud)
Hobbit2_freq <- table(Hobbit2_ud$lemma[Hobbit2_ud$upos %in% c("NOUN", "PROPN", "ADJ", "ADV", "VERB")])
sort(Hobbit2_freq, decreasing = TRUE)[1:20]

####added: correction of mistakes
#to see the mistake:
Hobbit2_ud[Hobbit2_ud$lemma == "Dwarve", 8:14]
#to correct the mistake:
Hobbit2_ud$lemma[Hobbit2_ud$lemma == "Dwarve"] <- "dwarf"
Hobbit2_freq <- table(Hobbit2_ud$lemma[Hobbit2_ud$upos %in% c("NOUN", "PROPN", "ADJ", "ADV", "VERB")])
sort(Hobbit2_freq, decreasing = TRUE)[1:20]

Hobbit3 <- read_file("Hobbit3.txt")
Hobbit3 <- gsub("<i>", "", Hobbit3)
Hobbit3 <- gsub("</i>", "", Hobbit3)
Hobbit3 <- gsub("\\b[0-9:,]+\\b", "", Hobbit3)
Hobbit3 <- gsub("-->", "", Hobbit3)
Hobbit3_ud <- udpipe(Hobbit3, eng_ud)
Hobbit3_freq <- table(Hobbit3_ud$lemma[Hobbit3_ud$upos %in% c("NOUN", "PROPN", "ADJ", "ADV", "VERB")])
sort(Hobbit3_freq, decreasing = TRUE)[1:20]

####NEW: correction of mistakes
Hobbit3_ud$lemma[Hobbit3_ud$lemma == "...our"] <- "our"
Hobbit3_ud$lemma[Hobbit3_ud$lemma == "...suffer"] <- "suffer"
Hobbit3_ud$lemma[Hobbit3_ud$lemma == "...upon"] <- "upon"
Hobbit3_freq <- table(Hobbit3_ud$lemma[Hobbit3_ud$upos %in% c("NOUN", "PROPN", "ADJ", "ADV", "VERB")])
sort(Hobbit3_freq, decreasing = TRUE)[1:20]


#Step 5. Turn the frequency vectors into dataframes and merge them together.
Hobbit1_df <- data.frame(Word = names(Hobbit1_freq), Hobbit1 = as.numeric(Hobbit1_freq))
head(Hobbit1_df)

Hobbit2_df <- data.frame(Word = names(Hobbit2_freq), Hobbit2 = as.numeric(Hobbit2_freq))
Hobbit3_df <- data.frame(Word = names(Hobbit3_freq), Hobbit3 = as.numeric(Hobbit3_freq))
LoR1_df <- data.frame(Word = names(LoR1_freq), LoR1 = as.numeric(LoR1_freq))
LoR2_df <- data.frame(Word = names(LoR2_freq), LoR2 = as.numeric(LoR2_freq))
LoR3_df <- data.frame(Word = names(LoR3_freq), LoR3 = as.numeric(LoR3_freq))


Tolkien <- merge(LoR1_df, LoR2_df, by = "Word", all = TRUE) 
Tolkien <- merge(Tolkien, LoR3_df, by = "Word", all = TRUE) 
Tolkien <- merge(Tolkien, Hobbit1_df, by = "Word", all = TRUE) 
Tolkien <- merge(Tolkien, Hobbit2_df, by = "Word", all = TRUE) 
Tolkien <- merge(Tolkien, Hobbit3_df, by = "Word", all = TRUE) 

head(Tolkien)

#Replace NA with 0
Tolkien[is.na(Tolkien)] <- 0
head(Tolkien)

#Step 6. Compute the distances between the films (from correlations) 
cor(Tolkien[, 2:7], method = "spearman")
Tolkien_dist <- as.dist(1 - cor(Tolkien[, 2:7], method = "spearman"))

#Step 7. Cluster the films.
Tolkien_clust <- hclust(Tolkien_dist, method = "average")
plot(Tolkien_clust)

#Try different clustering methods
Tolkien_clust <- hclust(Tolkien_dist, method = "complete")
plot(Tolkien_clust)
Tolkien_clust <- hclust(Tolkien_dist, method = "ward.D2")
plot(Tolkien_clust)

Tolkien_clust <- hclust(Tolkien_dist, method = "single")
plot(Tolkien_clust)

Tolkien_clust <- hclust(Tolkien_dist, method = "complete")
plot(Tolkien_clust)

###NEW: interpretation of the clusters!
#Question 1: which words occur in LoR and don't occur in The Hobbit?
#Let's make Word rownames and remove the first column:
rownames(Tolkien) <- Tolkien$Word 
Tolkien <- Tolkien[, -1]

#First, we compute the probabilities of every word in a particular film:
Tolkien_prob <- prop.table(data.matrix(Tolkien), 2)

#Next, we compute the average probabilities in LoR
LoR_meanprob <- rowMeans(Tolkien_prob[, 1:3])  

#The same for The Hobbit
Hobbit_meanprob <- rowMeans(Tolkien_prob[, 4:6])  

#Next, compute the log-odds (ratios):

film_ratio <- log((LoR_meanprob + 0.01)/(Hobbit_meanprob + 0.01))

#Negative log-odds: the ones where the words are more frequent in The Hobbit
sort(film_ratio)[1:20]

#Positive log-odds: the ones where the words are more frequent in LoR
sort(film_ratio, decreasing = TRUE)[1:20]

#Question 2: which words occur in LoR1 and don't occur in LoR2 and LoR3, and the other way round?

#First, we take the probabilities in LoR1
LoR1_meanprob <- Tolkien_prob[, 1]

#Next, we compute the average probabilities in LoR2 and LoR3
LoR23_meanprob <- rowMeans(Tolkien_prob[, 2:3])  

#Next, compute the log-odds (ratios):
LoR123_ratio <- log((LoR1_meanprob + 0.01)/(LoR23_meanprob + 0.01))

#Negative log-odds: the ones where the words are more frequent in LoR2 & 3
sort(LoR123_ratio)[1:20]

#Positive log-odds: the ones where the words are more frequent in LoR1
sort(LoR123_ratio, decreasing = TRUE)[1:20]

###Exercise:
#Find the words that are distinctive of Hobbit2 and Hobbit3 as opposed to Hobbit1


###Modifications of analysis
#Modification 1. What if we take only function words instead?

LoR1_freq <- table(LoR1_ud$lemma[LoR1_ud$upos %in% c("AUX", "DET", "ADP", "CCONJ", "SCONJ", "PRON", "PART")])
sort(LoR1_freq, decreasing = TRUE)[1:20]
LoR2_freq <- table(LoR2_ud$lemma[LoR2_ud$upos %in% c("AUX", "DET", "ADP", "CCONJ", "SCONJ", "PRON", "PART")])
LoR3_freq <- table(LoR3_ud$lemma[LoR3_ud$upos %in% c("AUX", "DET", "ADP", "CCONJ", "SCONJ", "PRON", "PART")])
Hobbit1_freq <- table(Hobbit1_ud$lemma[Hobbit1_ud$upos %in% c("AUX", "DET", "ADP", "CCONJ", "SCONJ", "PRON", "PART")])
Hobbit2_freq <- table(Hobbit2_ud$lemma[Hobbit2_ud$upos %in% c("AUX", "DET", "ADP", "CCONJ", "SCONJ", "PRON", "PART")])
Hobbit3_freq <- table(Hobbit3_ud$lemma[Hobbit3_ud$upos %in% c("AUX", "DET", "ADP", "CCONJ", "SCONJ", "PRON", "PART")])

Hobbit1_df <- data.frame(Word = names(Hobbit1_freq), Hobbit1 = as.numeric(Hobbit1_freq))
Hobbit2_df <- data.frame(Word = names(Hobbit2_freq), Hobbit2 = as.numeric(Hobbit2_freq))
Hobbit3_df <- data.frame(Word = names(Hobbit3_freq), Hobbit3 = as.numeric(Hobbit3_freq))
LoR1_df <- data.frame(Word = names(LoR1_freq), LoR1 = as.numeric(LoR1_freq))
LoR2_df <- data.frame(Word = names(LoR2_freq), LoR2 = as.numeric(LoR2_freq))
LoR3_df <- data.frame(Word = names(LoR3_freq), LoR3 = as.numeric(LoR3_freq))


Tolkien <- merge(LoR1_df, LoR2_df, by = "Word", all = TRUE) 
Tolkien <- merge(Tolkien, LoR3_df, by = "Word", all = TRUE) 
Tolkien <- merge(Tolkien, Hobbit1_df, by = "Word", all = TRUE) 
Tolkien <- merge(Tolkien, Hobbit2_df, by = "Word", all = TRUE) 
Tolkien <- merge(Tolkien, Hobbit3_df, by = "Word", all = TRUE) 

head(Tolkien)
Tolkien[is.na(Tolkien)] <- 0
head(Tolkien)

cor(Tolkien[, 2:7], method = "spearman")
Tolkien_dist <- as.dist(1 - cor(Tolkien[, 2:7], method = "spearman"))
Tolkien_clust <- hclust(Tolkien_dist, method = "average")
plot(Tolkien_clust)

#Modification 2. Compare the frequencies of parts of speech
LoR1_freq <- table(LoR1_ud$upos)
LoR1_freq
LoR2_freq <- table(LoR2_ud$upos)
LoR3_freq <- table(LoR3_ud$upos)
Hobbit1_freq <- table(Hobbit1_ud$upos)
Hobbit2_freq <- table(Hobbit2_ud$upos)
Hobbit3_freq <- table(Hobbit3_ud$upos)

Hobbit1_df <- data.frame(Word = names(Hobbit1_freq), Hobbit1 = as.numeric(Hobbit1_freq))
Hobbit2_df <- data.frame(Word = names(Hobbit2_freq), Hobbit2 = as.numeric(Hobbit2_freq))
Hobbit3_df <- data.frame(Word = names(Hobbit3_freq), Hobbit3 = as.numeric(Hobbit3_freq))
LoR1_df <- data.frame(Word = names(LoR1_freq), LoR1 = as.numeric(LoR1_freq))
LoR2_df <- data.frame(Word = names(LoR2_freq), LoR2 = as.numeric(LoR2_freq))
LoR3_df <- data.frame(Word = names(LoR3_freq), LoR3 = as.numeric(LoR3_freq))


Tolkien <- merge(LoR1_df, LoR2_df, by = "Word", all = TRUE) 
Tolkien <- merge(Tolkien, LoR3_df, by = "Word", all = TRUE) 
Tolkien <- merge(Tolkien, Hobbit1_df, by = "Word", all = TRUE) 
Tolkien <- merge(Tolkien, Hobbit2_df, by = "Word", all = TRUE) 
Tolkien <- merge(Tolkien, Hobbit3_df, by = "Word", all = TRUE) 

head(Tolkien)
Tolkien[is.na(Tolkien)] <- 0
head(Tolkien)

cor(Tolkien[, 2:7], method = "spearman")
Tolkien_dist <- as.dist(1 - cor(Tolkien[, 2:7], method = "spearman"))
Tolkien_clust <- hclust(Tolkien_dist, method = "average")
plot(Tolkien_clust)


#Simple Correspondence analysis
#Adjust the data a bit
rownames(Tolkien) <- Tolkien$Word 
Tolkien <- Tolkien[, -1]
Tolkien <- Tolkien[rowSums(Tolkien) > 50,]

#Install the package ca
library(ca)
plot(ca(Tolkien))

#remove SYM (symbols)
Tolkien <- Tolkien[rownames(Tolkien)!= "SYM",]
plot(ca(Tolkien))

###Exercises
#1. Cluster the films depending on particular parts of speech. Compare with our previous results.  
#2. Take any other texts from the Internet and cluster them.

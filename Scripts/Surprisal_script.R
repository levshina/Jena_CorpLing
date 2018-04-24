bigrams <- function(wordvector){
bigramvector <- character()  
for (i in 1:length(wordvector)){
bigramvector[i] <- paste(wordvector[i], wordvector[i+1], sep = " ")
} 
return (bigramvector[-length(wordvector)])
}

corpus_bigrams <- bigrams(corpus_words)

#compute the frequencies of each unique bigram
bigram_counts <- table(corpus_bigrams)

#check top 20 most frequent bigrams
sort(bigram_counts, decreasing = TRUE)[1:20]

#compute transitional probability manually
#e.g. P(the|is), where is is on the left
bigram_counts[names(bigram_counts) == "is the"]
word_counts[names(word_counts) == "is"]
142/1985 
#transform it into surprisal (Shannon information content)
-log2(142/1985) #in bits

#compute average surpisal of several wordforms given a word on the left
avsur <- function(wordlist, bigramfreqvector, unigramfreqvector){
avsur_vector <- numeric(length(wordlist))
for (j in 1:length(wordlist)){
word <- wordlist[j]
print (word)
n <- 0
surprisal_sum <- 0
for (i in 1:length(bigramfreqvector)){
bigram <- strsplit(names(bigramfreqvector[i]), split = " ")[[1]]
gram1 = bigram[1]
gram2 = bigram[2]
if (gram2 == word){
contextfreq <- unigramfreqvector[names(unigramfreqvector) == gram1]  
bigramfreq <- bigramfreqvector[i] 
surprisal_val <- -log2(bigramfreq/contextfreq)
surprisal_sum <- sum(surprisal_sum + surprisal_val*bigramfreq, na.rm = TRUE)
n <- n + bigramfreq
}
}
avsur_vector[j] <- as.numeric(surprisal_sum/n)
}
return (avsur_vector)  
}

avsur_speak <- avsur("speak", bigram_counts, word_counts)
  
avsur_the <- avsur("the", bigram_counts, word_counts)

avsur_intention <- avsur("intention", bigram_counts, word_counts)

avsur_all <- avsur(c("intention", "speak", "the"), bigram_counts, word_counts)

#make a wordlist 
mywordlist <- names(word_counts)

#draw a random sample of 10 words from the wordlist
set.seed(42)
wordsample <- mywordlist[sample(length(mywordlist), 10)]

avsur_sample <- avsur(wordsample, bigram_counts, word_counts)

#perform a correlation test (Spearman's rho based on ranks)
cor.test(avsur_sample, nchar(wordsample), method = "spearman")

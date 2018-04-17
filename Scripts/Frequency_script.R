corpus_words <- scan(file = file.choose(), what = "character") #read the file, split into wordforms (tokens) 

head(corpus_words) #first 6 tokens
tail(corpus_words) #last 6 tokens
length(corpus_words) #how many tokens are there?
corpus_words[1:50] #first 50 tokens

#fix problems with the apostrophe
corpus_words <- scan(file = file.choose(), what = "character", 
                    encoding = "UTF-8") #also to deal with non-ascii characters
corpus_words[1:50]

#remove numerals and punctuation marks in each word
#for ascii characters: replace all non-alphabetic characters and apostrophe with nothing
corpus_words <- gsub("[^[:alpha:]']", "", corpus_words)
head(corpus_words)
tail(corpus_words)

#for non-ascii characters: replace all punctuation marks and numbers
corpus_words <- gsub("[-.,!;:%'?*$@/0-9()]", "", corpus_words) #only for ascii characters
#replace all ascii characters
corpus_words <- gsub("[[:alpha:]]", "", corpus_words)

#remove empty strings and long strings (more than 20 letters)
corpus_words <- corpus_words[nchar(corpus_words, allowNA = TRUE) > 0 & nchar(corpus_words, allowNA = TRUE) < 21]
length(corpus_words)
head(corpus_words)

#use lower-case letters everywhere
corpus_words <- tolower(corpus_words)

##Test Zipf's law of abbreviation 
#1. Compute word frequencies
word_counts <- table(corpus_words)
head(word_counts)

#clean up a bit
word_counts <- word_counts[-c(1:3)]

#check top 20 most frequent strings
sort(word_counts, decreasing = TRUE)[1:20]
#check the least frequent strings (alphabetically arranged)
sort(word_counts, decreasing = FALSE)[1:20]

#visualize the distribution of frequencies 
plot(as.integer(sort(word_counts, decreasing = T)), type = "b")
#the same on the log scale
plot(log(as.integer(sort(word_counts, decreasing = T))), type = "b")

#2. Get word lengths
names(word_counts)[1:10]
word_lengths <- nchar(names(word_counts))
word_lengths[1:10]

#3. Perform correlation analysis
#visualize the relationship between word lengths and frequencies
plot(word_lengths, as.integer(word_counts))

#compute the Kendall tau and get the p-value
cor.test(word_counts, word_lengths, method = "kendall")

##For non-ascii characters (UTF-8): may be useful when representing characters:
Sys.getlocale(locale = "Russian") #for example

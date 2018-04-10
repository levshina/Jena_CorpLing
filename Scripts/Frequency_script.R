corpus_words <- scan(file = file.choose(), what = "character")
head(corpus_words)
tail(corpus_words)

#fix problems with the apostrophe
corpus_words <- scan(file = file.choose(), what = "character", 
                    encoding = "UTF-8")
corpus_words[1:30]

#remove punctuation marks in each word, and numerals
corpus_words <- gsub("[^[:alpha:]']", "", corpus_words)
tail(corpus_words)

#remove empty strings
corpus_words <- corpus_words[nchar(corpus_words) > 0 & nchar(corpus_words) < 15]

#use lower-case letters everywhere
corpus_words <- tolower(corpus_words)

#Compute word frequencies
word_counts <- table(corpus_words)
head(word_counts)

#clean up a bit
word_counts <- word_counts[-c(1:3)]

#top 20 most frequent strings
sort(word_counts, decreasing = T)[1:20]

#check if the distribution is Zipfian
plot(as.integer(sort(word_counts, decreasing = T)), type = "b")
plot(log(as.integer(sort(word_counts, decreasing = T))), type = "b")


#find the frequency of 'speak'
length(grep("speak", corpus_words))

#find all strings that contain 'speak'
corpus_words[grep("speak", corpus_words)]

#only 'speak', no other forms
length(grep("^speak$", corpus_words))
corpus_words[grep("^speak$", corpus_words)]

#find the contexts (sentences)
corpus_sent <- scan(file = file.choose(), what = "character", 
                    sep = "\n", encoding = "UTF-8")
head(corpus_sent)

#find all contexts with 'speak':
grep('speak', corpus_sent, ignore.case = TRUE)
corpus_sent[128] 

speak <- grep('speak', corpus_sent, ignore.case = TRUE)
corpus_sent[speak]

#substitute all instances of a string with another string
corpus_sent <- gsub("\t", " ", corpus_sent)

#split a string into tokens
strsplit(corpus_sent[3], " ")
unlist(strsplit(corpus_sent[3], " "))

#Test Zipf's law of abbreviation
names(word_counts)[1:5]
word_lengths <- nchar(names(word_counts))

plot(log(as.integer(word_counts)), word_lengths)
cor.test(word_counts, word_lengths, method = "spearman")

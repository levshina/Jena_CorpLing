##Finding individual contexts and tokens

#find all strings that contain 'speak'
corpus_words[grep("speak", corpus_words)]

#only 'speak', no other forms
corpus_words[grep("^speak$", corpus_words)]

#find the contexts (sentences)
corpus_sent <- scan(file = file.choose(), what = "character", 
                    sep = "\n", encoding = "UTF-8")
head(corpus_sent)

corpus_sent1 <- gsub("\t", " ", corpus_sent)

#to find speak as a separate word (newline, space, etc.)
corpus_sent1[grep("[[:space:]]speak[[:space:]]", corpus_sent1)]
#or more coarsely (only space)
corpus_sent1[grep(" speak ", corpus_sent1)]

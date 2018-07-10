fussball <- scan("Fussball_be_br.txt", what = "char", encoding = "UTF-8")
head(fussball)
#remove all strings with characters other than alphabetic
fussball <- gsub("[^[:alpha:]]", "", fussball)
#leave only non-empty strings (length > 0)
fussball <- fussball[nchar(fussball, allowNA = TRUE) > 0]
length(fussball)
#lower case
fussball <- tolower(fussball)

#make a table with frequencies of each word
fussball_counts <- table(fussball)
#look at 10 most frequent words
sort(fussball_counts, decreasing = TRUE)[1:10]
fussball_counts[names(fussball_counts) == "lukaku"]
#Who's more frequently mentioned, Neymar or de Bryune?
#Who are more often mentioned, Brazilians or Belgians? (Brazil or Belgium?)
#Check the most frequent words. Which are football terms?


#Let's make a wordcloud
fussball_top50 <- sort(fussball_counts, decreasing = TRUE)[1:50]
library(wordcloud) #you need to install it first!
mycol = c("blue", "green", "yellow", "red", "purple")
wordcloud(words = names(fussball_top50), freq = fussball_top50, colors = mycol, random.color = T)

#remove the function words and make another wordcloud
cbind(1:50, fussball_top50)
fussball_nofun <- fussball_top50[c(22, 24, 26, 30, 36, 39, 40, 46:49)]
wordcloud(words = names(fussball_nofun), freq = fussball_nofun, colors = mycol, random.color = T)

#task: make a selection of 100 top frequency words, select only the relevant words and make another wordcloud
#task: do the same for the match between England and Sweden
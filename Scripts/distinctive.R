coca <- scan(file = file.choose(), what = "character", sep = "\n",
                         encoding = "UTF-8")

##Saxon genitive: find the hosts and compute their frequencies

#find matching strings
saxon <- grep("'s\t's\tge", coca, ignore.case = TRUE, value = TRUE)
length(find_saxon)

#returns indices of matching elements
saxon <- grep("'s\t's\tge", coca, ignore.case = TRUE)
head(saxon)
coca[452]

#find the hosts (previous elements)
saxon_head <- coca[saxon - 1] 

#make a list
saxon_head_list <- strsplit(saxon_head, "\t")
head(saxon_head_list)

#list structure: compare
saxon_head_list[1]
is(saxon_head_list[1])
#and
saxon_head_list[[1]]
is(saxon_head_list[[1]])

#only proper and common nouns
all_nouns <- sapply(saxon_head_list, function(x) grepl(("nn1|nn2|np1|nn|np2"), x[3]))

summary(all_nouns)
length(which(all_nouns))

saxon_head_good <- saxon_head_list[which(all_nouns)]
length(saxon_head_good)

#collect all lemmas
saxon_lemmas <- sapply(saxon_head_good, function (x) x[[2]])

#get lemma frequencies
saxon_freq <- table(saxon_lemmas)
#top 20 most frequent hosts
sort(saxon_freq, decreasing = TRUE)[1:20]

##Norman genitive: find the hosts

norman <- grep("of\tof\tio", coca, ignore.case = TRUE)
length(norman)

find_norman <- function(coca, norman, n){
lemmas <- character(0)  
good <- c("nn1", "nn2", "nn", "np1", "np2")  
for (i in 1:length(norman)){
j <- norman[i]
k <- 1
while (k <= n){
cocaline <- unlist(strsplit(coca[j+k], "\t"))
if (cocaline[3] %in% good){
lemmas <- c(lemmas, cocaline[2])
break
}
else {k = k + 1}
}
} 
return (lemmas)
}

#get the list of heads (lemmas)
norman_lemmas <- find_norman(coca, norman, 5)

#get frequencies of heads
norman_freq <- table(norman_lemmas)

#top 20 most frequent heads
sort(norman_freq, decreasing = TRUE)[1:20]

###Distinctive collexeme analysis with log-odds ratio

#merge the heads of both genitive constructions in one data frame
saxon_df <- data.frame(saxon_freq)
norman_df <- data.frame(norman_freq)
genitive <- merge(saxon_df, norman_freq, by.x = "saxon_lemmas", by.y = "norman_lemmas", all = TRUE)
head(genitive)
#replace NA with 0
genitive[is.na(genitive)] <- 0
colnames(genitive) <- c("lemma", "freq_saxon", "freq_norman")

#obtain the two remaining frequencies for the table 
genitive$other_saxon <- sum(genitive$freq_saxon) - genitive$freq_saxon
genitive$other_norman <- sum(genitive$freq_norman) - genitive$freq_norman

#compute weighted log odds ratio based on 4 frequencies
genitive$logOR <- log((genitive$freq_saxon + 0.5)*(genitive$other_norman + 0.5)/((genitive$freq_norman + 0.5)*(genitive$other_saxon + 0.5)))

#top 50 collexemes attracted to the Saxon genitive
genitive[order(-genitive$logOR),][1:50,]

#top 50 collexemes attracted to the Norman genitive
genitive[order(genitive$logOR),][1:50,]
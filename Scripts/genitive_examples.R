#R code for finding examples and preparing the dataframes for replicating Szmrecsanyi et al.'s (2017) study of the genitive alternation  
#Author: Natalia Levshina
#Version: 22.05.2018

coca <- scan(file = file.choose(), what = "character", sep = "\n",
             encoding = "UTF-8")

#make a list, where each line is a vector separated by tabs
coca_list <- strsplit(coca, "\t")
head(coca_list)

#make simpler, keeping only the wordforms
coca_words <- sapply(coca_list, function (x) x[[1]])
head(coca_words)

#returns indices of matching elements (here: the Genitive s)
saxon <- grep("'s\t's\tge", coca, ignore.case = TRUE)
head(saxon)

#for each occurrence, return k words on the left and k words on the right from the marker
find_context <- function(corpus, indices, k){
contexts <- list(length(indices))  
for (i in 1:length(indices)){
index <- indices[i]  
neighbours_left <- integer(k)
neighbours_right <- integer(k)
j <- k
while (j > 0){
neighbours_left[k - j + 1]  <- index - j
neighbours_right[j]  <- index + j
j <- j - 1
}
all_indices <- c(neighbours_left, index, neighbours_right)
contexts[[i]] <- corpus[all_indices]
} 
return (contexts)
}

#find all contexts with Saxon genitives (a list with vectors corresponding to 10 words on the right and left from the marker in question)
saxon_contexts <- find_context(coca_words, saxon, 10)

#create sequences of words as one string (for easier reading)
saxon_sentences <- sapply(saxon_contexts, function(x) paste(x, collapse = " "))

#creating a data frame for manual coding
saxon_dataframe <- data.frame(sentence = saxon_sentences[1:100], response = rep("Saxon", 100), possessor = rep(NA, 100), possessed = rep(NA, 100), sibilant = rep(NA, 100), semrelation = rep(NA, 100), definite = rep(NA, 100))
#... code the variables manually

##Norman genitive: the same

norman <- grep("of\tof\tio", coca, ignore.case = TRUE)
length(norman)

norman_contexts <- find_context(coca_words, norman, 10)

norman_sentences <- sapply(norman_contexts, function(x) paste(x, collapse = " "))

norman_dataframe <- data.frame(sentence = norman_sentences[1:100], response = rep("Norman", 100), possessor = rep(NA, 100), possessed = rep(NA, 100), sibilant = rep(NA, 100), semrelation = rep(NA, 100), definite = rep(NA, 100))


#make a common dataframe
genitive_dataframe <- rbind(norman_dataframe, saxon_dataframe)

#write the data frame as a txt file to open in Excel or a similar application
write.table(genitive_dataframe, file = "genitive.txt", quote = FALSE)
#code the variables manually...

#Modelling the genitive alternations (which levels of the predictors are associated with which variant, other factors being controlled for)?

library(party) #install the package first
#Conditional inference tree
genitive_ctree <- ctree(response ~ possessor + possessed + sibilant + semrelation, data = genitive_dataframe)
plot(genitive_ctree)

#Conditional random forest
genitive_cforest <- cforest(response ~ possessor + possessed + sibilant + semrelation, data = genitive_dataframe)

#How important are the variables?
cforest_varimp <- varimp(genitive_cforest, conditional = TRUE)
dotchart(sort(cforest_varimp, decreasing = TRUE))
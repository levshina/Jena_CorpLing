#This script works with files in the Universal Dependencies format. 
#It allows one to investigate word order in syntactic dependencies.
#Author: Natalia Levshina
#Date: 12.6.2018

ud <- scan("en-ud-dev.conllu", what = "character", sep = "\n", comment.char = "#", encoding = "UTF-8")
head(ud)

#split each line by a tab
ud_list <- strsplit(ud, "\t")
head(ud_list)

#for each line (element in the list), get the dependency (element 8)
deps <- sapply(ud_list, function (x) x[8])
head(deps)
table(deps)

#the id of each word in the sentence
dep_id <- sapply(ud_list, function (x) as.numeric(x[1]))

#the id of the heads
head_id <- sapply(ud_list, function (x) as.numeric(x[7]))

deps.df <- data.frame(deps, dep_id, head_id)
head(deps.df)

deps.df$position <- ifelse(deps.df$dep_id < deps.df$head_id, "BeforeHead", "AfterHead")

#get the frequencies of each dependency before and after the head
table(deps.df$deps, deps.df$position)

#compute the proportions 
prop.table(table(deps.df$deps, deps.df$position), 1)

#make a separate vector only with proportions before head
propBefore <- prop.table(table(deps.df$deps, deps.df$position), 1)[, 2]

#sort the vector in descending order
sort(propBefore, decreasing = T)

#how to make sure that the dependencies has a certain POS (e.g. only nominal subjects)

dep_pos <- sapply(ud_list, function (x) x[4])
head(dep_pos)

deps.df$POS <- dep_pos
head(deps.df)

nsubj.df <- deps.df[deps.df$deps == "nsubj",]
nsubj.df$deps <- factor(nsubj.df$deps)
prop.table(table(nsubj.df$deps[nsubj.df$POS == "NOUN"], nsubj.df$position[nsubj.df$POS == "NOUN"]))

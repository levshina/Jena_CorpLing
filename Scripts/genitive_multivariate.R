#bivariate and multivariate analyses of the genitive alternation
#Author: Natalia Levshina
#Version: 05.06.2018

genitive3var <- read.table("genitive3var.txt", header = TRUE)
head(genitive3var)
summary(genitive3var)

#bivariate analyses

mytable <- table(genitive3var$Response, genitive3var$SemRelation)
mytable

barplot(mytable)
colors()
mycolors <- c("darkblue", "lightblue")
barplot(mytable, col = mycolors, ylim = c(0, 120))
legend("topleft", fill = mycolors, legend = levels(genitive3var$Response))

chisq.test(mytable)

#perform the same analysis for the two other predictors

#multivariate analyses
#conditional inference tree

library(party)
gen_ctree <- ctree(Response ~ Sibilant + SemRelation + Possessor, data = genitive3var)
plot(gen_ctree)


#random forest
gen_cforest <- cforest(Response ~ Sibilant + SemRelation + Possessor, data = genitive3var[complete.cases(genitive3var),])
gen_varimp <- varimp(gen_cforest, conditional = TRUE)
#this will last a while...
gen_varimp
dotchart(sort(gen_varimp))

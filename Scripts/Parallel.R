en_de <- read.table("Avatar_eng_deu.txt", sep = "\t", encoding = "UTF-8", quote = "")
head(en_de)

#Add column names
colnames(en_de) <- c("ID", "English", "German")

#Find instances of you in the English column (you should be a separate word!)
you <- grep("\\byou\\b", en_de[, 2], ignore.case = TRUE)
length(you)
head(you)
#Look at an example
en_de[48, 2]

#Find instances of "Sie" in the German column
Sie <- grep("\\bSie\\b", en_de[, 3], ignore.case = FALSE)
head(Sie)
en_de[36,]

#Find the sentences where you and Sie co-occur
you_Sie <- intersect(you, Sie)
head(you_Sie)

#check a couple of sentences, for example:
en_de[120,]

#how to add other forms, e.g. Ihnen:
Sie1 <- grep("\\b(Sie|Ihnen)\\b", en_de[, 3], ignore.case = FALSE)

length(Sie)
length(Sie1)
#the sentences in Sie1 that are not in Sie
setdiff(Sie1, Sie)
#an example:
en_de[135,]

#count the number of you corresponding to Sie/Ihnen:
you_Sie1 <- intersect(you, Sie1)
length(you_Sie1)
#130

#search for "du", "dich", "dir":
du <- grep("(\\b(du|dir|dich)\\b)", en_de[, 3], ignore.case = TRUE)
head(du)
length(du)
you_du <- intersect(you, du)
length(you_du)
#101

#search for "du", "dich", "dir":
ihr <- grep("(\\b(ihr|euch)\\b)", en_de[, 3], ignore.case = TRUE)
head(ihr)
length(ihr)
you_ihr <- intersect(you, ihr)
length(you_ihr)

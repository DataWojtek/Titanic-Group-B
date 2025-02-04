


install.packages("stringr")

library(stringr)







# Anreden extrahieren
anrede <- str_match(titanic$Name, ",\\s*([A-Za-z\\.]+)\\s")[,2]

print(anrede)


table(anrede)

# Gleiche Bedeutungen vereinen

anrede<-gsub("Mlle","Miss",anrede)
anrede<-gsub("Mme","Mrs",anrede)

table(anrede)

#  als Factor

titanic$Survived<- as.factor(titanic$Survived)
titanic$Sex<- as.factor(titanic$Sex)
titanic$Embarked<- as.factor(titanic$Embarked)

# 










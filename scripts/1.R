# make sure to set the working directory to the 'Titanic-Group-B' project folder
# Task 1

# import and install required packages
source("packages.R")
# stringr
# dplyr
# tibble

# import the dataset
titanic <- read.csv("data/titanic.csv")



### 1.1 Extract the forms of address out of the names and merge redundant ones

# Extract form of addresses
foa <- str_match(titanic$Name, ",\\s*([A-Za-z\\.]+)\\s")[,2]

# Overview over the raw forms of address
table(foa)

## Merge redundant forms of address

# french versions of Miss and Mrs
foa <- gsub("Mlle", "Miss", foa)
foa <- gsub("Mme", "Mrs" , foa)

# more neuter but still used for unmarried women
foa <- gsub("Ms", "Miss", foa)

# spanish version of Mr
foa <- gsub("Don", "Mr", foa)

# dutch version of Mr
foa <- gsub("Jonkheer", "Mr", foa)

# overview of the merged forms of address
table(foa)



### 1.2 Code survived, sex and embarked as factors

titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex      <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

### 1.3 Transfer Pclass to an ordered factor

titanic$Pclass <- factor(titanic$Pclass, levels = 1:3, ordered = TRUE)



### 1.4 Impute missing age values with the mean age of the form of address

# vector with all missing ages
na <- titanic[is.na(titanic$Age),]

# which forms of address contain missing ages?
table(str_match(na$Name, ",\\s*([A-Za-z\\.]+)\\s")[,2])


## doctors

# mean age of a doctor
mean_age_dr <- mean(na.omit(titanic$Age[grep(",\\s*Dr\\.",titanic$Name)])) # 42

# impute missing ages of doctors as 42
titanic <- titanic %>%
  mutate(Age = ifelse(grepl(",\\s*Dr\\.", Name) & is.na(Age), mean_age_dr, Age))


## masters

# mean age of a master
mean_age_master <- mean(na.omit(
                        titanic$Age[grep(",\\s*Master\\.",titanic$Name)])) # ~5

# impute the missing ages of masters with 5
titanic <- titanic %>%
  mutate(Age = ifelse(grepl(",\\s*Master\\.", Name) & 
                            is.na(Age), round(mean_age_master), Age))


## misses

# mean age of a miss
mean_age_miss <- mean(na.omit(titanic$Age[grep(",\\s*Miss\\.",titanic$Name)]))
# ~ 22

# impute the missing ages of misses with 22
titanic <- titanic %>%
  mutate(Age = ifelse(grepl(",\\s*Miss\\.", Name) & 
                        is.na(Age), round(mean_age_miss), Age))


## misters

# mean age of misters
mean_age_mr <- mean(na.omit(titanic$Age[grep(",\\s*Mr\\.",titanic$Name)]))
# ~ 32

# impute the mssing ages of misters with 32
titanic <- titanic %>%
  mutate(Age = ifelse(grepl(",\\s*Mr\\.", Name) & 
                        is.na(Age), round(mean_age_mr), Age))


## missis / missus

# mean age of Mrs
mean_age_mrs <- mean(na.omit(titanic$Age[grep(",\\s*Mrs\\.",titanic$Name)]))
# ~ 36

# impute the missing ages of mrs with 36
titanic <- titanic %>%
  mutate(Age = ifelse(grepl(",\\s*Mrs\\.", Name) & 
                        is.na(Age), round(mean_age_mrs), Age))


# check if there are still missing ages
any(is.na(titanic$Age)) # FALSE, ergo no missing ages



### 1.5 Extract information from the variable cabin


## 1.5.1 port or starboard

side <- as.numeric(substr((titanic$Cabin), 
                              nchar((titanic$Cabin)), nchar((titanic$Cabin))))
# "p" for port, "s" for starboard
side <- ifelse(side %% 2 == 0, "p", "s")

# add side as a column to the dataframe
titanic <- add_column(titanic, side = side, .after = "Cabin")


## 1.5.2 deck

# deck as the letter of the deck, mssing deck as NA
deck <- as.character(substr((titanic$Cabin), 1, 1))
deck[deck == ""] <- NA

# add deck as a column to the dataframe
titanic <- add_column(titanic, deck = deck, .after = "Cabin")



### 1.6 delete PassengerId, Name, Ticket and Cabin

titanic <- titanic %>% select(-PassengerId, -Name, -Ticket, -Cabin)



### 1.7 save the processed dataframe

## NOTE: Make sure that you set the working directory to the 'Titanic-Group-B'
#        project folder. 
# Check the working directory with:
# getwd()
# It should be "your/path/yourGitHubFolder/Titanic-Group-B"
#
# Only if you are certain you set the working directory right AND 
# you want to check if it works (otherwise the RData-file should be in the
# 'data' folder) 
# you can proceed

# save(titanic, file = "data/processed_titanic.RData")

# make sure to set the working directory to the 'Titanic-Group-B' project folder
# Task 4

# import and install required packages
source("~/GitHub/Titanic-Group-B/packages.R")


# load the processed dataframe
load("~/GitHub/Titanic-Group-B/data/processed_titanic.RData")

source("~/GitHub/Titanic-Group-B/scripts/2_1.R")


# Convert necessary columns to factors (categorical variables)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

# make sure to set the working directory to the 'Titanic-Group-B' project folder
# Task 4

# import and install required packages
source("packages.R")


# load the processed dataframe
load("data/processed_titanic.RData")



### Task 4

# Import relevant scripts containing functions
source("scripts/2_1.R") # Functions for descriptive analysis
source("scripts/2_2.R") # Helper functions

# Ensure categorical variables are factors
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

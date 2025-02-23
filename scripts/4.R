# make sure to set the working directory to the 'Titanic-Group-B' project folder
# Task 4

# import and install required packages
source("~/GitHub/Titanic-Group-B/packages.R")

# load the processed dataframe
load("~/GitHub/Titanic-Group-B/data/processed_titanic.RData")

# import the functions from script 2_1
source("~/GitHub/Titanic-Group-B/scripts/2_1.R")

# import internal functions 
source("~/GitHub/Titanic-Group-B/scripts/2_2.R")


### Task 4


### Task 4.1 Ensure categorical variables are factors
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

### Task 4.2 Descriptive Statistics for Metric Variables 

cat("\n--- Descriptive Statistics for Metric Variables ---\n")
metric()


### Task 4.3 Descriptive Statistics for Categorical Variables

cat("\n--- Descriptive Statistics for Categorical Variables ---\n")
describe_categorical_vars()


### Task 4.4 Relationship Between Two Categorical Variables
# Display mean survival rate by sex


cat("\n--- Relationship: Survival by Sex ---\n")
cat("Mean survival rate by sex:\n")
print(tapply(as.numeric(as.character(titanic$Survived)), titanic$Sex, mean, na.rm = TRUE))
categorical_relationship("Sex", "Survived")


# Display mean survival rate by passenger class
cat("\n--- Relationship: Survival by Passenger Class ---\n")
cat("Mean survival rate per passenger class:\n")
print(tapply(as.numeric(as.character(titanic$Survived)), titanic$Pclass, mean, na.rm = TRUE))
categorical_relationship("Pclass", "Survived")


# Display mean survival rate by embarkation point
cat("\n--- Relationship: Survival by Embarked Location ---\n")
cat("Mean survival rate per embarkation point:\n")
print(tapply(as.numeric(as.character(titanic$Survived)), titanic$Embarked, mean, na.rm = TRUE))
categorical_relationship("Embarked", "Survived")


### Task 4.5 Relationship Between a Metric and a Dichotomous Variable 

cat("\n--- Testing Age Difference Between Survivors and Non-Survivors ---\n")
age_difference <- stat_age_survived()
if (age_difference) {
    print("There is a significant difference in age between survivors and non-survivors.")
} else {
    print("No significant difference in age between survivors and non-survivors.")
}

# Display mean age of survivors vs. non-survivors
cat("\nMean age of survivors:\n", mean(titanic$Age[titanic$Survived == 1], na.rm = TRUE), "\n")
cat("Mean age of non-survivors:\n", mean(titanic$Age[titanic$Survived == 0], na.rm = TRUE), "\n")


### Task 4.6 Visualizations 

cat("\n--- Generating Visualizations ---\n")

# Survival distribution by class and gender
cat("Generating visualization: Survival by class and gender...\n")
visualize_categorical()

# Age distribution by survival status
cat("Generating visualization: Age distribution by survival status...\n")
plot_age_distribution()

# Boxplot: Age distribution Across Survival Status
ggplot(titanic, aes(x = as.factor(Survived), y = Age, fill = as.factor(Survived))) +
    geom_boxplot() +
    labs(title = "Age Distribution by Survival Status", x = "Survived", y = "Age") +
    theme_minimal()

# Boxplot: Fare distribution across survival status
ggplot(titanic, aes(x = as.factor(Survived), y = Fare, fill = as.factor(Survived))) +
    geom_boxplot() +
    labs(title = "Fare Distribution by Survival Status", x = "Survived", y = "Fare") +
    theme_minimal()

# Bar plot: Survival rate by Embarked location
ggplot(titanic, aes(x = Embarked, fill = as.factor(Survived))) +
    geom_bar(position = "fill") +
    labs(title = "Survival Rate by Embarkation Point", x = "Embarked", y = "Proportion") +
    scale_fill_discrete(name = "Survived") +
    theme_minimal()

cat("\n Analysis Completed! \n")
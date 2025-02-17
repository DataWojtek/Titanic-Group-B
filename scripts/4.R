# make sure to set the working directory to the 'Titanic-Group-B' project folder
# Task 4

# import and install required packages
source("~/GitHub/Titanic-Group-B/packages.R")


# load the processed dataframe
load("~/GitHub/Titanic-Group-B/data/processed_titanic.RData")

source("~/GitHub/Titanic-Group-B/scripts/2_1.R")



### Task 4


# Import relevant scripts containing functions
source("scripts/2_1.R") # Functions for descriptive analysis
source("scripts/2_2.R") # Helper functions

# Ensure categorical variables are factors
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

### --- Descriptive Statistics for Metric Variables ---
cat("\n--- Descriptive Statistics for Metric Variables ---\n")
metric()

### --- Descriptive Statistics for Categorical Variables ---
cat("\n--- Descriptive Statistics for Categorical Variables ---\n")
describe_categorical_vars()

### --- Relationship Between Two Categorical Variables ---
cat("\n--- Relationship: Survival by Sex ---\n")
categorical_relationship("Sex", "Survived")

cat("\n--- Relationship: Survival by Passenger Class ---\n")
categorical_relationship("Pclass", "Survived")

cat("\n--- Relationship: Survival by Embarked Location ---\n")
categorical_relationship("Embarked", "Survived")

### --- Relationship Between a Metric and a Dichotomous Variable ---
cat("\n--- Testing Age Difference Between Survivors and Non-Survivors ---\n")
age_difference <- stat_age_survived()
if (age_difference) {
    print("There is a significant difference in age between survivors and non-survivors.")
} else {
    print("No significant difference in age between survivors and non-survivors.")
}

### --- Visualizations ---
cat("\n--- Generating Visualizations ---\n")

# Survival distribution by class and gender
visualize_categorical()

# Age distribution by survival status
plot_age_distribution()

# Boxplot: Age distribution across different classes
ggplot(titanic, aes(x = Pclass, y = Age, fill = Pclass)) +
    geom_boxplot() +
    labs(title = "Age Distribution by Passenger Class", x = "Passenger Class", y = "Age") +
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

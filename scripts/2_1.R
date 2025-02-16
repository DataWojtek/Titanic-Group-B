# make sure to set the working directory to the 'Titanic-Group-B' project folder
# Task 2.1 / 2 a)

# import and install required packages
source("~/GitHub/Titanic-Group-B/packages.R")


# load the processed dataframe
load("~/GitHub/Titanic-Group-B/data/processed_titanic.RData")

# load the file
source("~/GitHub/Titanic-Group-B/scripts/2_2.R")


### 2.1.1 - Descriptive statistics for metric variables

metric <- function() {
  # titanic$Survived
  print("Number of survivers")
  print(sum(as.numeric(titanic$Survived) - 1))
  print("Number of survivers in percent")
  wert <- round((mean(as.numeric(titanic$Survived) - 1)) * 100, 1)
  print(paste0(wert, "%"))

  # prints the number of survivers and the percentage of all pasengers, that survived

  # titanic$Pclass
  print("Average class")
  print(mean(as.numeric(titanic$Pclass)))
  print("Distribution of classes")
  print("1       2       3")
  print(paste0(round(prop.table(table(titanic$Pclass)), 2), "%"))

  # prints the  aveerage class, aswell as the distribution of all classes on board

  # titanic$Age
  print("Range of Ages")
  print(paste("Min:", min(titanic$Age), "| Max:", max(titanic$Age)))
  print("Average age")
  print(mean(titanic$Age))
  print("Most commom age")
  print(median(titanic$Age))

  # prints the min and max age of the passengers and the mean and median age

  # titanic$SibSp
  print("Range of siblings and spouses brought")
  print(paste("Min:", min(titanic$SibSp), "| Max:", max(titanic$SibSp)))
  print("Average number of siblings and spouses brought")
  print(mean(titanic$SibSp))
  print("Most commom number of siblings and spouses brought")
  print(median(titanic$SibSp))

  # prints the min, max mean and modus of the number of spouses or siblings brought

  # titanic$Parch
  print("Range of parents and children brought")
  print(paste("Min:", min(titanic$Parch), "| Max:", max(titanic$Parch)))
  print("Average number of parents and children brought")
  print(mean(titanic$Parch))
  print("Most commom number of parents and children brought")
  print(median(titanic$Parch))

  # prints the min, max mean and modus of the number of parents and children brought

  # titanic$Fare
  print("Range of ticket costs")
  print(paste("Min:", min(titanic$Fare), "| Max:", max(titanic$Fare)))
  print("Average ticket cost")
  print(mean(titanic$Fare))
  print("Average cost for first class")
  print(mean(titanic[titanic$Pclass == 1, ]$Fare))
  print("Average cost for second class")
  print(mean(titanic[titanic$Pclass == 2, ]$Fare))
  print("Average cost for third class")
  print(mean(titanic[titanic$Pclass == 3, ]$Fare))

  # prints the range of prices, aswell as the average price for all
  # classes and individual classes
}


### 2.1.2 - Descriptive statistics for categorial variables
describe_categorical_vars <- function() {
  # Define a vector containing the categorical variables to analyze
  categorical_vars <- c("Sex", "Pclass", "Embarked", "Survived")

  # Loop through each categorical variable
  for (var in categorical_vars) {
    # Print the name of the variable being analyzed
    print(paste("Distribution of", var))

    # Print the frequency distribution of the variable, including NA values
    print(table(titanic[[var]], useNA = "ifany"))

    # Print a label before showing the percentage distribution
    print("Percentage distribution")

    # Calculate and print the proportion of each category
    print(prop.table(table(titanic[[var]])))
  }
}


### 2.1.3 - Descriptive statistic for the relationship between two categorial
###         variables
categorical_relationship <- function(var1, var2) {
  print(paste("Contingency table for", var1, "and", var2))
  contingency_table <- table(titanic[[var1]], titanic[[var2]])
  print(contingency_table)

  print("Chi-square test results")
  chi_test <- chisq.test(contingency_table)
  print(chi_test)
}

categorical_relationship("Sex", "Survived")
categorical_relationship("Pclass", "Survived")

### 2.1.4 - Descriptive statistic for the relationship between a metric and a
###         dichotomous variable

## stat_age_survived - a function which provides descriptive statistics for
##                     the correlation between the age of the person and
##                     if they survived
##
## input:
## alpha - alpha significance level for the tests, normally at 0.05
##
##
## output:
## age_dors_diff - a boolean value which indicates if there is
##                 a significant difference between the groups
##                 of the ages which survived and died
##
##

stat_age_survived <- function(alpha = 0.05) {
  # check input
  stopifnot(
    is.numeric(alpha), is.vector(alpha), length(alpha) == 1, alpha > 0,
    alpha <= 1
  )

  # divide the age in survived or not survived
  age_died <- titanic$Age[titanic$Survived == 0]
  age_survived <- titanic$Age[titanic$Survived == 1]

  # list for the simple stats for both groups (maybe a candidate for an internal
  # function)
  # 1 stands for died and 2 for survived

  age_dors <- stats_simple(age_died, age_survived)
  # $means = 30.63, 28.26
  # $medians = 32, 28
  # $vars = 160.10, 198.19
  # $sds = 12.65, 14.10

  ## t-test to check if there are significant differences
  # dependencies for the tTest

  # 1. independence of values
  # the independence of the groups can be assumed independent because
  # there is no direct focus on couples, familys, etc. in this analysis

  # 2. normal distribution with the shapiro-wilk-test

  # is TRUE if both p-values are above alpha -> normal distribution
  age_dors_nd <- all(c(
    shapiro.test(age_died)$p.value,
    shapiro.test(age_survived)$p.value
  ) > alpha)

  # 3. homogeneity of variances
  # is TRUE if the pvalue lies above alpha
  age_dors_hov <- var.test(Age ~ Survived, data = titanic)$p.value > alpha



  ## use the correct test
  if (age_dors_nd == TRUE) {
    age_dors_diff <- t.test(age_died,
      age_survived,
      var.equal = age_dors_hov
    )$p.value < alpha
  } else {
    age_dors_diff <- wilcox.test(age_died, age_survived)$p.value < alpha
  }

  # age_dors_diff is TRUE if there is a significant difference between the
  # ages of the passengers that died and those who survived

  return(age_dors_diff)
}



### 2.1.5 - Visualization for three or four categorial variables

## to visualize the survival distribution of Titanic passengers based on passenger class and gender.

visualize_categorical <- function() {
  library(ggplot2)
  ggplot(titanic, aes(x = Pclass, fill = Sex)) +
    geom_bar(position = "dodge") +

    facet_wrap(~ Survived) +
    labs(title = "Survival Rate by Class and Gender", x = "Passenger Class", y = "Count")
}

visualize_categorical()

### 2.1.6 - Optional: Additional functions for descriptive statistics or
###         visualizations
plot_age_distribution <- function() { # Define a function to plot age distribution
  library(ggplot2) # Load the ggplot2 package for visualization

  ggplot(titanic, aes(x = Age, fill = as.factor(Survived))) + # Create a ggplot with Age on x-axis and color by Survived status
    geom_histogram(binwidth = 5, alpha = 0.7, position = "identity") + # Plot a histogram with bins of width 5, set transparency, and overlay bars
    labs(
      title = "Age Distribution by Survival Status", # Set the title of the plot
      x = "Age", # Label for x-axis
      y = "Count", # Label for y-axis
      fill = "Survived"
    ) # Legend label for the fill color
}

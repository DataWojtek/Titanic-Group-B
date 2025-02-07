# make sure to set the working directory to the 'Titanic-Group-B' project folder
# Task 2.1 / 2 a)

# import and install required packages
source("packages.R")


# load the processed dataframe
load("data/processed_titanic.RData")

# load the file
source("scripts/2_2.R")


### 2.1.1 - Descriptive statistics for metric variables



### 2.1.2 - Descriptive statistics for categorial variables



### 2.1.3 - Descriptive statistic for the relationship between two categorial
###         variables



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

stat_age_survived <- function(alpha = 0.05)
{
  # check input
  stopifnot(is.numeric(alpha), is.vector(alpha), length(alpha) == 1, alpha > 0,
            alpha <= 1)
  
  # divide the age in survived or not survived
  age_died     <- titanic$Age[titanic$Survived == 0]
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
  age_dors_nd <- all(c(shapiro.test(age_died)$p.value,
                       shapiro.test(age_survived)$p.value) > alpha)
  
  # 3. homogeneity of variances
  # is TRUE if the pvalue lies above alpha
  age_dors_hov <- var.test(Age ~ Survived, data = titanic)$p.value > alpha
  
  
  
  ## use the correct test
  if(age_dors_nd == TRUE)
  {
    age_dors_diff <- t.test(age_died, 
                            age_survived, 
                            var.equal = age_dors_hov)$p.value < alpha
  }
  else
  {
    age_dors_diff <- wilcox.test(age_died, age_survived)$p.value < alpha
  }
  
  # age_dors_diff is TRUE if there is a significant difference between the
  # ages of the passengers that died and those who survived
  
  return(age_dors_diff)
}



### 2.1.5 - Visualization for three or four categorial variables



### 2.1.6 - Optional: Additional functions for descriptive statistics or 
###         visualizations



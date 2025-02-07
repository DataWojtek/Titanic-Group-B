# make sure to set the working directory to the 'Titanic-Group-B' project folder
# Task 2.2 / 2 b)

# This is the file for internal functions (iF)

## stats_simple - creates a list with the means, medians, vars and sds of the 
##                given vectors and returns them in a list
##
## input:
## na - a logical value which indicates if NAs should be ignored
##
## output:
## a list with 4 numeric vectors
## means - the means of the given vectors
## medians - the medians of the given vectors
## vars - the variances of the given vectors
## sds - the standard deviations of the given vectors
##

stats_simple <- function(..., na = TRUE)
{
  # check input
  stopifnot(is.logical(na))
  
  # collect the vectors
  vectors <- list(...)
  
  # calculate the simple descriptive stats for all vectors
  means <- sapply(vectors, function(vec) mean(vec, na.rm = na))
  medians <- sapply(vectors, function(vec) median(vec, na.rm = na))
  vars <- sapply(vectors, function(vec) var(vec, na.rm = na))
  sds <- sapply(vectors, function(vec) sd(vec, na.rm = na))
  
  # return the stats as a list
  return(list(means = means, medians = medians, vars = vars, sds = sds))
}






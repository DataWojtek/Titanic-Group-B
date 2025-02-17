# packages.R - this file imports and installs if needed the required packages
#              for this project

# vector of the required packages of this project
required_packages <- c("stringr", "dplyr", "tibble", "ggplot2")

# checks which packages are already installed
new_packages <- required_packages[!(required_packages %in% 
                                    installed.packages()[,"Package"])]

# installs the new packages if there are any
if(length(new_packages)) 
{
  install.packages(new_packages)
}

# imports the required packages
lapply(required_packages, library, character.only = TRUE)
##########################################################
# Create alzheimer set, validation set (final hold-out test set)
##########################################################

# Loading libraries used in later analysis
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(readr)) install.packages("readr")

library(tidyverse)
library(caret)
library(readr)

data <- read_csv("https://raw.githubusercontent.com/dsmiddendorf/Alzheimers-Disease-Capstone-Project/main/oasis_cross-sectional.csv")
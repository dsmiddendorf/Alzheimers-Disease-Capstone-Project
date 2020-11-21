##########################################################
# Create alzheimer set, validation set (final hold-out test set)
##########################################################

# Downloading required libraries for analysis if they have not been downloaded yet
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(readr)) install.packages("readr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(psych)) install.packages("psych")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(cowplot)) install.packages("cowplot")
if(!require(corrplot)) install.packages("corrplot")

# Loading libraries used for the following analysis
library(tidyverse)
library(caret)
library(readr)
library(ggplot2)
library(psych)
library(ggthemes)
library(cowplot)
library(corrplot)

# Loading data 
dat <- read_csv("https://raw.githubusercontent.com/dsmiddendorf/Alzheimers-Disease-Capstone-Project/main/oasis_longitudinal.csv")
dat[,c("Hand")] <- NULL

# Change variable name for sex from "M/F" to "Sex"
names(dat)[6] <- "Sex" 

# Validation set will be 10% of the original dataset
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = dat$Group, times = 1, p = 0.1, list = FALSE)
alzheimer <- dat[-test_index,]
validation <- dat[test_index,]

rm(test_index, dat)



##########################################################
# Data Exploration
##########################################################

## Descriptive Statistics
# Column names
names(alzheimer)

# General summary of the alzheimer data
describe(alzheimer[,2:9])

# Gender distribution
table(alzheimer$Sex)/nrow(alzheimer)

# Number of rows and columns in the alzheimer dataset
dim(alzheimer)

# Number of Participants
length(unique(alzheimer$`Subject ID`))

# Replace missing values (NA) with median values
alzheimer$SES[is.na(alzheimer$SES)] <- median(alzheimer$SES, na.rm = T)
alzheimer$MMSE[is.na(alzheimer$MMSE)] <- median(alzheimer$MMSE, na.rm = T)

## Visualization
# Create Density Diagrams for continuous variables
dage <- alzheimer %>% ggplot(aes(Age)) + geom_density() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

deduc <- alzheimer %>% ggplot(aes(EDUC)) + geom_density() + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

dmmse <- alzheimer %>% ggplot(aes(MMSE)) + geom_density() + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

detiv <- alzheimer %>% ggplot(aes(eTIV)) + geom_density() + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

dnwbv <- alzheimer %>% ggplot(aes(nWBV)) + geom_density() + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

dasf <- alzheimer %>% ggplot(aes(ASF)) + geom_density() + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

density_plots <- plot_grid(dage, deduc, dmmse, detiv, dnwbv, dasf)
rm(dage, deduc, dmmse, detiv, dnwbv, dasf)

# Create barplots for categorical/ordinal data
x <- alzheimer %>% ggplot(aes(Sex)) + geom_bar() + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
y <- alzheimer %>% ggplot(aes(SES)) + geom_bar() + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
z <- alzheimer %>% ggplot(aes(CDR)) + geom_bar() + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
bar_plots <- plot_grid(x,y,z)
rm(x,y,z)

 
# Correlation matrix between numerical variables
pval <- psych::corr.test(alzheimer[,c(5,7,8,9,10,11,12,13,14)], adjust="none")$p
pval[pval >= .05] <- NA
corrplot(cor(alzheimer[,c(5,7,8,9,10,11,12,13,14)]), type="upper", p.mat=pval, insig="p-value", 
         tl.pos="n", sig.level=0)

corrplot(cor(alzheimer[,c(5,7,8,9,10,11,12,13,14)]), type="lower", add=T, tl.pos="d", cl.pos="n", addCoef.col = T)
?corrplot
# → It can be seen that many variables are highly & significantly correlated with each other. 
# We have to check for multicollinearity to conclude which parameters to include in our final model.

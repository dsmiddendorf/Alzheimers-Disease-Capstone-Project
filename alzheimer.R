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
if(!require(glmnet)) install.packages("glmnet")

# Loading libraries used for the following analysis
library(tidyverse)
library(caret)
library(readr)
library(ggplot2)
library(psych)
library(ggthemes)
library(cowplot)
library(corrplot)
library(glmnet)

# Loading data 
dat <- read_csv("https://raw.githubusercontent.com/dsmiddendorf/Alzheimers-Disease-Capstone-Project/main/oasis_longitudinal.csv")
dat[,c("Hand")] <- NULL # Removing column including handedness as all participants were dextrals

# Change variable name for sex from "M/F" to "Sex"
names(dat)[6] <- "Sex" 

# Validation set will be 10% of the original dataset
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = dat$Group, times = 1, p = 0.1, list = FALSE)
alzheimer <- dat[-test_index,]
validation <- dat[test_index,]
rm(test_index, dat) # Removing original dataset and test index





##########################################################
# Data Exploration
##########################################################



######### Descriptive Statistics ##########

# Column names
names(alzheimer)

# General summary of the alzheimer data
head(alzheimer)
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
validation$SES[is.na(validation$SES)] <- median(validation$SES, na.rm = T)
validation$MMSE[is.na(validation$MMSE)] <- median(validation$MMSE, na.rm = T)



######### Visualization ##########

# Create Density Diagrams for continuous variables

## Density Plot for Age
dage <- alzheimer %>% ggplot(aes(Age)) + geom_density() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Density Plot for Education
deduc <- alzheimer %>% ggplot(aes(EDUC)) + geom_density() + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

## Density Plot for MMSE (Mini Mental Status Examination)
dmmse <- alzheimer %>% ggplot(aes(MMSE)) + geom_density() + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

## Density Plot for eTIV
detiv <- alzheimer %>% ggplot(aes(eTIV)) + geom_density() + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

## Density Plot for nWBV
dnwbv <- alzheimer %>% ggplot(aes(nWBV)) + geom_density() + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

## Density Plot for ASF
dasf <- alzheimer %>% ggplot(aes(ASF)) + geom_density() + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

# Putting all Density Plots together in one Figure
density_plots <- plot_grid(dage, deduc, dmmse, detiv, dnwbv, dasf)
density_plots
rm(dage, deduc, dmmse, detiv, dnwbv, dasf)


# Create barplots for categorical/ordinal data

## Barplot for Sex
x <- alzheimer %>% ggplot(aes(Sex)) + geom_bar() + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

## Barplot for Socioeconomic Status
y <- alzheimer %>% ggplot(aes(SES)) + geom_bar() + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

## Barplot for CDR
z <- alzheimer %>% ggplot(aes(CDR)) + geom_bar() + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
bar_plots <- plot_grid(x,y,z)
rm(x,y,z)



######### Bivariate Examination of the Data ##########

# Correlation matrix between numerical variables
pval <- psych::corr.test(alzheimer[,c(5,7,8,9,10,11,12,13,14)], adjust="none")$p # Calculating p-values for the different correlations
pval[pval >= .05] <- NA # Removing p-values above .05
corrplot(cor(alzheimer[,c(5,7,8,9,10,11,12,13,14)]), type="upper", p.mat=pval, insig="p-value", 
         tl.pos="n", sig.level=0) # Plotting the upper half of the correlation matrix with p-values
corrplot(cor(alzheimer[,c(5,7,8,9,10,11,12,13,14)]), type="lower", add=T, tl.pos="d", cl.pos="n", addCoef.col = T) # Plotting the lower part with correlations
rm(pval)
# -> It can be seen that many variables are highly & significantly correlated with each other. 
# We have to check for multicollinearity to conclude which parameters to include in our final model.

# Checking if the variable Sex and CDR are independent with a Chi-Square Test (they aren't)
chisq.test(alzheimer$Sex, alzheimer$CDR)

# Are the variables CDR and Group the same?
## Creating a barplot of Group ('converted', 'demented', 'Nondemented') filled by CDR
alzheimer %>% ggplot(aes(Group, fill = as.factor(CDR))) + geom_bar() + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
## Selecting two participants who would not fit if both variables would be the same
alzheimer[which(alzheimer$CDR == .5 & alzheimer$Group == "Nondemented"),] 
# -> It seems like CDR and Group are the same variable; group is just a collapsed form
# Nevertheless, there are two entries that do not fit. 





##########################################################
# Data Wrangling
##########################################################

# Collapsing the CDR variable into 0, Nondemented and 1 Demented, new variable is collapsed CDR (cCDR)
alzheimer$cCDR <- ifelse(alzheimer$CDR %in% c(0.5,1,2), 1, 0)
alzheimer$cCDR <- as.factor(alzheimer$cCDR)
alzheimer$CDR <- as.factor(alzheimer$CDR)

validation$cCDR <- ifelse(validation$CDR %in% c(0.5,1,2), 1, 0)
validation$cCDR <- as.factor(validation$cCDR)
validation$CDR <- as.factor(validation$CDR)

# Checking for Multicollinearity
numeric_variables <- alzheimer[, sapply(alzheimer, is.numeric)]
correlations<-cor(numeric_variables)
highCorr<-findCorrelation(correlations, cutoff = .75)
numeric_variables<-numeric_variables[,highCorr] # Dropping ASF and 'MR Delay' because of Multicollinearity
rm(correlations, numeric_variables, highCorr)





##########################################################
# Building the Models
#########################################################

######### Just the Average ##########

# Predicting cCDR of the Validation set with the mean cCDR of the training (alzheimer) dataset
averagecCDR <- mean(as.numeric(as.character(alzheimer$cCDR)))
# RMSE of this prediction by the average
RMSE_Average <- sqrt(mean((as.numeric(as.character(validation$cCDR)) - averagecCDR)^2))
rm(averagecCDR) # Remove Average of cCDR

######### Simple Logistic Regression Model ##########
# Calculating a first simple logistic regression model
model <- glm(cCDR ~ Sex + Age + EDUC + SES+ nWBV, family = "binomial", data = alzheimer)
plot.dat <- predict(model, validation)
# RMSE of this theory based simple logistic regression model
RMSE_Logistic <- sqrt(mean(as.numeric(as.character(validation$cCDR)) - exp(plot.dat)/(1+exp(plot.dat)))^2)

# Plotting a Graph for our Model
  # Creating a new dataframe containing the probabilities of having Dementia along with the actual dementia status
predicted.data <- data.frame(
  probability.of.cCDR=model$fitted.values,
  cCDR=alzheimer$cCDR)

  # Sort dataframe from low to high probabilities
predicted.data <- predicted.data[
  order(predicted.data$probability.of.cCDR, decreasing=F),]
  # Add a new column to the dataframe that has the rank of each sample, from low to high probability
predicted.data$rank <- 1:nrow(predicted.data)
  # Actually plotting the graph 
ggplot(data=predicted.data, aes(x=rank, y=probability.of.cCDR)) +
  geom_point(aes(color=cCDR), alpha=1, shape=4, stroke=2) + 
  xlab("Index") + 
  ylab("Predicted probability of getting Dementia")

rm(plot.dat, model, predicted.data) # Remove predicted values & model for the simple logistic regression model



######### KNN Model ##########
train <- train(cCDR ~ Sex + Age + EDUC + SES + eTIV + nWBV, method = "knn", 
                   data = alzheimer,
                   tuneGrid = data.frame(k = seq(1, 500, 10)))
# Plotting the Accuracy with various values of k
ggplot(train, highlight = TRUE)

# Value of k that achieves the highest accuracy levels
train$bestTune

# Classification of the data according th the final model
train$finalModel

# RMSE of this KNN model
RMSE_KNN <- sqrt(mean((as.numeric(as.character(validation$cCDR)) - as.numeric(as.character(predict(train, validation, type = "raw")))))^2)
rm(train) # Remove KNN Model



######### Ridge & Lasso Logistic Regression Model ##########

# Data Wrangling necessary to use cv.glmnet
## Creating data.matrix with all independent variables from the training dataset (alzheimer)
x.train <- alzheimer %>% select(Sex, Age, EDUC, SES, eTIV, nWBV) %>% data.matrix()
## Creating data.matrix with the dependent variable from the training dataset (alzheimer)
y.train <- alzheimer$cCDR %>% data.matrix()
## Creating data.matrix with the independent variables from the validation dataset (validation)
x.validation <- validation %>% select(Sex, Age, EDUC, SES, eTIV, nWBV) %>% data.matrix()
## Creating data.matrix with the dependent variable from the validation dataset (validation)
y.validation <- validation$cCDR %>% data.matrix()



######### Ridge Logistic Regression Model ##########

# Creating the Ridge Logistic Regression Model
alpha0.fit <- cv.glmnet(x.train, y.train, type.measure="deviance",
                       alpha=0, family="binomial")

# Predicting values for the validation dataset
alpha0.predicted <- 
  predict(alpha0.fit, s=alpha0.fit$lambda.1se, newx=x.validation)

# RMSE Ridge Logistic Regression Model with validation dataset
RMSE_Ridge <- sqrt(mean(as.numeric(as.character(validation$cCDR)) - exp(alpha0.predicted)/(1+exp(alpha0.predicted)))^2)



######### Lasso Logistic Regression Model ##########

# Creating the Lasso Logistic Regression Model
alpha1.fit <- cv.glmnet(x.train, y.train, type.measure="deviance",
                        alpha=1, family="binomial")

# Predicting values for the validation dataset
alpha1.predicted <- predict(alpha1.fit, s=alpha1.fit$lambda.1se, newx=x.validation)

# RMSE Lasso Logistic Regression Model with validation dataset
RMSE_Lasso <- sqrt(mean(as.numeric(as.character(validation$cCDR)) - exp(alpha1.predicted)/(1+exp(alpha1.predicted)))^2)

# Remove data used for the Ridge and Lasso Regression
rm(alpha0.fit, alpha0.predicted, alpha1.fit, alpha1.predicted, x.train, y.train, x.validation, y.validation)





#########################################################
# Summary of the Results
#########################################################

# Descriptive Statistics
bar_plots # Barplots of Categorical/Ordinal variables
density_plots # Density Plots of numerical variables

# Overview of the reported RMSEs
data.frame(RMSE_Average, RMSE_Logistic, RMSE_KNN, RMSE_Ridge, RMSE_Lasso)
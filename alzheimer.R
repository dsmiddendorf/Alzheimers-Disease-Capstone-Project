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

# → It can be seen that many variables are highly & significantly correlated with each other. 
# We have to check for multicollinearity to conclude which parameters to include in our final model.

# Checking if the variable Sex and CDR are independent (they aren't)
chisq.test(alzheimer$Sex, alzheimer$CDR)

# Are the variables CDR and Group the same?
alzheimer %>% ggplot(aes(Group, fill = as.factor(CDR))) + geom_bar() + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
alzheimer[which(alzheimer$CDR == .5 & alzheimer$Group == "Nondemented"),]





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





##########################################################
# Building the Models
#########################################################

######### Just the Average ##########
averageCDR <- mean(as.numeric(as.character(alzheimer$cCDR)))
RMSE_Average <- sqrt(mean((as.numeric(as.character(validation$cCDR)) - averageCDR)^2))

######### Simple Logistic Regression Model ##########
# Calculating a first simple logistic regression model
model <- glm(cCDR ~ Sex + Age + EDUC + SES+ nWBV, family = "binomial", data = alzheimer)
plot.dat <- predict(model, validation)
RMSE_Logistic <- sqrt(mean(as.numeric(as.character(validation$cCDR)) - exp(plot.dat)/(1+exp(plot.dat)))^2)

# Calculation McFadden's Pseudo R^2 (Interpreted as overall effect size)
ll.null <- model$null.deviance/-2
ll.proposed <- model$deviance/-2
(ll.null - ll.proposed) / ll.null

# Calculation of p-value for McFadden's Pseudo R^2
1 - pchisq(2*(ll.proposed - ll.null), df = (length(model$coefficients)-1))

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


## KNN Model

# KNN Model
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train <- train(cCDR ~ Sex + Age + EDUC + SES + eTIV + nWBV, method = "knn", 
                   data = alzheimer,
                   tuneGrid = data.frame(k = seq(1, 500, 10)))
ggplot(train, highlight = TRUE)

train$bestTune
train$finalModel
RMSE_KNN <- sqrt(mean((as.numeric(as.character(validation$cCDR)) - as.numeric(as.character(predict(train, validation, type = "raw")))))^2)
RMSE_KNN
## Ridge Logistic Regression Model
x.train <- alzheimer %>% select(Sex, Age, EDUC, SES, eTIV, nWBV) %>% data.matrix()
y.train <- alzheimer$cCDR %>% data.matrix()


x.validation <- validation %>% select(Sex, Age, EDUC, SES, eTIV, nWBV) %>% data.matrix()
y.validation <- validation$cCDR %>% data.matrix()


alpha0.fit <- cv.glmnet(x.train, y.train, type.measure="deviance",
                       alpha=0, family="binomial")

alpha0.predicted <- 
  predict(alpha0.fit, s=alpha0.fit$lambda.1se, newx=x.validation)

# RMSE Ridge Logistic Regression Model with validation dataset
RMSE_Ridge <- sqrt(mean(as.numeric(as.character(validation$cCDR)) - exp(alpha0.predicted)/(1+exp(alpha0.predicted)))^2)

## Lasso Logistic Regression Model
alpha1.fit <- cv.glmnet(x.train, y.train, type.measure="deviance",
                        alpha=1, family="binomial")
alpha1.predicted <- 
  predict(alpha1.fit, s=alpha1.fit$lambda.1se, newx=x.validation)

# RMSE Lasso Logistic Regression Model with validation dataset
RMSE_Lasso <- sqrt(mean(as.numeric(as.character(validation$cCDR)) - exp(alpha1.predicted)/(1+exp(alpha1.predicted)))^2)


data.frame()
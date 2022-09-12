---
title: "PracticalMachineLearning"
author: "Reb Paralleon"
date: "9/6/2022"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Overview

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, the goal is to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.


## Loading the Dataset

```{r, include=FALSE, message=FALSE}
library(caret)
set.seed(123)
```

The initial step to this project is to load the Weight Lifting Exercise datasets. The datasets are already partitioned into train and test split where we use train set for tuning our models while we use the test set to generate our predictions. 

```{r, echo=TRUE, cache = T}
train_data <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
test_data <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
```


## Exploring the Data

As described in our overview, the model is expected to predict 5 different exercise class (**`classe`** variable) from our 6 participants. Let us verify if this is aligned to our data set.

```{r, echo=TRUE, cache = T}
# Participants
table(train_data$user_name)

# Exercise Classifications
table(train_data$classe)
```
Upon checking the dataset, we have 6 participants namely Adelmo, Carlitos, Charles, Eurico, Jeremy, and Pedro. On the other hand, we have also verified that we have 5 different class exercise (Class A to E). 
We have confirmed that we have confirmed that the variable to be predicted is in our dataset. Let us check the other variables we can potentially use when we fit our model as our predictors.

```{r, echo=TRUE, cache = T}
dim(train_data)
```
Our dataset currently has 160 columns. We can explore the option to minimize the number of variables to be used when we clean our data for better model explainability.   

## Cleaning the Data

We can start by removing columns that has too many NULL or blank values

```{r, echo=TRUE, cache = T}
# NULL Value Count
table(colSums(is.na(train_data) | train_data == "")) 
```
Out of 160 columns, 100 of which have 98% null values (19,216/19,622). Removing them will result to 60 columns remaining. Let us check what other variables we can exclude that may not be relevant as predictors to our exercise classifications.

```{r, echo=TRUE, cache = T}
train_data = train_data[ , colSums(is.na(train_data)) == 0]
train_data = train_data[ , colSums(train_data == "" ) == 0]
str(train_data[,c(1:10)]) # Just to preview first 10 columns.
```
From our data structure, we can remove the index, timestamps and the window columns (total of 7) since they do not pertain to any kind of measurement that directly describe the participant's weightlifting. This leaves us to a total of 53 variables (52 if excluding the classification column).

```{r, cache = T}
train_dim <- train_data[,c(1:7)]
train_data <- train_data[,-c(1:7)]
```

With our final variables on hand, we can now partition our data to train and validation split. We use our train set for fitting the model and our validation set to measure the model's performance. The validation set will serve as our out-of-sample evaluation set.

```{r, echo=TRUE, cache = T}
inTrain <- createDataPartition(train_data$classe, p=0.7, list=FALSE)
train_set <- train_data[inTrain,]
validation_set <- train_data[-inTrain,]
```

## Bulding the Model

Since the nature of our project is to predict classifications and we still have 52 variables to work on which is still relatively high, using **`Random Forest`** may be a good option. Random Forest is great with high dimensionality since it works with subsets of data. 

In tuning the model, different hyperparameters were tested to find the optimal out-of-sample accuracy and we arrived at 250 trees. For our control, we use cross validation (5) which balances the training time and bias. 

```{r, echo=TRUE, cache = T}
#Cross-validation
rfControl <- trainControl(method="cv", 5)

#Train the model
rfModel <- train(classe~., data=train_set, method="rf", trControl = rfControl, tuneLength = 5, ntree=250)
rfModel
```

```{r, cache = T}
rfPredictions <- predict(rfModel, validation_set)
confusionMatrix(factor(validation_set$classe), rfPredictions)
```
Upon evaluating our model's performance on our out-of-sample data set, we got an **`overall accuracy of 99%`**.


## Predicting the Test Set

The final step to this project is to generate our predictions to our test set (total of 20 observations) using our fitted model.

```{r, cache = T}
train_cols <- colnames(train_data)
test_cols <- train_cols[train_cols != "classe"] 
test_set <- test_data[, test_cols]

test_pred <- predict(rfModel, test_set)
test_pred

```








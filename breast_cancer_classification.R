library(tidyverse)
library(caret)
library(randomForest)
library(mlbench)

# Load dataset
data("BreastCancer")
cancer_data <- BreastCancer

# Clean data
cancer_data[cancer_data == "?"] <- NA
cancer_data[, 2:10] <- lapply(cancer_data[, 2:10], as.numeric)
cancer_data <- na.omit(cancer_data)

# Split into train and test
set.seed(123)
trainIndex <- createDataPartition(cancer_data$Class, p = 0.8, list = FALSE)
train_data <- cancer_data[trainIndex, ]
test_data <- cancer_data[-trainIndex, ]

# Train model
model <- randomForest(Class ~ ., data = train_data, importance = TRUE)

# Predict & evaluate
predictions <- predict(model, newdata = test_data)
confusionMatrix(predictions, test_data$Class)

# Feature importance
importance(model)
varImpPlot(model)

# ============================================================
# Breast Cancer Classification Using Random Forest
# ============================================================

# 1. Load required packages
library(caret)
library(randomForest)
library(mlbench)

# 2. Load dataset
data("BreastCancer")
cancer_data <- BreastCancer

# 3. Data cleaning
cancer_data[cancer_data == "?"] <- NA

feature_cols <- setdiff(
  names(cancer_data),
  c("Id", "Class")
)

cancer_data[feature_cols] <- lapply(
  cancer_data[feature_cols],
  as.numeric
)

cancer_data <- na.omit(cancer_data)

# 4. Prepare modelling data
model_data <- cancer_data[, c(feature_cols, "Class")]

# 5. Train-test split
set.seed(123)

train_index <- createDataPartition(
  model_data$Class,
  p = 0.80,
  list = FALSE
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# 6. Train Random Forest
set.seed(123)

rf_model <- randomForest(
  Class ~ .,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

# 7. Test-set prediction
predictions <- predict(
  rf_model,
  newdata = test_data
)

# 8. Model evaluation
confusion_results <- confusionMatrix(
  predictions,
  test_data$Class,
  positive = "malignant"
)

print(confusion_results)

# 9. Feature importance
print(importance(rf_model))

varImpPlot(
  rf_model,
  main = "Random Forest Feature Importance"
)

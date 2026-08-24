# Breast Cancer Classification Using Random Forest

## Overview

This project develops a supervised machine learning pipeline for binary classification of breast tumors as **benign** or **malignant** using the `BreastCancer` dataset from the `mlbench` R package.

A **Random Forest classifier** is trained on the preprocessed tumor measurements and evaluated on a held-out test set. Model performance is assessed using accuracy, sensitivity, specificity, precision, negative predictive value, and Cohen's kappa. Random Forest feature importance is also examined to identify variables that contribute most strongly to the model's predictions.

> **Clinical Disclaimer:** This project is intended for educational and research purposes only. The model has not been clinically validated and should not be used for medical diagnosis or clinical decision-making.

## Dataset

The project uses the `BreastCancer` dataset provided by the `mlbench` R package.

After preprocessing:

- **683 observations**
- **9 predictive variables**
- **1 binary target variable:** `Class`
- **Target classes:** `benign` and `malignant`

The `Id` variable is excluded from model development because it represents a sample identifier rather than a predictive feature.

### Data Preparation

The preprocessing workflow includes:

- Converting `?` values to `NA`
- Converting predictor variables to numeric format
- Removing observations containing missing values
- Excluding the `Id` identifier from modelling
- Retaining `Class` as the binary target variable

After removing observations with missing values, **683 observations remained for analysis**.

## Methodology

### Train-Test Split

The cleaned dataset was divided into training and testing subsets using an **80:20 split** with stratification based on the target variable.

- **Training set:** 548 observations
- **Test set:** 135 observations

A fixed random seed (`123`) was used to improve reproducibility.

### Model Development

A **Random Forest classifier** was trained using:

| Parameter | Value |
|---|---:|
| Algorithm | Random Forest |
| Number of trees | 500 |
| Target variable | `Class` |
| Positive class | `malignant` |
| Train-test split | 80:20 |
| Random seed | 123 |

## Model Evaluation

Performance was evaluated exclusively on the **held-out test set**.

| Metric | Score |
|---|---:|
| Accuracy | **96.30%** |
| Sensitivity (Recall) | **95.74%** |
| Specificity | **96.59%** |
| Precision (PPV) | **93.75%** |
| Negative Predictive Value | **97.70%** |
| Cohen's Kappa | **0.9188** |

### Confusion Matrix

| | Actual Benign | Actual Malignant |
|---|---:|---:|
| **Predicted Benign** | 85 | 2 |
| **Predicted Malignant** | 3 | 45 |

The model correctly classified **130 of 135 test observations**.

### Interpretation

The model demonstrated strong classification performance on the held-out test set, with high sensitivity and specificity for distinguishing malignant from benign cases.

However, these results represent performance on this specific dataset and test split and should not be interpreted as evidence of clinical diagnostic performance.

## Feature Importance

Random Forest feature importance was examined to identify variables contributing most strongly to model predictions.

The analysis uses the model's variable-importance measures and `varImpPlot()` for visualization.

Feature importance indicates the variables most useful to the model for prediction; it **does not establish causality or clinical importance**.

## Reproducibility

A fixed random seed is used during dataset splitting and model training:

`set.seed(123)`

This makes the train-test split and model training reproducible under the same software and package environment.

## Limitations

- Model performance is estimated using a single held-out train-test split.
- The dataset is relatively small.
- No independent external validation cohort was used.
- No hyperparameter optimization or nested cross-validation was performed.
- Feature importance should not be interpreted as causal or clinical evidence.
- The model has not been clinically validated.
- Results may vary under different data splits or computational environments.

## Technologies

- R
- RStudio
- `caret`
- `randomForest`
- `mlbench`

## Project Structure

- `breast_cancer_classification.R` - Main analysis and modelling script
- `README.md` - Project documentation

## Reproducibility and Execution

### Requirements

- R
- RStudio
- `caret`
- `randomForest`
- `mlbench`

### Setup

Install the required packages if they are not already installed:

`install.packages(c("caret", "randomForest", "mlbench"))`

### Execution

1. Clone or download this repository.
2. Open `breast_cancer_classification.R` in RStudio.
3. Install the required packages if necessary.
4. Run the script using **Source**.

The script performs data preprocessing, train-test splitting, Random Forest training, test-set evaluation, and feature-importance analysis.

## Conclusion

This project demonstrates an end-to-end supervised machine learning workflow for breast tumor classification using Random Forest in R.

On the held-out test set, the model achieved **96.30% accuracy**, with **95.74% sensitivity** and **96.59% specificity**, and a Cohen's kappa of **0.9188**.

The results demonstrate strong predictive performance on the evaluated dataset while highlighting the importance of appropriate validation and the limitations of applying machine learning models to clinical problems without independent external validation.

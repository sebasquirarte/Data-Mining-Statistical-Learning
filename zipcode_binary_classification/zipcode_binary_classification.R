# ZIP Code Data: Handwritten Digit Classification
#
# Description:
# Binary classification and exploratory analysis of handwritten
# digits using the ZIP Code dataset from ESL.
#
# Dataset:
# 16 x 16 grayscale images (256 pixel predictors).
# Response variable: digit label (0-9).
#
# Objectives:
# - Perform exploratory data analysis and visualization.
# - Implement linear regression and KNN classification.
# - Evaluate training and testing errors.
# - Apply Monte Carlo cross-validation.
# - Compare model performance and select the optimal k.
#
# Methods: EDA, Linear Regression, KNN, Cross-Validation

# November 2026

# Load data ----

zip_test <- read.csv("zip.test.csv", header = FALSE)
zip_train <- read.csv("zip.train.csv", header = FALSE)

# EDA ----

dim(zip_train) # 7,291 rows x 257 columns
str(zip_train)
min(zip_train[, -1])
max(zip_train[, -1]) # grayscale values range from [-1,1]


# Visualize digits
view_digit <- function(n){
  # Extract the nth observation
  digit <- as.numeric(zip_train[n, -1])
  # Reconstruct the 16 x 16 matrix
  digit_matrix <- (matrix(digit, nrow = 16, ncol = 16))
  # Visualize
  image(
    (digit_matrix)[, 16:1],
    col = gray.colors(256),
    axes = FALSE,
    main = paste("Digit:", zip_train[n, 1])
  )
}

view_digit(4) # change position (n)

# Linear Regression ----

model_lm <- lm(V1 ~ ., data = zip_train)


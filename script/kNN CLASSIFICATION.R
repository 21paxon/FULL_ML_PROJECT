
# kNN CLASSIFICATION
# ============================================================

# Install packages once if necessary:
# install.packages("tidyverse")
# install.packages("class")
# install.packages("ISLR2")

library(tidyverse)
library(class)
library(ISLR2)

# Load the dataset
data(" ")

set.seed(12345)

# Inspect the dataset
head(Default)
dim(Default)
str(Default)

# Split data: 70% training, 30% testing
n <- nrow(Default)
train_id <- sample(
  1:n,
  size = floor(0.80 * n)
)

train <- Default[train_id, ]
test <- Default[-train_id, ]

dim(train)
dim(test)

# Select numeric predictors
x_train <- train[, c("balance", "income")]
x_test <- test[, c("balance", "income")]

# Target variable
y_train <- train$default
y_test <- test$default

# Scale predictors using training data statistics
x_train_scaled <- scale(x_train)

x_test_scaled <- scale(
  x_test,
  center = attr(x_train_scaled, "scaled:center"),
  scale = attr(x_train_scaled, "scaled:scale")
)

# kNN classification with k = 5
knn_pred <- knn(
  train = x_train_scaled,
  test = x_test_scaled,
  cl = y_train,
  k = 5
)

# Confusion matrix
table(
  Actual = y_test,
  Predicted = knn_pred
)

# Calculate accuracy
accuracy_k5 <- mean(knn_pred == y_test)
accuracy_k5

# Test several values of k
k_values <- c(1, 3, 4, 5, 6, 7, 11, 15)

accuracy <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  pred <- knn(
    train = x_train_scaled,
    test = x_test_scaled,
    cl = y_train,
    k = k_values[i]
  )

  accuracy[i] <- mean(pred == y_test)
}

# Compare accuracy for different k values
results <- data.frame(
  k = k_values,
  Accuracy = accuracy
)

print(results)

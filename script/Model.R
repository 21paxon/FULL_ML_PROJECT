# ============================================================
# PART A. LOAD AND PREPARE THE DATA
# ============================================================

library(dplyr)
library(caret)
library(e1071)
library(class)
library(pROC)
library(ggplot2)
library(tibble)

# 1. Read the raw dataset directly from GitHub
# PimaIndiansDiabetes <- read.csv(
#   "https://raw.githubusercontent.com/jbrownlee/Datasets/master/pima-indians-diabetes.data.csv",
#   header = FALSE
# )

PimaIndiansDiabetes <- read.csv(
  "data/pima-indians-diabetes.data.csv",
  header = FALSE
)

# # Check that the data loaded successfully
# head(PimaIndiansDiabetes)
# dim(PimaIndiansDiabetes)

# 2. Assign column names
colnames(PimaIndiansDiabetes) <- c(
  "pregnant", "glucose", "pressure", "triceps",
  "insulin", "mass", "pedigree", "age", "diabetes"
)

# 3. Convert the target into a factor
PimaIndiansDiabetes$diabetes <- factor(
  PimaIndiansDiabetes$diabetes,
  levels = c(0, 1),
  labels = c("neg", "pos")
)

# Create the main dataset
Pima <- PimaIndiansDiabetes

# Inspect the dataset
glimpse(Pima)

# Check class balance
print(table(Pima$diabetes))
print(prop.table(table(Pima$diabetes)))

dim(PimaIndiansDiabetes)


# ============================================================
# TRAIN / TEST SPLIT
# ============================================================

set.seed(75)

train_idx <- createDataPartition(
  Pima$diabetes,
  p = 0.75,
  list = FALSE
)

train <- Pima[train_idx, ]
test  <- Pima[-train_idx, ]

cat("Training rows:", nrow(train), "\n")
cat("Testing rows:", nrow(test), "\n")


# ============================================================
# PART B. BAYESIAN DECISION THEORY
# ============================================================

# Calculate class prior probabilities
class_priors <- prop.table(table(train$diabetes))

print(class_priors)

# Interpretation:
# P(neg) = probability of a negative class before using features
# P(pos) = probability of a positive class before using features


# ============================================================
# PART C. NAIVE BAYES CLASSIFIER
# ============================================================

# Train the Naive Bayes model
nb_model <- naiveBayes(
  diabetes ~ .,
  data = train
)

# Display the learned model
print(nb_model)

# Predict classes
nb_pred <- predict(
  nb_model,
  newdata = test
)

# Confusion matrix
nb_cm <- confusionMatrix(
  nb_pred,
  test$diabetes,
  positive = "pos"
)

print(nb_cm)

# Predict posterior probabilities
nb_probs <- predict(
  nb_model,
  newdata = test,
  type = "raw"
)

# View the first six probability predictions
head(nb_probs)

# Probability of the positive class
nb_prob_pos <- nb_probs[, "pos"]


# ============================================================
# PART D. k-NEAREST NEIGHBOURS (k-NN)
# ============================================================

# Select predictor columns.
# Exclude the target variable "diabetes".
num_cols <- setdiff(names(train), "diabetes")

# Save the original, unscaled data for comparison
train_unscaled <- train[, num_cols]
test_unscaled  <- test[, num_cols]

# Calculate scaling values using TRAINING data only
train_center <- colMeans(train_unscaled)
train_scale  <- apply(train_unscaled, 2, sd)

# Protect against zero or invalid standard deviations
if (any(!is.finite(train_scale) | train_scale == 0)) {
  stop("A predictor has an invalid or zero standard deviation.")
}

# Scale training data
train_scaled <- scale(
  train_unscaled,
  center = train_center,
  scale = train_scale
)

# Scale testing data using TRAINING means and standard deviations
test_scaled <- scale(
  test_unscaled,
  center = train_center,
  scale = train_scale
)

# Convert matrices to data frames for easier inspection
train_scaled <- as.data.frame(train_scaled)
test_scaled  <- as.data.frame(test_scaled)

# View scaled training and testing data
head(train_scaled)
head(test_scaled)

# Run k-NN with k = 5
set.seed(42)

knn_pred <- knn(
  train = train_scaled,
  test = test_scaled,
  cl = train$diabetes,
  k = 5,
  prob = TRUE
)

# Confusion matrix
knn_cm <- confusionMatrix(
  knn_pred,
  test$diabetes,
  positive = "pos"
)

print(knn_cm)

# Select several k values and compare accuracy
k_values <- c(1, 3, 5, 7, 9, 11, 15)

accuracies <- sapply(k_values, function(k) {

  set.seed(42)

  pred_k <- knn(
    train = train_scaled,
    test = test_scaled,
    cl = train$diabetes,
    k = k
  )

  mean(pred_k == test$diabetes)
})

# Display the results
knn_results <- tibble(
  k = k_values,
  accuracy = accuracies
)

print(knn_results)

# Plot accuracy for each k
ggplot(knn_results, aes(x = k, y = accuracy)) +
  geom_line(colour = "#2E86AB") +
  geom_point(size = 3, colour = "#F26430") +
  labs(
    title = "Choosing k for k-NN",
    x = "k (number of neighbours)",
    y = "Test Accuracy"
  ) +
  theme_minimal()

# Calculate positive-class probabilities for ROC analysis.
# class::knn reports the winning class's vote proportion.
knn_vote_prob <- attr(knn_pred, "prob")

knn_prob_pos <- ifelse(
  knn_pred == "pos",
  knn_vote_prob,
  1 - knn_vote_prob
)


# ============================================================
# PART E. SUPPORT VECTOR MACHINE (SVM)
# ============================================================

# Use the scaled predictors for SVM.
# Keep the target variable unchanged.
train_svm <- train_scaled
test_svm  <- test_scaled

train_svm$diabetes <- train$diabetes
test_svm$diabetes  <- test$diabetes

# ------------------------------------------------------------
# E.1 LINEAR KERNEL
# ------------------------------------------------------------

svm_linear <- svm(
  diabetes ~ .,
  data = train_svm,
  kernel = "linear",
  cost = 1,
  probability = TRUE
)

svm_linear_pred <- predict(
  svm_linear,
  newdata = test_svm
)

svm_linear_cm <- confusionMatrix(
  svm_linear_pred,
  test$diabetes,
  positive = "pos"
)

print(svm_linear_cm)


# ------------------------------------------------------------
# E.2 RADIAL (RBF / GAUSSIAN) KERNEL
# ------------------------------------------------------------

svm_radial <- svm(
  diabetes ~ .,
  data = train_svm,
  kernel = "radial",
  cost = 1,
  gamma = 0.1,
  probability = TRUE
)

svm_radial_pred <- predict(
  svm_radial,
  newdata = test_svm
)

svm_radial_cm <- confusionMatrix(
  svm_radial_pred,
  test$diabetes,
  positive = "pos"
)

print(svm_radial_cm)

# Predict probabilities for the positive class
svm_radial_prediction <- predict(
  svm_radial,
  newdata = test_svm,
  probability = TRUE
)

svm_probs <- attr(
  svm_radial_prediction,
  "probabilities"
)

svm_prob_pos <- svm_probs[, "pos"]


# ------------------------------------------------------------
# E.3 TUNE COST AND GAMMA USING CROSS-VALIDATION
# ------------------------------------------------------------

set.seed(42)

tuned <- tune(
  svm,
  diabetes ~ .,
  data = train_svm,
  kernel = "radial",
  ranges = list(
    cost = c(0.1, 1, 10),
    gamma = c(0.01, 0.1, 1)
  ),
  tunecontrol = tune.control(
    cross = 5
  )
)

print(summary(tuned))

# Best model selected by cross-validation
best_svm <- tuned$best.model

print(best_svm)

# Evaluate the tuned model on the test data
best_svm_pred <- predict(
  best_svm,
  newdata = test_svm
)

best_svm_cm <- confusionMatrix(
  best_svm_pred,
  test$diabetes,
  positive = "pos"
)

print(best_svm_cm)


# ============================================================
# PART F. COMPARE ALL THREE MODELS USING ROC / AUC
# ============================================================

# ROC curve for Naive Bayes
roc_nb <- roc(
  response = test$diabetes,
  predictor = as.numeric(nb_prob_pos),
  levels = c("neg", "pos"),
  direction = "<",
  quiet = TRUE
)

# ROC curve for k-NN
roc_knn <- roc(
  response = test$diabetes,
  predictor = as.numeric(knn_prob_pos),
  levels = c("neg", "pos"),
  direction = "<",
  quiet = TRUE
)

# ROC curve for radial SVM
roc_svm <- roc(
  response = test$diabetes,
  predictor = as.numeric(svm_prob_pos),
  levels = c("neg", "pos"),
  direction = "<",
  quiet = TRUE
)

# Calculate AUC values
auc_nb  <- as.numeric(auc(roc_nb))
auc_knn <- as.numeric(auc(roc_knn))
auc_svm <- as.numeric(auc(roc_svm))

# Display AUC comparison
auc_results <- tibble(
  Model = c("Naive Bayes", "k-NN", "SVM Radial"),
  AUC = c(auc_nb, auc_knn, auc_svm)
)

print(auc_results)

# Plot all three ROC curves
plot(
  roc_nb,
  col = "#2E86AB",
  main = "ROC Curves: Naive Bayes vs k-NN vs SVM"
)

plot(roc_knn, col = "#2CA02C", add = TRUE)
plot(roc_svm, col = "#F26430", add = TRUE)

legend(
  "bottomright",
  legend = c(
    paste("Naive Bayes AUC =", round(auc_nb, 3)),
    paste("k-NN AUC =", round(auc_knn, 3)),
    paste("SVM Radial AUC =", round(auc_svm, 3))
  ),
  col = c("#2E86AB", "#2CA02C", "#F26430"),
  lwd = 2
)


# ============================================================
# EXERCISES FOR TRAINEES
# ============================================================

# 1. Which model has the highest AUC?
#    Does this match your expectations?

# 2. Run k-NN using the unscaled data.
#    Does performance change? Why?

# 3. Increase the SVM cost parameter to 100.
#    What happens to the number of support vectors?
#    Relate the result to overfitting.

# 4. Compare accuracy, precision, recall and F1-score
#    from the confusion matrices.

# 5. For diabetes screening, why might recall be important?
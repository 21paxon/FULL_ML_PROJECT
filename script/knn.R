# ============================================================
# FULL MACHINE LEARNING PROJECT
# Pima Indians Diabetes Classification
# Environment: R / Positron
# Dataset: data/pima-indians-diabetes.data.csv
# Models: Naive Bayes, kNN, Linear SVM, Radial SVM
# ============================================================

# 1. CHECK PROJECT DIRECTORY AND DATASET -----------------------

cat("Working directory:\n")
print(getwd())

file_path <- file.path("data", "pima-indians-diabetes.data.csv")

if (!file.exists(file_path)) {
  # Try locating the file under the current project directory.
  matches <- list.files(
    path = getwd(),
    pattern = "^pima-indians-diabetes\\.data\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(matches) > 0) {
    file_path <- matches[1]
  } else {
    stop(
      "Dataset not found. Expected: data/pima-indians-diabetes.data.csv\n",
      "Check that the working directory is FULL_ML_PROJECT."
    )
  }
}

cat("\nDataset found at:\n", normalizePath(file_path), "\n")


# 2. INSTALL AND LOAD THE REQUIRED PACKAGE ---------------------

# class is part of the standard R installation.
if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071", repos = "https://cloud.r-project.org")
}

if (!requireNamespace("e1071", quietly = TRUE)) {
  stop(
    "Package 'e1071' is not available. Install it in the Positron Console ",
    "using install.packages('e1071'), then run this script again."
  )
}

library(class)
library(e1071)


# 3. LOAD THE DATASET ------------------------------------------

diabetes_data <- read.csv(
  file_path,
  header = FALSE,
  stringsAsFactors = FALSE
)

expected_columns <- c(
  "pregnant", "glucose", "pressure", "triceps", "insulin",
  "mass", "pedigree", "age", "diabetes"
)

if (ncol(diabetes_data) != 9) {
  stop(
    "Expected 9 columns in the Pima dataset, but found ",
    ncol(diabetes_data),
    ". Check whether the selected CSV is the correct dataset."
  )
}

names(diabetes_data) <- expected_columns

# Convert every feature column to numeric.
feature_names <- expected_columns[expected_columns != "diabetes"]
diabetes_data[feature_names] <- lapply(
  diabetes_data[feature_names],
  function(x) suppressWarnings(as.numeric(x))
)

# Ensure the outcome is 0/1 before converting it to a factor.
outcome_values <- suppressWarnings(as.numeric(diabetes_data$diabetes))

if (anyNA(outcome_values) || !all(outcome_values %in% c(0, 1))) {
  stop("The diabetes column must contain only 0 and 1, with no missing values.")
}

diabetes_data$diabetes <- factor(
  outcome_values,
  levels = c(0, 1),
  labels = c("neg", "pos")
)

cat("\nDataset dimensions:\n")
print(dim(diabetes_data))

cat("\nFirst six rows:\n")
print(head(diabetes_data))

cat("\nDataset structure:\n")
str(diabetes_data)

cat("\nClass distribution:\n")
print(table(diabetes_data$diabetes))


# 4. CHECK AND CLEAN DATA --------------------------------------

cat("\nMissing values by column:\n")
print(colSums(is.na(diabetes_data)))

cat("\nDuplicate rows:\n")
print(sum(duplicated(diabetes_data)))

diabetes_data <- unique(diabetes_data)

# Remove rows containing actual NA values.
diabetes_data <- diabetes_data[complete.cases(diabetes_data), ]

# IMPORTANT:
# In this dataset, some zero values in glucose, pressure, triceps,
# insulin, and mass may represent missing measurements. This script
# keeps those zeros unchanged so the workflow remains transparent.
# Consider handling them separately in a later data-cleaning step.

if (nrow(diabetes_data) < 10) {
  stop("Too few rows remain after cleaning. Check the dataset format.")
}


# 5. EXPLORE THE DATA ------------------------------------------

cat("\nSummary statistics:\n")
print(summary(diabetes_data))

cat("\nClass proportions:\n")
print(round(prop.table(table(diabetes_data$diabetes)), 3))

if (!dir.exists("output")) {
  dir.create("output", recursive = TRUE)
}

png("output/class_distribution.png", width = 900, height = 650)
barplot(
  table(diabetes_data$diabetes),
  main = "Diabetes Class Distribution",
  xlab = "Diabetes class",
  ylab = "Number of records",
  col = c("lightblue", "salmon")
)
dev.off()

png("output/glucose_by_class.png", width = 900, height = 650)
boxplot(
  glucose ~ diabetes,
  data = diabetes_data,
  main = "Glucose Level by Diabetes Class",
  xlab = "Diabetes class",
  ylab = "Glucose level"
)
dev.off()


# 6. PREPARE FEATURES AND TARGET -------------------------------

features <- diabetes_data[, feature_names, drop = FALSE]
target <- droplevels(diabetes_data$diabetes)

# Make sure all feature columns are numeric.
features[] <- lapply(features, as.numeric)

if (anyNA(features) || any(!is.finite(as.matrix(features)))) {
  stop("The feature data contains NA or non-finite values. Clean them before modelling.")
}


# 7. STRATIFIED TRAIN/TEST SPLIT -------------------------------

set.seed(123)

train_indices <- unlist(
  lapply(levels(target), function(class_name) {
    class_indices <- which(target == class_name)
    sample(
      class_indices,
      size = max(1, floor(0.75 * length(class_indices)))
    )
  }),
  use.names = FALSE
)

train_indices <- sort(train_indices)
test_indices <- setdiff(seq_along(target), train_indices)

if (length(test_indices) == 0) {
  stop("The test set is empty. Check the number of records.")
}

X_train <- features[train_indices, , drop = FALSE]
X_test  <- features[test_indices, , drop = FALSE]
y_train <- droplevels(target[train_indices])
y_test  <- factor(target[test_indices], levels = c("neg", "pos"))

cat("\nTraining rows:", nrow(X_train), "\n")
cat("Testing rows:", nrow(X_test), "\n")
cat("\nTraining class distribution:\n")
print(table(y_train))
cat("\nTesting class distribution:\n")
print(table(y_test))


# 8. SCALE FEATURES FOR kNN AND SVM ----------------------------

train_means <- vapply(X_train, mean, numeric(1))
train_sds <- vapply(X_train, sd, numeric(1))
train_sds[!is.finite(train_sds) | train_sds == 0] <- 1

X_train_scaled <- scale(
  X_train,
  center = train_means,
  scale = train_sds
)

X_test_scaled <- scale(
  X_test,
  center = train_means,
  scale = train_sds
)


# 9. MODEL 1: NAIVE BAYES --------------------------------------

naive_bayes_model <- e1071::naiveBayes(
  x = X_train,
  y = y_train
)

nb_predictions <- predict(
  naive_bayes_model,
  newdata = X_test,
  type = "class"
)

nb_probabilities <- predict(
  naive_bayes_model,
  newdata = X_test,
  type = "raw"
)[, "pos"]


# 10. MODEL 2: k-NEAREST NEIGHBORS -----------------------------

k_value <- min(5, nrow(X_train))

knn_predictions <- class::knn(
  train = X_train_scaled,
  test = X_test_scaled,
  cl = y_train,
  k = k_value,
  prob = TRUE
)

# class::knn returns the estimated probability of the winning class.
knn_winner_probability <- attr(knn_predictions, "prob")

knn_probabilities <- ifelse(
  knn_predictions == "pos",
  knn_winner_probability,
  1 - knn_winner_probability
)


# 11. MODEL 3: LINEAR SVM --------------------------------------

svm_linear_model <- e1071::svm(
  x = X_train_scaled,
  y = y_train,
  kernel = "linear",
  cost = 1,
  scale = FALSE,
  probability = TRUE
)

svm_linear_predictions <- predict(
  svm_linear_model,
  newdata = X_test_scaled,
  probability = TRUE
)

svm_linear_prob_matrix <- attr(
  svm_linear_predictions,
  "probabilities"
)

if (is.null(svm_linear_prob_matrix) || !("pos" %in% colnames(svm_linear_prob_matrix))) {
  stop("Could not obtain positive-class probabilities from the linear SVM.")
}

svm_linear_probabilities <- svm_linear_prob_matrix[, "pos"]


# 12. MODEL 4: RADIAL SVM --------------------------------------

svm_radial_model <- e1071::svm(
  x = X_train_scaled,
  y = y_train,
  kernel = "radial",
  cost = 1,
  gamma = 1 / ncol(X_train_scaled),
  scale = FALSE,
  probability = TRUE
)

svm_radial_predictions <- predict(
  svm_radial_model,
  newdata = X_test_scaled,
  probability = TRUE
)

svm_radial_prob_matrix <- attr(
  svm_radial_predictions,
  "probabilities"
)

if (is.null(svm_radial_prob_matrix) || !("pos" %in% colnames(svm_radial_prob_matrix))) {
  stop("Could not obtain positive-class probabilities from the radial SVM.")
}

svm_radial_probabilities <- svm_radial_prob_matrix[, "pos"]


# 13. METRICS AND ROC/AUC USING BASE R -------------------------

calculate_auc <- function(actual, probabilities) {
  actual_positive <- actual == "pos"

  if (length(unique(actual_positive)) < 2) {
    return(NA_real_)
  }

  # Rank-based AUC calculation.
  ranks <- rank(probabilities, ties.method = "average")
  n_pos <- sum(actual_positive)
  n_neg <- sum(!actual_positive)

  (sum(ranks[actual_positive]) - n_pos * (n_pos + 1) / 2) /
    (n_pos * n_neg)
}

make_roc <- function(actual, probabilities) {
  thresholds <- sort(unique(c(Inf, probabilities, -Inf)), decreasing = TRUE)

  tpr <- numeric(length(thresholds))
  fpr <- numeric(length(thresholds))

  actual_positive <- actual == "pos"
  n_pos <- sum(actual_positive)
  n_neg <- sum(!actual_positive)

  for (i in seq_along(thresholds)) {
    predicted_positive <- probabilities >= thresholds[i]
    tpr[i] <- if (n_pos > 0) sum(predicted_positive & actual_positive) / n_pos else NA_real_
    fpr[i] <- if (n_neg > 0) sum(predicted_positive & !actual_positive) / n_neg else NA_real_
  }

  data.frame(FPR = fpr, TPR = tpr)
}

evaluate_model <- function(actual, predicted, probabilities, model_name) {
  actual <- factor(actual, levels = c("neg", "pos"))
  predicted <- factor(predicted, levels = c("neg", "pos"))
  probabilities <- as.numeric(probabilities)

  if (length(probabilities) != length(actual) || any(!is.finite(probabilities))) {
    stop(paste("Invalid probability values for model:", model_name))
  }

  confusion <- table(Actual = actual, Predicted = predicted)

  TN <- unname(confusion["neg", "neg"])
  FP <- unname(confusion["neg", "pos"])
  FN <- unname(confusion["pos", "neg"])
  TP <- unname(confusion["pos", "pos"])

  accuracy <- (TP + TN) / sum(confusion)
  precision <- if ((TP + FP) > 0) TP / (TP + FP) else 0
  recall <- if ((TP + FN) > 0) TP / (TP + FN) else 0
  specificity <- if ((TN + FP) > 0) TN / (TN + FP) else 0
  f1_score <- if ((precision + recall) > 0) {
    2 * precision * recall / (precision + recall)
  } else {
    0
  }

  auc_value <- calculate_auc(actual, probabilities)
  roc_data <- make_roc(actual, probabilities)

  cat("\n========================================\n")
  cat("Model:", model_name, "\n")
  cat("========================================\n")
  cat("\nConfusion matrix:\n")
  print(confusion)
  cat("\nAccuracy:", round(accuracy, 4), "\n")
  cat("Precision:", round(precision, 4), "\n")
  cat("Recall / Sensitivity:", round(recall, 4), "\n")
  cat("Specificity:", round(specificity, 4), "\n")
  cat("F1-score:", round(f1_score, 4), "\n")
  cat("AUC:", round(auc_value, 4), "\n")

  list(
    model = model_name,
    confusion_matrix = confusion,
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    specificity = specificity,
    f1_score = f1_score,
    auc = auc_value,
    roc = roc_data
  )
}


# 14. EVALUATE ALL MODELS --------------------------------------

nb_results <- evaluate_model(
  y_test, nb_predictions, nb_probabilities, "Naive Bayes"
)

knn_results <- evaluate_model(
  y_test, knn_predictions, knn_probabilities, paste0("kNN (k = ", k_value, ")")
)

svm_linear_results <- evaluate_model(
  y_test, svm_linear_predictions, svm_linear_probabilities, "Linear SVM"
)

svm_radial_results <- evaluate_model(
  y_test, svm_radial_predictions, svm_radial_probabilities, "Radial SVM"
)


# 15. COMPARE MODEL PERFORMANCE --------------------------------

results_list <- list(
  nb_results,
  knn_results,
  svm_linear_results,
  svm_radial_results
)

model_comparison <- data.frame(
  Model = vapply(results_list, function(x) x$model, character(1)),
  Accuracy = vapply(results_list, function(x) x$accuracy, numeric(1)),
  Precision = vapply(results_list, function(x) x$precision, numeric(1)),
  Recall = vapply(results_list, function(x) x$recall, numeric(1)),
  Specificity = vapply(results_list, function(x) x$specificity, numeric(1)),
  F1_Score = vapply(results_list, function(x) x$f1_score, numeric(1)),
  AUC = vapply(results_list, function(x) x$auc, numeric(1))
)

model_comparison <- model_comparison[
  order(model_comparison$F1_Score, decreasing = TRUE),
]

cat("\nMODEL PERFORMANCE COMPARISON:\n")
print(model_comparison, row.names = FALSE)


# 16. PLOT ROC CURVES ------------------------------------------

plot(
  nb_results$roc$FPR,
  nb_results$roc$TPR,
  type = "l",
  lwd = 2,
  col = "blue",
  xlim = c(0, 1),
  ylim = c(0, 1),
  xlab = "False Positive Rate",
  ylab = "True Positive Rate",
  main = "ROC Curve Comparison"
)

lines(knn_results$roc$FPR, knn_results$roc$TPR, col = "red", lwd = 2)
lines(svm_linear_results$roc$FPR, svm_linear_results$roc$TPR, col = "darkgreen", lwd = 2)
lines(svm_radial_results$roc$FPR, svm_radial_results$roc$TPR, col = "purple", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")

legend(
  "bottomright",
  legend = c(
    paste0("Naive Bayes AUC = ", round(nb_results$auc, 3)),
    paste0("kNN AUC = ", round(knn_results$auc, 3)),
    paste0("Linear SVM AUC = ", round(svm_linear_results$auc, 3)),
    paste0("Radial SVM AUC = ", round(svm_radial_results$auc, 3))
  ),
  col = c("blue", "red", "darkgreen", "purple"),
  lty = 1,
  lwd = 2,
  cex = 0.8
)


# 17. SAVE RESULTS ---------------------------------------------

write.csv(
  model_comparison,
  file = "output/model_comparison.csv",
  row.names = FALSE
)

prediction_results <- data.frame(
  Actual = y_test,
  Naive_Bayes = as.character(nb_predictions),
  kNN = as.character(knn_predictions),
  Linear_SVM = as.character(svm_linear_predictions),
  Radial_SVM = as.character(svm_radial_predictions)
)

write.csv(
  prediction_results,
  file = "output/test_predictions.csv",
  row.names = FALSE
)

# Save the ROC comparison plot as an image.
png("output/roc_comparison.png", width = 1000, height = 750)
plot(
  nb_results$roc$FPR,
  nb_results$roc$TPR,
  type = "l",
  lwd = 2,
  col = "blue",
  xlim = c(0, 1),
  ylim = c(0, 1),
  xlab = "False Positive Rate",
  ylab = "True Positive Rate",
  main = "ROC Curve Comparison"
)
lines(knn_results$roc$FPR, knn_results$roc$TPR, col = "red", lwd = 2)
lines(svm_linear_results$roc$FPR, svm_linear_results$roc$TPR, col = "darkgreen", lwd = 2)
lines(svm_radial_results$roc$FPR, svm_radial_results$roc$TPR, col = "purple", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
legend(
  "bottomright",
  legend = c(
    paste0("Naive Bayes AUC = ", round(nb_results$auc, 3)),
    paste0("kNN AUC = ", round(knn_results$auc, 3)),
    paste0("Linear SVM AUC = ", round(svm_linear_results$auc, 3)),
    paste0("Radial SVM AUC = ", round(svm_radial_results$auc, 3))
  ),
  col = c("blue", "red", "darkgreen", "purple"),
  lty = 1,
  lwd = 2,
  cex = 0.8
)
dev.off()

cat("\nBest model by F1-score:\n")
print(model_comparison[1, , drop = FALSE])

cat("\nPROJECT COMPLETED. Results are saved in the output folder.\n")

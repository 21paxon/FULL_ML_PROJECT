df<-read.csv("paxon.csv")
head(df)
# Extract the target variable from the dataset
y <- df$Final.result

# Display the target variable
print(y)
# Select the five input features from the dataset
x <- df[, c("Attendance...", 
            "Study.hours.week", 
            "Previous.score", 
            "Gender", 
            "Programme")]

# Display the selected features
print(x)
# Convert Gender into a factor
x$Gender <- as.factor(x$Gender)

# Convert Programme into a factor
x$Programme <- as.factor(x$Programme)

# Check the updated data types
str(x)
# Convert Gender and Programme into numerical dummy columns
x_encoded <- model.matrix(~ Gender + Programme - 1, data = x)

# Display the encoded columns
print(x_encoded)

# Select the three numerical features
x_numeric <- x[, c("Attendance...", 
                   "Study.hours.week", 
                   "Previous.score")]

# Combine numerical and encoded categorical features
X_final <- cbind(x_numeric, x_encoded)

# Display the final feature dataset
print(X_final)

# Check the number of rows and columns in the final dataset
dim(X_final)

# Load the caTools package
library(caTools)

# Set a random seed to make the split reproducible
set.seed(123)

# Divide the target variable into 80% training and 20% testing
split <- sample.split(y, SplitRatio = 0.8)

# Select the training features (80% of the records)
X_train <- X_final[split, ]

# Select the testing features (20% of the records)
X_test <- X_final[!split, ]

# Select the training target values (80%)
y_train <- y[split]

# Select the testing target values (20%)
y_test <- y[!split]

# Count the training records
nrow(X_train)

# Count the testing records
nrow(X_test)

# Count Pass and Fail outcomes in the training data
table(y_train)

# Count Pass and Fail outcomes in the testing data
table(y_test)

# Display the number of training records
nrow(X_train)

# Display the number of testing records
nrow(X_test)

# Display the Pass/Fail distribution in training data
table(y_train)

# Display the Pass/Fail distribution in testing data
table(y_test)

# Convert Pass to 1 and Fail to 0
y_binary <- ifelse(y == "Pass", 1, 0)

# Display the binary target values
print(y_binary)


# Create a training dataset containing input features and target values
train_data <- data.frame(
  X_train,
  Final.result = y_binary[split]
)

# Display the training dataset
print(train_data)


# Train a logistic regression model using the training dataset
model <- glm(
  Final.result ~ .,
  data = train_data,
  family = binomial
)

# Display the trained model results
summary(model)
# Remove the redundant GenderMale column
X_train <- X_train[, colnames(X_train) != "GenderMale"]

# Remove the same column from the testing features
X_test <- X_test[, colnames(X_test) != "GenderMale"]

# Display the remaining training features
print(X_train)

# Train logistic regression using the corrected training features
train_data <- data.frame(
  X_train,
  Final.result = y_binary[split]
)

# Fit the logistic regression model
model <- glm(
  Final.result ~ .,
  data = train_data,
  family = binomial
)

# Display the model summary
summary(model)

# Predict the probability of passing for the testing students
predictions <- predict(model, newdata = X_test, type = "response")

# Display the predicted probabilities
print(predictions)

# Convert predicted probabilities into binary predictions
predicted_classes <- ifelse(predictions >= 0.5, 1, 0)

# Display the predicted classes
print(predicted_classes)

# Display the actual results of the testing students
print(y_test)

# Display the predicted results
print(predicted_classes)
# Convert actual testing results into binary values
actual_classes <- ifelse(y_test == "Pass", 1, 0)

# Create the confusion matrix
confusion_matrix <- table(
  Actual = actual_classes,
  Predicted = predicted_classes
)

# Display the confusion matrix
print(confusion_matrix)

# Convert actual testing results into binary values
actual_classes <- ifelse(y_test == "Pass", 1, 0)

# Display the actual binary values
print(actual_classes)
# Compare actual results with predicted results
confusion_matrix <- table(
  Actual = actual_classes,
  Predicted = predicted_classes
)

# Display the confusion matrix
print(confusion_matrix)

# Calculate accuracy using the confusion matrix
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Display accuracy
print(accuracy)

# Calculate precision
precision <- confusion_matrix["1", "1"] /
  sum(confusion_matrix[, "1"])

# Display precision
print(precision)

# Calculate recall
recall <- confusion_matrix["1", "1"] /
  sum(confusion_matrix["1", ])

# Display recall
print(recall)

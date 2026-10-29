# ============================================================
# STUDENT DATASET CLEANING USING R
# Purpose: Load, inspect, clean, and validate student data
# ============================================================


# STEP 1: CREATE PROJECT FOLDERS

# Store the names of the folders we need in a character vector.
folder <- c("data", "lesson1")

# Loop through each folder name in the vector.
for (f in folder) {

  # Check whether the current folder does not exist.
  if (!dir.exists(f)) {

    # Create the folder if it does not already exist.
    dir.create(f)

  } else {

    # Display a message when the folder already exists.
    print(paste("Folder already exists:", f))
  }
}


# STEP 2: SPECIFY THE DATASET PATH

# Store the relative path to the CSV dataset.
# The file students.csv should be inside the data folder.
data <- "data/students.csv"


# STEP 3: LOAD THE DATASET

# Read the CSV file and store its contents in the dataframe df.
df <- read.csv(
  data,

  # Keep text columns as character values instead of factors.
  stringsAsFactors = FALSE,

  # Treat these values as missing data when importing the file.
  na.strings = c("", "NA", "Missing", "unknown")
)


# STEP 4: CHECK THE NUMBER OF ROWS AND COLUMNS

# Display the dataset dimensions.
# The first number is the number of rows.
# The second number is the number of columns.
dim(df)


# STEP 5: DISPLAY COLUMN NAMES

# Show the names of all columns in the dataset.
names(df)


# STEP 6: CHECK DATA TYPES AND STRUCTURE

# Display each column's name, data type, and sample values.
str(df)


# STEP 7: COUNT MISSING VALUES

# is.na(df) identifies missing values in the dataframe.
# colSums() counts the missing values in each column.
colSums(is.na(df))


# STEP 8: COUNT DUPLICATE ROWS

# duplicated(df) identifies rows that duplicate an earlier row.
# sum() counts how many duplicate rows are found.
sum(duplicated(df))


# ============================================================
# STEP 9: HANDLE MISSING VALUES IN ATTENDANCE
# ============================================================

# Calculate the median attendance.
# na.rm = TRUE tells R to ignore missing values during calculation.
median(df$Attendance, na.rm = TRUE)

# Find missing values in Attendance and replace them with the median.
df$Attendance[is.na(df$Attendance)] <-
  median(df$Attendance, na.rm = TRUE)

# Count the remaining missing values in Attendance.
# The expected result is 0 if all missing values were replaced.
sum(is.na(df$Attendance))


# IMPORTANT:
# The original CSV may have imported "Attendance (%)" as
# "Attendance..." because read.csv() can modify column names.
# Remove the old column only if it is an unwanted duplicate
# and the cleaned Attendance column contains the correct data.
df$`Attendance...` <- NULL

# Display the current column names after removing the old column.
names(df)

# Confirm whether Attendance still contains missing values.
sum(is.na(df$Attendance))


# ============================================================
# STEP 10: HANDLE MISSING VALUES IN STUDY HOURS
# ============================================================

# Calculate the median number of study hours per week.
# Missing values are ignored during the calculation.
median(df$Study.hours.week, na.rm = TRUE)

# Replace missing study-hour values with the median study hours.
df$Study.hours.week[is.na(df$Study.hours.week)] <-
  median(df$Study.hours.week, na.rm = TRUE)

# Count any missing values remaining in Study.hours.week.
sum(is.na(df$Study.hours.week))


# ============================================================
# STEP 11: SUMMARIZE THE DATA
# ============================================================

# Display descriptive statistics for numeric columns,
# such as minimum, maximum, median, mean, and quartiles.
summary(df)


# ============================================================
# STEP 12: VISUALIZE ATTENDANCE USING A BOXPLOT
# ============================================================

# Create a boxplot to visualize the distribution of attendance.
# The boxplot can help identify potentially unusual values.
boxplot(
  df$Attendance,
  main = "Attendance Boxplot",  # Title of the graph.
  ylab = "Attendance (%)"       # Label for the vertical axis.
)


# ============================================================
# STEP 13: CALCULATE ATTENDANCE QUARTILES
# ============================================================

# Calculate the minimum, first quartile, median,
# third quartile, and maximum attendance values.
quantile(df$Attendance, na.rm = TRUE)


# ============================================================
# STEP 14: CALCULATE THE INTERQUARTILE RANGE (IQR)
# ============================================================

# Calculate the IQR, which is the third quartile minus
# the first quartile. The IQR measures the spread of the
# middle 50% of the data.
IQR_value <- IQR(df$Attendance, na.rm = TRUE)

# Display the calculated IQR.
IQR_value


# ============================================================
# STEP 15: CALCULATE STATISTICAL OUTLIER BOUNDARIES
# ============================================================

# Calculate the lower boundary using Q1 - 1.5 * IQR.
# Values below this boundary are potential low outliers.
lower_bound <- 62.50 - (1.5 * IQR_value)

# Calculate the upper boundary using Q3 + 1.5 * IQR.
# Values above this boundary are potential high outliers.
upper_bound <- 87.25 + (1.5 * IQR_value)

# Display the lower boundary.
lower_bound

# Display the upper boundary.
upper_bound


# ============================================================
# STEP 16: IDENTIFY INVALID ATTENDANCE VALUES
# ============================================================

# Display rows where attendance is below 0 or above 100.
# These values are invalid if attendance is recorded as a percentage.
df[df$Attendance < 0 | df$Attendance > 100, ]


# ============================================================
# STEP 17: CALCULATE THE MEDIAN OF VALID ATTENDANCE VALUES
# ============================================================

# Select attendance values between 0 and 100.
# These are the values considered valid for this exercise.
valid_attendance <- df$Attendance[
  df$Attendance >= 0 & df$Attendance <= 100
]

# Calculate the median using only valid attendance values.
median_attendance <- median(valid_attendance, na.rm = TRUE)

# Display the median of the valid attendance values.
median_attendance


# ============================================================
# STEP 18: REPLACE INVALID ATTENDANCE VALUES
# ============================================================

# Replace attendance values below 0 or above 100 with
# the median of the valid attendance values.
df$Attendance[df$Attendance < 0 | df$Attendance > 100] <-
  median_attendance

# Display the complete record for student ST005.
# This helps us verify the attendance correction.
df[df$Student.ID == "ST005", ]


# ============================================================
# STEP 19: VERIFY ATTENDANCE AFTER CLEANING
# ============================================================

# Display summary statistics for the cleaned Attendance column.
summary(df$Attendance)

# Count attendance values outside the valid range of 0 to 100.
# A result of 0 means no such values remain.
sum(df$Attendance < 0 | df$Attendance > 100, na.rm = TRUE)


# ============================================================
# STEP 20: STANDARDIZE GENDER CATEGORIES
# ============================================================

# Replace the category "M" with "Male" for consistency.
df$Gender[df$Gender == "M"] <- "Male"

# Display the unique categories in the Gender column.
unique(df$Gender)

# Count the number of records in each gender category.
table(df$Gender)

# Check which rows, if any, still contain the value "M".
# All values should be FALSE if the replacement succeeded.
df$Gender == "M"

# Count gender categories and include missing values if present.
table(df$Gender, useNA = "ifany")

# Display the complete record for student ST010.
# This allows us to verify the gender standardization.
df[df$Student.ID == "ST010", ]


# ============================================================
# STEP 21: CHECK FOR DUPLICATE ROWS AGAIN
# ============================================================

# Count duplicate rows after cleaning.
# This checks whether any completely duplicated records remain.
sum(duplicated(df))


# ============================================================
# STEP 22: CHECK ALL REMAINING MISSING VALUES
# ============================================================

# Count missing values in every column after cleaning.
# Ideally, each column should show 0 for this dataset.
colSums(is.na(df))


# ============================================================
# STEP 23: CHECK THE FINAL DATAFRAME STRUCTURE
# ============================================================

# Confirm column names, data types, and sample values.
str(df)

# Confirm the final number of rows and columns.
dim(df)

# Open the dataframe in RStudio/Positron's data viewer.
View(df)


# ============================================================
# END OF DATA CLEANING
# ============================================================


# Step 1: Create folders if they do not exist
folder <- c("data", "lesson1")

for (f in folder) {
  if (!dir.exists(f)) {
    dir.create(f)
  } else {
    print("Folder already exists")
  }
}

# Step 2: Specify the dataset path
data <- "data/students.csv"

# Step 3: Load the dataset
df <- read.csv(
  data,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "Missing", "unknown")
)

# Step 4: Check the number of rows and columns
dim(df)

# Step 5: Display column names
names(df)

# Step 6: Check data types
str(df)

# Step 7: Count missing values in each column
colSums(is.na(df))

# Step 8: Count duplicate rows
sum(duplicated(df))

median(df$Attendance,na.rm=TRUE)

df$Attendance[is.na(df$Attendance)] <- median(df$Attendance,na.rm=TRUE)

sum(df$Attendance...)
print(df$Attendance...)



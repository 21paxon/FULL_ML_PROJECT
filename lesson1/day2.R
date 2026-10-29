
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

sum(is.na(df$Attendance))

sum(is.na(df$Attendance))
sum(is.na(df$Attendance))

df$`Attendance...` <- NULL
names(df)
sum(is.na(df$Attendance))


median(df$Study.hours.week,na.rm=TRUE)
df$Study.hours.week[is.na(df$Study.hours.week)] <-median(df$Study.hours.week,na.rm=TRUE)
sum(is.na(df$Study.hours.week))


# Summary of numeric columns
summary(df)

boxplot(df$Attendance,
        main = "Attendance Boxplot",
        ylab = "Attendance (%)")

quantile(df$Attendance, na.rm = TRUE)

IQR_value <- IQR(df$Attendance, na.rm = TRUE)

IQR_value

lower_bound <- 62.50 - (1.5 * IQR_value)
upper_bound <- 87.25 + (1.5 * IQR_value)

lower_bound
upper_bound

df[df$Attendance < 0 | df$Attendance > 100, ]

valid_attendance <- df$Attendance[
  df$Attendance >= 0 & df$Attendance <= 100
]

median_attendance <- median(valid_attendance, na.rm = TRUE)

median_attendance

df$Attendance[df$Attendance < 0 | df$Attendance > 100] <-
  median_attendance
df[df$Student.ID == "ST005", ]


summary(df$Attendance)
sum(df$Attendance < 0 | df$Attendance > 100, na.rm = TRUE)

df$Gender[df$Gender == "M"] <- "Male"

unique(df$Gender)

table(df$Gender)

df$Gender == "M"
table(df$Gender, useNA = "ifany")

df[df$Student.ID == "ST010", ]

sum(duplicated(df))

colSums(is.na(df))

#Check the structure of your dataset
str(df)
#chec dimmension of our dataset
dim(df)
#view cleaned dataset
View(df)

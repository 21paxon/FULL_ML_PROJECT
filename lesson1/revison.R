#load the dataset
df <- read.csv("data/students.csv")
#view imported dataset
View(df)
#to view the stracture of the dataset
str(df)
#to view the dimension of the dataset
dim(df)
#count missing values in each collumn

colSums(is.na(df))
#(is.na(df) checks every cell and identifies missing values.)

# Count all missing values in the entire dataset
sum(is.na(df))
#(sum() adds up all the missing-value indicators.)

# Display statistical summaries of numeric and categorical columns
summary(df)

## Display only rows that contain missing values
df[!complete.cases(df),]

# Calculate the average attendance, ignoring missing values
mean(df$'Attendance...',na.rm=TRUE)
# Calculate the median attendance, ignoring missing values
median(df$'Attendance...',na.rm=TRUE)

# Calculate the average Study.hours.week, ignoring missing values
 mean(df$Study.hours.week,na.rm=TRUE)
#Calculate the median Study.hours.week, ignoring missing values
median(df$Study.hours.week,na.rm=TRUE)

# Replace missing attendance with the median attendance (75)
df$'Attendance...'[is.na(df$'Attendance...')] <- 75

# Display the attendance column to confirm the replacement
df$'Attendance...'

# Replace missing study.hour.week with the median attendance (9)
df$Study.hours.week[is.na(df$Study.hours.week)]<- 9
# Display the study hour week column to confirm the replacement
df$Study.hours.week

colSums(is.na(df))
# Display the attendance column to confirm the replacement
df[df$'Attendance...'>100,]
# Replace attendance values greater than 100 with the median (75)
df$'Attendance...'[df$'Attendance...'>100]<-75

df$'Attendance...'
#verify if there isnt any other remanin
any(df$'Attendance...'>100)
colSums(is.na(df))

df$Gender
# Standardize the gender value M to Male
df$Gender[df$Gender == "M"] <- "Male"

df$Gender

# Count duplicated rows in the dataset
sum(duplicated(df))

str(df)
summary(df)

write.csv(df,"paxon.csv",row.names=FALSE)
file.exists("paxon.csv")
# Count the number of Pass and Fail records
df$Final.result
# Count the number of students in each outcome category
table(df$Final.result)

# Extract the target variable that the model will predict
y<-(df$Final.result)
# Select the input features used to predict the final result
x<-df[,c(
  "Attendance...", 
  "Study.hours.week", 
  "Previous.score", 
  "Gender", 
  "Programme"
)
]
# Display the input features
print(x)
#display the target values
print(y)

# Examine the structure and data types of the input features
str(x)

# Convert Gender from character to factor
x$Gender<-as.factor(x$Gender)
# Convert Programme from character to factor
x$Programme<- as.factor(x$Programme)

str(x)
# Convert categorical variables into one-hot encoded columns
x_encoded <- model.matrix(~ Gender + Programme - 1, data = x)

# Display the encoded categorical variables
print(x_encoded)

# Select the numerical features from the original dataset
x_numeric <- x[, c("Attendance...", 
                   "Study.hours.week", 
                   "Previous.score")]

# Combine numerical features with encoded categorical features
X_final <- cbind(x_numeric, x_encoded)

# Display the final feature dataset
print(X_final)

# Check the structure and data types of the final features
str(X_final)

# Check the number of rows and columns
dim(X_final)
# Install caTools (only needed once on your computer)
install.packages("caTools")
# Load caTools into the current R session
library(caTools)

# Set a random seed so we can reproduce the same split
set.seed(123)

# Generate random values for the 10 student records
split <- sample.split(y, SplitRatio = 0.8)
# Display all objects currently available in the R environment
ls()

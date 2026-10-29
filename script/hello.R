install.packages("readr")
library(readr)
students <- data.frame(
  Attendance = c(85, 60, 95, 70, 80),
  StudyHours = c(12, 5, 18, 8, 10),
  PreviousScore = c(72, 45, 90, 55, 68),
  FinalResult = c("Pass", "Fail", "Pass", "Fail", "Pass")
)

print(students)
head(students)
dim(students)
names(students)
str(students)
summary(students)

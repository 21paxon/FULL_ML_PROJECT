install.packages("readr")
library(readr)

students <- data.frame(
  Attendance = c(85, 60, NA, 70, 80),
  StudyHours = c(12, 5, 18, 8, 10),
  PreviousScore = c(72, 45, 90, 55, 68),
  FinalResult = c("Pass", "Fail", "Pass", "Fail", "Pass")
)

print(students)
is.na(students)
colSums(is.na(students))
#Handling  the missing value
median(students$Attendance,na.rm = TRUE)
#replacing  the missing value
students$Attendance[
  is.na(students$Attendance)
] <- median(students$Attendance, na.rm = TRUE)

print(students)
#to check again if there is no  null valaue
colSums(is.na(students))

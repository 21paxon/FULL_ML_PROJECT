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
median(students$Attendance,na.rm = TRUE)
students$Attendance[
  is.na(students$Attendance)
] <- median(students$Attendance, na.rm = TRUE)

print(students)

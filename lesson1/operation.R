a <-20
b <-30

a+b
a-b
a*b
a%%b
a/b

a <- 20
b <- 30

a == b
a != b
a > b
a < b
a >= b
a <= b

age <- 25

age > 18 & age < 30
age > 30 | age == 25
!(age > 18)

#we create vectors using c()
marks <- c(70, 85, 90, 65, 80)

print(marks)

length(marks)  # Number of elements
sum(marks) #this shows us the total marks
mean(marks) # shhows the average marks
max(marks) #highest marks 
min(marks) #the lowest marks



students <- c("west","mamy","fred","papias")
marks <- c(75,80,65,90)
attendance <- c(85,90,75,95)

length(students) #number of students 
sum(marks)
mean(marks)
max(marks)
min(marks)

mark <- c(75, 80, 65, 90)

mark[1]  # First element
mark[2]  # Second element
mark[3]  # Third element
mark[4]  # Fourth element

mark[c(1,4)]
mark[3]<-95

mark[c(1,3)]

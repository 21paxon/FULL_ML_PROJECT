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

students[2]
marks[3]
students[c(1,4)]
marks[2]<-90
marks[2]


scores <- matrix( c(75,80,65,90,85,70),

nrow=2,

ncol=3
)

print(scores)

#Accessing matrix elements
scores[1,1]
scores[1,]
scores[2,3]
scores[,2]

#The if statement

age <- 16

if (age >= 18) {
  print("you are adult")
}else{
  print("you are young go to the place of the minors")
}

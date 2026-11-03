marks <- 40

if(marks >= 80){
  print("Exclellent")
}else if(marks>=70){
  print("very Good")
}else if(marks>=50){
  print("Pass")
}else{
  print("Fail")
}

#The for loop
for(i in 1:5 ){
  print(i)
}

marks <- c(75, 80, 40, 90, 85)

for (mark in marks) {
 if (mark>=80){
   print("Excellent")
 }else if(mark>=70){
   print("Very Good")
 }else if(mark>=60){
   print("Good")
 }else if(mark>=50){
   print("Pass")
 }else{
   print("Fail")
 }
}

#While loop

i <-1
while(i<=10){
  print(i)
  i <-i+ 1
}
#concentanation
text1 <- "R is "
text2 <-"awesome"

paste(text1,text2)

# multilple variables
var1<-var2<-var3<-"orange"
var1
var2
var3

i <- 0
while (i < 6) {
  print(i)
  i <- i + 1
  if (i == 3) {
    next
  }
  print(i)
}
dice <- c(1, 2, 3, 4, 5, 6)

for (x in dice) {
  print(x)
}

#nested loop
adj <- list("red", "big", "tasty")

fruits <- list("apple", "banana", "cherry")
  for (x in adj) {
    for (y in fruits) {
      print(paste(x, y))
  }
}

my_function <- function() {
  print("Hello World!")
}

my_function() # call the function named my_function

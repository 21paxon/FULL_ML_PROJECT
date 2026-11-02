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

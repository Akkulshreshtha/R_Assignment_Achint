
#1.	Write a function that takes three arguments - the first two should be numbers and the third should be a string.

#a.	If the string "add", output should be addition of two numbers

new5.func=function(p,q,s)
{
  if(s=="add")
  {
    r=p+q;
    return(r)
  }
}
out4<-new5.func(5,8,"add")
print(out4)

#b.	If the string is "subtract", output should be subtraction of two numbers

new6.func=function(p1,q1,s1)
{
  if(s1=="subtract")
  {
    r1=p1-q1;
    return(r1)
  }
}
out5<-new6.func(5,8,"subtract")
print(out5)

#c.	If the string is "divide", output should be division of two numbers


new7.func=function(p2,q2,s2)
{
  if(s2=="divide")
  {
    r1=p2/q2;
    return(r1)
  }
}
out6<-new7.func(12,6,"divide")
print(out6)
#d.	If the string is "multiply", output should be multuplication of two numbers


new8.func=function(p2,q2,s2)
{
  if(s2=="multiply")
  {
    r1=p2*q2;
    return(r1)
  }
}
out7<-new8.func(12,6,"multiply")
print(out7)
#e.	If the string is "log", output should be log of the first number with second number as base


new9.func=function(p2,q2,s2)
{
  if(s2=="log")
  {
    r1=log(p2,base=q2);
    return(r1)
  }
}
out8<-new9.func(12,6,"log")
print(out8)

#f.	If the string is "power", output should be the first number raised to the power of the second number


new10.func=function(p2,q2,s2)
{
  if(s2=="power")
  {
    r1=p2^q2
    return(r1)
  }
}
out9<-new10.func(3,3,"power")
print(out9)


#g.	If the string in anything else, return -1

new11.func=function(p2,q2,s2)
{
  if(s2=="anything else")
  {
    
    return(-1)
  }
}
out10<-new11.func(3,3,"anything else")
print(out10)

#2.	Write a function that takes a number as input and outputs a sequence of odd numbers starting from 1 to that number.


new12.function <- function(n) {
  for(i in 1:n) {
    if(i%%2!=0){
      print(i)
    }
  }
}
new12.function(12)    


#3.	Write a program that iterates through all LETTERS and prints only the vowels.

v <- LETTERS[1:26]

for ( i in v) 
{
  if(i=="a" ||i=="e"||i=="i"|| i=="o"||i=="u"||i=="A" ||i=="E"||i=="I"|| i=="O"||i=="U")
  {
    print(i)
  }
}

#4.	Write a function that returns the class of the supplied input

new13.func<-function(k)
{
  l=class(k)
  return(l)
}
out11=new13.func("Achint")
print(out11)

#5.	Write a program that checks whether a number is prime and return TRUE for prime and FALSE for not prime

flag<-0
new14.func<-function(n)
{
  for(i in 2:(n/2))
  {
    if(n%%i==0)
    {
      flag=1;
      break;
    }
  }
  
  if (flag==0)
  {
    return(TRUE)
  }
  else
  {
    return(FALSE)
  }
}

out12=new14.func(12)
print(out12)

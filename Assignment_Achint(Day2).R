#In the Forest Fires data set -

df<-read.csv("C:/Users/Achint/Desktop/forestfires.csv")
summary(df)
  
#1.	Compute the square of each data point in the X column and store the result in a new column called "X_square"

  transform(df,X_Square=X^2)
  

#2.	Compute the sum, mean, median, standard deviation of the following columns - 
  #a.	FMCC
  
  sum(df[,5])
  mean(df[,5])
  median(df[,5])
  sd(df[,5])
  
#b.	DMC
  
  sum(df[,6])
  mean(df[,6])
  median(df[,6])
  sd(df[,6])
#c.	DC
  
  sum(df[,7])
  mean(df[,7] )
  median(df[,7] )
  sd(df[,7])
#3.	Create another column called "Month", which has full values of month, i.e "aug" becomes "August", "sep" becomes "September" and so on
  
  
   df$Month_Full <-
     sapply(df$month,function(x){
     if(x=="jan"){
       x <- as.factor("January")
     }
       if(x=="feb"){
         x <- as.factor("February")
       }
       if(x=="mar"){
         x <- as.factor("March")
       }
       if(x=="apr"){
         x <- as.factor("April")
       }
       if(x=="may"){
         x <- as.factor("May")
       }
       if(x=="jun"){
         x <- as.factor("June")
       }
       if(x=="jul"){
         x <- as.factor("July")
       }
       if(x=="aug"){
         x <- as.factor("August")
       }
       if(x=="sep"){
         x <- as.factor("September")
       }
       if(x=="oct"){
         x <- as.factor("October")
       }
       if(x=="nov"){
         x <- as.factor("November")
       }
       if(x=="dec"){
         x <- as.factor("December")
       }
       return( x )
     }
     )
#4.	Create another Column Day_Num where day will be from 1 to 7 - 1 being Sunday, 2 being Monday, 3 being Tuesday and so on
   
   df$Column_Day_Num <-
     sapply(df$day,function(x){
       if(x=="sun"){
         x <- as.factor("1")
       }
       if(x=="mon"){
         x <- as.factor("2")
       }
       if(x=="tue"){
         x <- as.factor("3")
       }
       if(x=="wed"){
         x <- as.factor("4")
       }
       if(x=="thu"){
         x <- as.factor("5")
       }
       if(x=="fri"){
         x <- as.factor("6")
       }
       if(x=="sat"){
         x <- as.factor("7")
       }
       return( x )
     }
     )
#5.	Find the correlation coefficient (Theory: http://mathbits.com/MathBits/TISection/Statistics2/correlation.htm) between X and Y [HINT: research and use cor() function]
  
  X=df[,1]
  Y=df[,2]
  cor(X,Y)
  
#6.	Find the total rain,wind  for each month [HINT: dplyr]
  
  install.packages("dplyr")
  library(dplyr)
  
  summarise(group_by(df, month), sum_rain=sum(rain),sum_wind=sum(wind))
  
#7.	Find the mean rain,wind  for each month [HINT: dplyr]
  
  summarise(group_by(df,month),mean_rain=mean(rain),mean_wind=mean(wind))
  
#8.	Find the number of records present for each month
  
  summarise(group_by(df,month),count=n())
  
#9.	Find the number of records for each month-day combo

  summarise(group_by(df,month,day),count=n())
  
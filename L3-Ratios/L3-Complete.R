#Welcome to Lab 3, please answer the following questions

#Install and require package MASS
library(MASS)
library (dplyr)


#Using the built in dataset anorexia, aggregate the data to look at:
#       1. the number of patients who received a treatment (choose one) who responded positively to treatment
#       2. the number of patients who recieved a treatment who either did not respond or responded negatively to treatment
#       3. the number of patients who were in the control group who responded positively to treatment
#       4. the number of patients who were in the control group who did not respond or responded negatively to treatment

#Such that you end up with a 2 by 2 table

data <- mutate(anorexia, weight.change = X3 - X2) #Positive numbers mean that treatment worked!

weight.decrease <- filter(data, weight.change <= 0, X1 == "Cont")
                   
nrow(weight.decrease)
  



#What kind of analysis appropriate to perform on a study like this?




#Perform the appropriate choice on the dataset




#What does your result tell you?





#Write a function that allows you to build the table based on what kind of treatment you want 
#that returns the appropriate calculation





#Which treatment worked better?






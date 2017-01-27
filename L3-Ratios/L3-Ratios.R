#Welcome to Lab 3, please answer the following questions

#Install and require package MASS
install.packages("MASS")
require(MASS)
library(MASS)
library(dplyr)



#Using the built in dataset anorexia, aggregate the data to look at:
data <- mutate(anorexia, weight.change = Postwt - Prewt)
weight.decrease <- data %>% filter(weight.change <= 0, Treat == "Cont")



#       1. the number of patients who received a treatment (choose one) who responded positively to treatment
treatedpositive <- data %>% filter(Treat == "CBT", weight.change > 0)
#       2. the number of patients who recieved a treatment who either did not respond or responded negatively to treatment
treatednegative <- data %>% filter(Treat == "CBT", weight.change <= 0)
#       3. the number of patients who were in the control group who responded positively to treatment
contpositive <- data %>% filter(Treat == "Cont", weight.change > 0)
#       4. the number of patients who were in the control group who did not respond or responded negatively to treatment
contnegative <- data %>% filter(Treat == "Cont", weight.change <= 0)
#Such that you end up with a 2 by 2 table

notanorexic <- c(nrow(treatedpositive), nrow(contpositive), (nrow(treatedpositive) + nrow(contpositive)))
anorexic <- c(nrow(treatednegative), nrow(contnegative), (nrow(treatednegative) + nrow(contnegative)))

resulttbl <- data.frame(notanorexic, anorexic)
resulttbl <- mutate(resulttbl, tot = notanorexic + anorexic)

#What kind of analysis appropriate to perform on a study like this?
# you can do relative risk assessment




#Perform the appropriate choice on the dataset
relative_risk <- (resulttbl$anorexic[1] / resulttbl$tot[1])/(resulttbl$anorexic[2] / resulttbl$tot[2])




#What does your result tell you?
## The risk of developing anorexia was 0.6574 times less for participants that were treated with
## CBT than those who weren't treated.




#Write a function that allows you to perform your analysis based on what kind of treatment you want 
#that returns the appropriate calculation

get_relative_risk_for_therapy <- function(type_of_therapy){
  data <- mutate(anorexia, weight.change = Postwt - Prewt)
  weight.decrease <- data %>% filter(weight.change <= 0, Treat == "Cont")

  #       1. the number of patients who received a treatment (choose one) who responded positively to treatment
  treatedpositive <- data %>% filter(Treat == type_of_therapy, weight.change > 0)
  #       2. the number of patients who recieved a treatment who either did not respond or responded negatively to treatment
  treatednegative <- data %>% filter(Treat == type_of_therapy, weight.change <= 0)
  #       3. the number of patients who were in the control group who responded positively to treatment
  contpositive <- data %>% filter(Treat == "Cont", weight.change > 0)
  #       4. the number of patients who were in the control group who did not respond or responded negatively to treatment
  contnegative <- data %>% filter(Treat == "Cont", weight.change <= 0)
  
  notanorexic <- c(nrow(treatedpositive), nrow(contpositive), (nrow(treatedpositive) + nrow(contpositive)))
  anorexic <- c(nrow(treatednegative), nrow(contnegative), (nrow(treatednegative) + nrow(contnegative)))
  
  resulttbl <- data.frame(notanorexic, anorexic)
  resulttbl <- mutate(resulttbl, tot = notanorexic + anorexic)
  
  relative_risk <- (resulttbl$anorexic[1] / resulttbl$tot[1])/(resulttbl$anorexic[2] / resulttbl$tot[2])

  return(relative_risk)
  }



#Which treatment worked better?
get_relative_risk_for_therapy("CBT")
get_relative_risk_for_therapy("FT")

## FT treatment worked better!




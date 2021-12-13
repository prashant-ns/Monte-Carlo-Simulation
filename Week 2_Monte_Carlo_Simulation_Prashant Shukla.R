library(readr)
library(MonteCarlo)
library(tidyverse)
library(purrr)
library(ggpubr)
library(ggplot2)

set.seed(666) # note self, number is for reproducibility only

Demand <- sample(80:130,260,replace = TRUE) # Generate random number from 80 to 130 for 260 days
Beginning_Inventory <- 100
Production <- 100
Ending_Inventory <- Beginning_Inventory[1] + Production[1] - Demand[1] # Applied the given equation

Days <- 1:260
j <-0 # Started counter to count the number of extra shifts
addshifts <- vector()
monte <- data.frame()

# Started the loop and applied all the conditions

loopmaster <-function(){
  
  Demand <- sample(80:130,260,replace = TRUE) # Generate random number from 80 to 130 for 260 days
  
  i <- 1
  while (i <= 260){
    Beginning_Inventory[i+1] <- Ending_Inventory[i] # Begining inventory for second day will equal to Previous day ending inventory
    if(Beginning_Inventory[i+1] < 50){ # applied condition that if begining inventory is falls below 50 the 
      Production[i+1] <- 200  # increase the production by adding another shift
    } else {
      Production[i+1] <- 100 # else the production remain same
    }
    
    Ending_Inventory[i+1] <- Beginning_Inventory[i+1] + 
      Production[i+1] - Demand[i+1] # applied the equation
    i=i+1
    
  }
  
  PLE <- Production[1:260]
  addshifts <- sum(PLE == 200) # if PLE is equal to 200, it means there is extra shift for that day
  return(addshifts)
  j <- j+1 # Add 1 to the counter for extra shift
}




#Monte Carlo ----
runs <- 100 # number of time simulation runs
monte <- as.data.frame((replicate(runs, loopmaster(), simplify = TRUE))) # simulation runs for 100 times (Cases) and generate number of extra shifts required in a year.
class(monte)
summary(monte)
bin <- (max(monte) - min(monte))/max(monte)
bin
view(monte)
#assume Normal Distributionplot
# Plot shows that 13 (Extra shift) has maximum frequency
plot(table(monte),
     main="Extra Shifts in a year",
     xlab="Count of Additional Shifts",
     ylab = "Frequency")


# QQ Plot for normality


simulation<-c(1:100,1)

monte1<-cbind(simulation,monte$`(replicate(runs, loopmaster(), simplify = TRUE))`)

view(monte1)
monte1<-as.data.frame(monte1)
colnames(monte1$V2)<-"No. Of Extra Shifts"
ggqqplot(monte$`(replicate(runs, loopmaster(), simplify = TRUE))`)

# Density Plot
ggplot(monte1, aes(x=monte1$V2))+
  geom_density(color="black", fill="skyblue")





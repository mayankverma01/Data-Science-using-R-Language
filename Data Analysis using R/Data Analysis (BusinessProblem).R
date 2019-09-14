setwd("C:/Users/HP/Desktop/AnalytixLabs/R_Language/BA_Classes_1-2_Files1_2/BA Classes 1-2 Files/Basic Stats and Hypothesis Testing Case Studies/2. Case Study - Customer Experience")
library(Hmisc)
# ---Business Problem
### This is a peer comparison project. Suppose that you areworking for Samsung in customer experience management team.
### The idea is to regularly monitor the customer satisfaction levels and peer company moves. The competitor company is Apple.
### The objective is to test two main hypothesis.


## 1. The Samsung Average customer satisfaction score is minimum 75%.

### let H0 < 75% (customer satisfaction score is less than  75% )
### and H1 >= 75%  (customer satisfaction score is grater than or equal to  75% )

### Here we are using one sample t.test

customer <- read.csv("Testing_Of_Hypothesis.csv")

Hmisc::describe(customer)
head(customer)
t.test(customer$Samsung.Score,mu = 75)

### from the output result p value is  less then 0.5 is means ( confidence intervel is very high for reject the null hypothesis)
### *** Conclsion***
###  => Average customer satisfaction score is greater then 75% (84.9%).


## 2. The overall average satisfaction score of Samsung is same
## as Apple. There is no significant difference in the satisfaction
## scores

### Let H0 => average satis. score of samsung is = avg. satis. score of Apple
### and H1 => average satis. score of samsung is # avg. satis. score of Apple
### Here we are using independent sample t.test

t.test(customer$Samsung.Score,customer$Apple_Score)

### From the output of the result : p value is greater than 0.50 
### it means H0 is accepted (low confidence level to reject the Null Hypothesis )
### *** Conclusion***
### There is no significant difference between samsung and apple

save.image(file = "~/BusinessProblem.RData")

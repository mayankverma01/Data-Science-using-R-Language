setwd("C:/Users/HP/Desktop/AnalytixLabs/R_Language/BA_Classes_1-2_Files1_2/BA Classes 1-2 Files/Basic Stats and Hypothesis Testing Case Studies/1. Case Study - Hypothesis Testing")
library(Hmisc)

# 1 : Dietstudy
Dietstudy <- read.csv("dietstudy.csv")
head(Dietstudy)
Hmisc::describe(Dietstudy)


##  Test for Triglyceride
## Ho= there are no any difference in the Triglyceride levels
## H1= there are  difference in the Triglyceride levels
## here we will go for paired sample t test for 
## (a=tgo ,b=tg4) 

t.test(Dietstudy$tg0,Dietstudy$tg1)## (a=tgo ,b=tg1) 
t.test(Dietstudy$tg0,Dietstudy$tg2)## (a=tgo ,b=tg2) 
t.test(Dietstudy$tg0,Dietstudy$tg3)## (a=tgo ,b=tg3)
t.test(Dietstudy$tg0,Dietstudy$tg4)## (a=tgo ,b=tg4) 

## From the above random sample test for Triglyceride levels
## p value is greter than the 0.05 and their are minor difference in t statistics.(we have low confidence level(significance level) for reject the Ho )
## => Ho is Accepted.
## It means Set of measurement is not changing in the Triglyceride levels sample.





# Creditpromo

Creditpromo <- read.csv("creditpromo.csv")
str(Creditpromo)

## Ho = NO increase in the Sales
## H1 = Increase in the Sales

## Here we are checking for two independent sample therefore,
## we are using InDependent Sample t Test.
t.test(dollars~insert, data = Creditpromo,var.equal = TRUE)
t.test(dollars~insert, data = Creditpromo,var.equal = FALSE)

## From the above result 
## p value is less than 0.05 (it means we have low evidence for reject Ho)
## => Ho is rejected So
## it means that,
## Yes,The promotion effective to increase the sales.

# Pollination
Pollination <- read.csv("pollination.csv")
str(Pollination)
head(Pollination)

## a: Is the overall population of seed yield/plant (g) equals to 200?
  
## Here we are using One Samle T Test
## Ho = 200
## H1 # 200

t.test(Pollination$Seed_Yield_Plant,mu = 200)


## From above result p value is less than 0.05 
## => Ho is rejected (because we have more evidence for rejecting HO)
## it means The overall popukation of Seed yield/plant(g) unequal to 200

## b: Test whether the natural pollination and hand pollination under open firld conditions 
## are equally effective or are significatly different.

## H0 = natural pollination and hand pollination under open field condition are equally effective.
## H1=  natural pollination and hand pollination under open field condition are significantly different.
## here i will use Independent Sample t test

t.test(Fruit_Wt~Group,data = Pollination,var.equal=TRUE)
t.test(Fruit_Wt~Group,data = Pollination,var.equal=FALSE)


## From the above result,
## P value is very very lower than 0.05 
## => Ho is rejected (because we have more evidence for reject the Ho (Confidence level>95%))
## it means There are significant difference between natural pollination and hand pollination under open field.


# DVD Player

dvd_player <- read.csv("dvdplayer.csv")
str(dvd_player)
head(dvd_player)

## Ho= Rating of various ages consumar are not different
## H1= Rating of various ages consumer are different

ANOVA_segment <- aov(dvdscore ~ agegroup, data = dvd_player)
summary(ANOVA_segment)


## From the above result P-valve is less than 0.05
## => Ho is rejected (cBecause we have more evidence for reject the null hypothesis)
## it means consumers of various ages rated te design differently.

# Sample Survey
survey <- read.csv("sample_survey.csv")
str(survey)
head(survey)

## a: Is there any relationship in between Labour force status with marita status?
## Ho = There are relationship between Labour force status with marital status
## H1 = There are no relationship between Labour force status with marital status

## Here i am doing comparison of two or more populations therefore, i am using
## Chi Square Test of Independence

tab <- xtabs(~wrkstat+marital, data = survey)
chisq.test(tab)

chisq.test(table(survey$wrkstat, survey$marital))

## Form the above result 
## p vlaue < 0.05 and value of statistic is very high 
## (this shows that there are lot diff. between two population)
## => Ho is Rejected (Because i have more evidence (or Confidence level > 95%) for Reject the Ho)

## it Means There are no relationship between Labour force status with marital status


##  b: Do you thinks Aducational Qualification is somehow controlling the marital status?
## Ho= Educational Qualification is  controllng the marital status
## H1= Educational Qualification is not controlling the marital status

## Here i am comparing of two Categorical Variables(populations) therefore, i am using
## Chi Square Test of Independence
chisq.test(table(survey$marital, survey$degree))

## Form the above result 
## p vlaue < 0.05 and value of statistic is very high 
## (this shows that there  lot diff. between two population)
## => Ho is Rejected (Because i have more evidence (or Confidence level > 95%) for Reject the Ho)

## it Means Educational Qualification is not controlling the marital status.

## c: Is happiness is drien by earning or marital status?

## Case 1: Is happiness is driven by income?
## HO= Happiness is driven by earnings
## H1= Happiness is not driven by earnings

## we are using Chi-Squared test (Assessing the relationship between two categorical variable)

chisq.test(table(survey$happy, survey$income))


## From the above result :
##  p value is less then 0.05 ( it we have more evidence for reject the Ho)
## =>Ho is rejected
## it means Happiness is not driven by earning

## Case 2: Is happiness is driven by marital?

chisq.test(table(survey$happy, survey$marital))

## From the above result :
## p value is less then 0.05 ( it we have more evidence for reject the Ho)
## =>Ho is rejected
## it means Happiness is not driven by marital status.

save.image(file = "~/Statistical Methods Assignment.RData")
library(rmarkdown)

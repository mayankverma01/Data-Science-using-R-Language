setwd("C:/Users/HP/Desktop/AnalytixLabs/R_Language/R Case Studies_Sharable/R Credit Card Case Study/Updated R Banking case study - with changes/R case study 2 (Credit card)")

# Import file
Cus.equ <- read.csv("Customer Acqusition.csv")
re.pment <- read.csv("Repayment.csv")
spend <- read.csv("spend.csv")


# import library
library(dplyr)
library(lubridate)

library(ggplot2)
library(plotly)

## Question 1
### a Incase age is less than 18, replace it with mean of age value?

Cus.equ$Age <- ifelse(Cus.equ$Age<18,mean(Cus.equ$Age,na.rm = TRUE),Cus.equ$Age)

### b incase spendamount is more then the limit ,replace it with 50% of limit?
cusequ.spend <- spend%>%left_join(Cus.equ,by = "Customer")
cusequ.spend$Amount <- ifelse(cusequ.spend$Amount>cusequ.spend$Limit,
                              cusequ.spend$Limit/2,
                              cusequ.spend$Amount)

spend <- cusequ.spend[c(colnames(spend))]

### incase the repayment amount is more than the limit, replace the repayment with the limit
cusequ.rep <- re.pment%>%left_join(Cus.equ,by = "Customer")
cusequ.rep$Amount <- ifelse(cusequ.rep$Amount>cusequ.rep$Limit,
                            cusequ.rep$Limit,
                            cusequ.rep$Amount)

re.pment <- cusequ.rep[c(colnames(re.pment))]

## 2 Question
## a How many distinct customers exist?
dis.customer <- unique(Cus.equ$Customer)
length(dis.customer)

## b How many distinct categories exist?
dis.cat <- unique(Cus.equ$Product)
length(dis.cat)

## c average monthly spend by customer?

spend$Month <- as.Date(as.character(spend$Month),format="%d-%b-%Y")
spend$mon <- format(as.Date(spend$Month), "%b-%Y")
Res2 <- spend %>% group_by(Customer,mon)%>%dplyr::summarize(avg.spend=mean(Amount,na.rm = TRUE),total.spend=sum(Amount,na.rm = TRUE))
Res2

# d What is the monthly repayment by customers?

re.pment$Month <- as.Date(as.character(re.pment$Month),format="%d-%b-%Y")
re.pment$mon <- format(as.Date(re.pment$Month), "%b-%Y")
Res21 <- re.pment %>% group_by(Customer,mon)%>%dplyr::summarize(avg.rep=mean(Amount,na.rm = True),total.rep=sum(Amount,na.rm = TRUE))
Res21

## e if the monthly rate of interest is 2.9% what is the profit for the bank for each month?

Res2c <- Res21 %>% left_join(Res2,by=c("Customer","mon"))
Res2c$profit <- Res2c$total.rep-Res2c$total.spend
Res2c$interest <- ifelse((Res2c$profit)<0,"",
                        (Res2c$profit*2.9)/100)
  

## f what are the top 5 product type?

top_Prod <- dplyr::arrange(spend %>% group_by(Type) %>% dplyr::summarize(totalamount=sum(Amount))
                           ,desc(totalamount))
top_Prod[1:5,1]

## g which city is having maximum spend?

city.spend <- dplyr::arrange(cusequ.spend%>%group_by(City)%>% dplyr::summarise(totalamount=sum(Amount))
                             ,desc(totalamount))
city.spend[1,]

## h Which age group is spend more money?
age.spend <- dplyr::arrange(cusequ.spend%>%group_by(Age)%>% dplyr::summarise(totalamount=sum(Amount))
                             ,desc(totalamount))
age.spend[1,]


## i Who are the top 10 customer in terms of repayment?
top10cus <- dplyr::arrange(cusequ.rep%>%group_by(Customer)%>% dplyr::summarise(totalamount=sum(Amount))
                            ,desc(totalamount))
top10cus[1:10,1]




## 3 Calculate the city wise spend on each product on yearly basis.
## Also include a graphical representation for the same?
cusequ.spend$Year <- format(as.Date(as.character(cusequ.spend$Month),format = "%d-%b-%Y"),"%Y")
Res3 <- cusequ.spend%>%group_by(City,Year)%>% dplyr::summarise(totalamount=sum(Amount))

plot1 <- ggplot(Res3)+aes(City,totalamount)+geom_bar(stat = "identity",position = "dodge")+
  facet_grid(.~Year)
plotly::ggplotly(plot1)

##4
## a Monthly comparison of total spends,city wise
cusequ.spend$Month <- as.Date(as.character(cusequ.spend$Month),"%d-%b-%y")
cusequ.spend$Mon <- format(as.Date(as.character(cusequ.spend$Month)), "%b-%Y")
Res4a <- cusequ.spend%>%group_by(Customer,City,Mon)%>% dplyr::summarise(total.spend=sum(Amount,na.rm = TRUE))
Res4a

## b  Comparison of yearly spend on air tickets?
spend$year <- format(as.Date(spend$Month), "%Y")
Res4b <- spend %>% group_by(year,Type)%>% dplyr::summarise(year.spend=sum(Amount))
Res4b[Res4b$Type=="AIR TICKET",]


## c Comparison of monthly spend for each product 

Res4c <- spend%>% group_by(Type,mon) %>% dplyr::summarise(totalamount=sum(Amount)) 
Res4c
plot(Res4c$totalamount,type = "l", lty = 1)
# There is no any seasonality in Data.
## 5
#**********************************************************************#
top10.customers <- function(df=NULL,time_period=NULL,product=NULL){
  
if (time_period=="Month") {
  df$Month <- format(as.Date(as.character(spend$Month)),"%b-%y")
  top <- df%>% group_by(Customer,City,Product,Month)%>% dplyr::summarise(totalrepay=sum(Amount))
  top <- top[top$Product==product,]
  top <- dplyr::arrange(top,desc(totalrepay))
  top <- top[1:10,]
    
}else if (time_period=="year") {
  
  df$year <- format(as.Date(as.character(spend$Month)), "%Y")
  top <- df%>% group_by(Customer,City,Product,year)%>% dplyr::summarise(totalrepay=sum(Amount))
  top <- top[top$Product==product,]
  top <- dplyr::arrange(top,desc(totalrepay))
  top <- top[1:10,]
}
  return(top)
  
}
#********************************************************************#
#Please pass only "Month" or "year" of time period and any product type

top10.customers(df =cusequ.rep,time_period = "year",product = "Gold")

save.image(file = "~/Credit Card Case Study.RData")

#***********************************The End**************************#


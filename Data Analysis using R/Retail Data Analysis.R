setwd("C:/Users/HP/Desktop/AnalytixLabs/R_Language/R Case Studies_Sharable/R Retail Case Study/R Retail Case Study/mv89764@gmail.com-R case study 1 (Retail)")


# Merge the data set Customers,Product Hierarchy,Transation data
customer <- read.csv("Customer.csv")
prod.cat.info <- read.csv("prod_cat_info.csv")
transaction <- read.csv("Transactions.csv")

#a By Base Merge
trans <- merge(x=transaction,y=prod.cat.info,
               by.x=c("prod_cat_code","prod_subcat_code"),
               by.y=c("prod_cat_code","prod_sub_cat_code"),
               all.x = TRUE)

Customer.Final  <- merge(x = trans, y = customer,
                         by.x = "cust_id", 
                         by.y = "customer_Id",
                         all.x = TRUE)

#b By dplyr merge

library(dplyr)

colnames(prod.cat.info) <- c("prod_cat_code","prod_cat","prod_subcat_code","prod_subcat")

trans <- transaction%>%left_join(prod.cat.info,by=c("prod_cat_code","prod_subcat_code"))

colnames(customer) <- c("cust_id","DOB","Gender","city_code")

Customer.Final <-  trans %>% left_join(customer,by = c("cust_id"))


#2 Prepare summary report for the merged data set

#Column name and Data type
ncol(Customer.Final)
names(Customer.Final)
str(Customer.Final)

# Top/Bottom 10 observation
head(Customer.Final, 10)
tail(Customer.Final,10)

#c Summary of Continuous variable

Cus.num <- Customer.Final[c(6,7,8,9)]
summary(Cus.num)
#d Frequencty of Catagoriacle variable

Cus.cat <- Customer.Final[c(4,5,10,14)]
sapply(Cus.cat, table)

# 3 generate a histogram for all continuous variable

sapply(Cus.num,hist)

# 4 Calculate the following 
# a Time period of the available transation Date
library(lubridate)
class(Customer.Final$tran_date)
Customer.Final$tran_date <- parse_date_time(Customer.Final$tran_date,c("%d-%m-%y","%d/%m/%y"))
max(Customer.Final$tran_date,na.rm = T)
min(Customer.Final$tran_date,na.rm = T)


#b Count of transaction where total number of transation is negative
length(Customer.Final$transaction_id[Customer.Final$total_amt<0])

#5 Analyze which product categories are more popular among females vs male customer

# for Male

Res5<- Customer.Final %>% group_by(Gender,prod_cat)%>%dplyr::summarise(count=n())
ResM <- Res5[Res5$Gender=="M",]
ResM[which.max(ResM$count),]

#For Female

ResF <- Res5[Res5$Gender=="F",]
ResF[which.max(ResF$count),]

#6 Which City code has the maximum customer and what was the percentage of customers from that city

Res6 <- Customer.Final %>% group_by(city_code)%>%dplyr::summarise(count=n())
Res6[which.max(Res6$count),]

#7 Which store type sells the maximum products by value and by quantity
Res7 <- Customer.Final %>% group_by(Store_type)%>%dplyr::summarise(count=n(),Quantity=sum(Qty))
# by Value
Res7[which.max(Res7$count),]
# by Quantity
Res7[which.max(Res7$Quantity),]

#8 What was the total amount earned from "Electronic" and "Clothing" categories from Flagship Stores.


Res8 <- Customer.Final%>% group_by(Store_type="Flagship store",prod_cat)%>%dplyr::summarise(amount=sum(total_amt))
Res8[Res8$prod_cat %in% c("Electronics","Clothing"),]


#9 What was the total amount earned from "Male" customer under the "Electronics" category.

Res9 <- Customer.Final%>% group_by(prod_cat=="Electronics",Gender)%>%dplyr::summarise(amount=sum(total_amt))
Res9[Res9$Gender=="M",]


#10 How many customes have more than 10 unique transactions after
# removing all transactions which have any negative amounts
data1 <- Customer.Final[Customer.Final$total_amt>0,]
Res10 <- data1%>% group_by(cust_id)%>%dplyr::summarise(count=n())
Res10[Res10$count>10,]


#11 For all customer aged between 25-35 ,find out

#a What was the total amount spent for "Electronics" and "Books" product categories

class(Customer.Final$DOB)
Customer.Final$DOB <- as.character(Customer.Final$DOB)
Customer.Final$DOB <- as.Date(Customer.Final$DOB,format="%d-%m-%Y",origin='1872-01-01')
Today <- Sys.Date()
Customer.Final$age <- difftime(Today, Customer.Final$DOB, units = "days")
Customer.Final$age <- Customer.Final$age/365
data2 <- Customer.Final[(Customer.Final$age>=25)&(Customer.Final$age<=35),]



Res11 <- data1%>% group_by(prod_cat)%>%dplyr::summarise(sum=sum(total_amt))

Res11 <- Res11[Res11$prod_cat %in% c("Clothing","Electronics"),]
Res11

#What was the total amount spent by these customers between 1st jan,2014 to 1st Mar,2014?
class(Customer.Final$tran_date)
data3 <- Customer.Final[Customer.Final$tran_date > "2014-01-01" & Customer.Final$tran_date < "2014-03-01",]
sum(data3$total_amt)

###################################################################################
ls()
objects()
save.image(file = "~/R Retail Case StudyR.RData")

install.packages("rmarkdown")

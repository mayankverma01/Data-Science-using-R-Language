setwd("C:/Users/HP/Desktop/AnalytixLabs/R_Language/R Case Studies_Sharable/CaseStudy_R_Visualizations/CaseStudy_R_Visualizations")
sales <- read.csv("SalesData.csv")
head(sales)
str(sales)

## Required library
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape2)
library(plotrix)


## 1. Compare Sales by region for 2016 with 2015 using bar chart
Res1 <- sales %>% group_by(Region) %>% dplyr::summarise(TotalSales2015=sum(Sales2015),TotalSales2016=sum(Sales2016))
Res1 <- reshape2::melt(Res1,id.vars= "Region",variable.name="Year",value.name="sales")
plot1 <- ggplot(Res1)+aes(x=Region,y=sales,fill=Year)+geom_bar(stat = "identity",position = "dodge")
plotly::ggplotly(plot1)
## 2. What are the contributing factors to the sales for each region in 2016. Visualize it using a Pie Chart.
### For 3D Pie Chart
### install.packages("plotrix")

Res2 <- sales %>% group_by(Region) %>% dplyr::summarise(Total_Sales2016=sum(Sales2016))
pct <- round(Res2$Total_Sales2016/sum(Res2$Total_Sales2016)*100)
labels <- paste(pct,"%",":",c("Central","East","West"))

par(mfrow = c(1,2))
pie(Res2$Total_Sales2016,labels=labels,main="Pie chart of Sales in 2016")
pie3D(Res2$Total_Sales2016,labels=labels,explode=0.1,main="Pie chart of Sales in 2016")


## 3. Compare the total sales of 2015 and 2016 with respect to Region and Tiers

Res3 <- sales %>% group_by(Region,Tier) %>% dplyr::summarise(TotalSales2015=sum(Sales2015),TotalSales2016=sum(Sales2016))
Res3 <- reshape2::melt(Res3,id.vars= c("Region","Tier"),variable.name="Year",value.name="sales")
plot3 <- ggplot(Res3)+
  aes(x=Tier,y=sales,fill=Year)+
  geom_bar(stat = "identity",position = "dodge")+
  facet_grid(.~Region)
plotly::ggplotly(plot3)
## 4. In East region, which state registered a decline in 2016 as compared to 2015?
Res4 <- sales %>% group_by(Region,State) %>% dplyr::summarise(TotalSales2015=sum(Sales2015),TotalSales2016=sum(Sales2016))
Res4 <- Res4[Res4$Region=="East",]
Res4 <- reshape2::melt(Res4,id.vars= c("Region","State"),variable.name="Year",value.name="sales")
plot4 <- ggplot(Res4)+
  aes(x=State,y=sales,fill=Year)+
  geom_bar(stat = "identity",position = "dodge")

plotly::ggplotly(plot4)
## 5. In all the High tier, which Division saw a decline in number of units sold in 2016 compared to 2015?
Res5 <- sales %>% group_by(Tier,Division) %>% dplyr::summarise(TotalSales2015=sum(Sales2015),TotalSales2016=sum(Sales2016))
Res5 <- Res5[Res5$Tier=="High",]
Res5 <- reshape2::melt(Res5,id.vars= c("Tier","Division"),variable.name="Year",value.name="sales")
plot5 <- ggplot(Res5)+
  aes(x=Division,y=sales,fill=Year)+
  geom_bar(stat = "identity",position = "dodge")
plotly::ggplotly(plot5)
## 6. Create a new column Qtr using ifelse() or any suitable utility in the imported dataset. 
## The Quarters are based on months and defined as

sales$Qtr <- ifelse(sales$Month%in% c("Jan","Feb","Mar"),"Q1",
                      ifelse(sales$Month%in% c("Apr","May","Jun"),"Q2",
                             ifelse(sales$Month%in% c("Jul","Aug","Sep"),"Q3","Q4")))

## 7. Compare Qtr wise sales in 2015 and 2016 in a bar plot

Res7 <- sales %>% group_by(Qtr) %>% dplyr::summarise(TotalSales2015=sum(Sales2015),TotalSales2016=sum(Sales2016))
Res7 <- reshape2::melt(Res7,id.vars= "Qtr",variable.name="Year",value.name="sales")
plot7 <- ggplot(Res7)+
  aes(x=Qtr,y=sales,fill=Year)+
  geom_bar(stat = "identity",position = "dodge")
plotly::ggplotly(plot7)
## 8. Determine the composition of Qtr wise sales in and 2016 with regards to all the Tiers in a pie chart.
#(Draw 4 pie charts representing a Quarter for each Tier)

Res8 <- sales %>% group_by(Qtr,Tier) %>% dplyr::summarise(TotalSales2016=sum(Sales2016))
Res8<- reshape2::melt(Res8,id.vars= c("Qtr","Tier"),variable.name="Year",value.name="sales")

ggplot(Res8, aes(x = factor(1), y = sales, fill = factor(Tier))) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") +
  facet_wrap(~Qtr)

## Save in EData formate

save.image(file = "~/R Data Visualization Case Study.RData")




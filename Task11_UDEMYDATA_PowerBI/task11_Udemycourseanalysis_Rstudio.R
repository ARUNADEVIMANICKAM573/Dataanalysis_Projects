library(tidyverse)
library(readr)
library(dplyr)
view(UDMYCdataCSV)
data1 <- UDMYCdataCSV
view(data1)
data <-data1
view(data)

#1. Total Titles 
v1 <- data %>%
  group_by(created) %>%
  summarise(title=n())
view(v1)

# Extract Year, month, day from Date
library(dplyr)
library(ggplot2)
str(data)
datetime <- as.POSIXct(data$created, format = "%Y-%m-%dT%H:%M:%SZ")
year<- format(datetime, "%Y")
month <- format(datetime, "%m")
monthname <- format(datetime, "%b")
day <- format(datetime, "%d")
hour <- format(datetime, "%H")
              
#2. Total Price_detail_amount per Year
datetime <- as.POSIXct(data$created, format = "%Y-%m-%dT%H:%M:%SZ")
year<- format(datetime, "%Y")
Total_year_Sale <- aggregate(price_detail_amount ~ year, data, sum)
print(Total_year_Sale)
ggplot(Total_year_Sale, aes(x= reorder(year, -price_detail_amount), y = price_detail_amount)) +
  geom_bar(stat = "identity",fill="steelblue") +
  labs(x= " Yar", y= "Price_Detail_Amount", title = "Total Price_detail_amount per Year ")+
  theme(axis.text.x=element_text(angle = 90, hjust=0.5,vjust=0.5), 
        axis.title.x = element_blank()) 

# 3.total sale per month of all years
Total_Month_Sale = aggregate(price_detail_amount ~ monthname, data, sum)
Total_Month_Sale = Total_Month_Sale[order(Total_Month_Sale$monthname), ]
#Total_Month_Sale <- Total_Month_Sale[order(match(Total_Month_Sale$monthname, month.name)), ]
print(Total_Month_Sale)

ggplot(Total_Month_Sale, aes(x=reorder(monthname, -price_detail_amount), y=price_detail_amount))+
geom_bar(stat = "identity", fill="steelblue")+
  labs(x="Name of the Month", y=" Price_Detail_Amount", title="Total Price_Detail_Amount per Month of all Years")+
theme(axis.text.x = element_text(angle=90, hjust=0.5,vjust=0.5),
                                  axis.title.x=element_blank())  

# 4.Top 10 most subscriber courses
a1 <-data %>%
  summarise(title, num_subscribers) %>%  
  arrange(desc(num_subscribers)) %>%
  head(10)
  view(a1)
  # plot graph
  ggplot(a1, aes(x=reorder(title, -num_subscribers), y=num_subscribers ))+
    geom_bar(stat="identity", fill="steelblue")+
    labs(x="Name of the Course", y="No. of Subscriber", title = "Top 10 most subscriber per course")+
    theme(axis.text.x = element_text(angle=90,hjust = 0.5,vjust = 0.5),
          axis.title.x = element_blank())
  
  # 5.Number of subscribers > 100000 per year

   data1 <- data1 %>%
    mutate(datetime = as.POSIXct(created, format = "%Y-%m-%dT%H:%M:%SZ")) %>%
    mutate(year = format(datetime, "%Y")) %>%
    filter(num_subscribers >= 100000) %>%
    group_by(year) %>%
    summarise(count1L = n())
    View(data1)
  
  # plot graph
  ggplot(data1, aes(x=reorder(year, -count1L), y=count1L ))+
    geom_bar(stat="identity", fill="steelblue")+
    labs(x="Year", y="Count of Subscriber", title = "Count of subscriber > 100000 per Year")+
    theme(axis.text.x = element_text(angle=90,hjust = 0.5,vjust = 0.5),
          axis.title.x = element_blank()) +
    geom_text(aes(label = count1L), vjust = -0.5, color = "white", size = 3, position = position_stack(vjust = 0.5))
    
  

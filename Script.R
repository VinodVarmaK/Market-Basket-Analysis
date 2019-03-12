library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)
library(dplyr)
library(magrittr)

#Data Pre-processing and exploring
retail <- read.csv('F:/Doodhwala/Market Basket/Market_Basket-July.csv')
str(retail)
retail <- retail[complete.cases(retail), ]
retail <- retail %>% mutate(product_name = as.factor(product_name))
#retail <- retail %>% mutate(Country = as.factor(Country))
retail$Date <- as.Date(retail$delivery_date)
retail$Time <- format(as.POSIXct(retail$delivery_date), format = "%H:%M:%S")
#retail$Time <- format(retail$delivery_date,"%H:%M:%S")
retail$plan_id <- as.numeric(as.character(retail$plan_id))
glimpse(retail)

#What time do people purchase online
retail$Time <- as.factor(retail$Time)
a <- hms(as.character(retail$Time))
retail$Time = hour(a)
retail %>% 
  ggplot(aes(x=Time)) + 
  geom_histogram(stat="count",fill="indianred")
remove.packages("ggplot2")
remove.packages("reshape2")
remove.packages("scales")
remove.packages("broom")
remove.packages("plyr")
pkg <- c("package:plyr")
lapply(pkg, detach, character.only = TRUE, unload = TRUE)

#How many items each customer buy
detach("package:plyr", unload=TRUE)
retail %>% 
  group_by(user_id) %>% 
  summarize(n_items = mean(quantity)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred", bins = 100) + 
  geom_rug()+
  coord_cartesian(xlim=c(0,20))

#Top 10 Best Sellers 
tmp <- retail %>% 
  group_by(product_mapping_id, product_name) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
tmp <- head(tmp, n=10)
tmp

tmp %>% 
  ggplot(aes(x=reorder(product_name,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()

#Association Rules of retail products
retail_sorted <- retail[order(retail$user_id),]
install.packages("plyr")
library(plyr)
itemList <- ddply(retail, c("user_id","Date"), 
                  function(df1)paste(df1$product_name, 
                                     collapse = ","))

#To check the transaction of products
itemList$user_id <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")
write.csv(itemList,"market_basket_Analysis.csv", quote = FALSE, row.names = FALSE)

#How many transactions we have and what they are
library(arules)
tr <- read.transactions('C:/Users/Vinod Varma/Desktop/R/market_basket_Analysis.csv', format = 'basket', sep=',')
tr
summary(tr)

#Item frequency plot
itemFrequencyPlot(tr, topN=20, type='absolute')

#Using Apriori algorithm 
#We pass supp=0.001 and conf=0.8 to return all the rules that have a support 
#of at least 0.1% and confidence of at least 80%.
rules <- apriori(tr, parameter = list(supp=0.001, conf=0.20))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

#Inspecting rules
inspect(rules[1:10])

#Plotting top rules
topRules <- rules[1:10]

plot(topRules)

plot(topRules, method="graph")

plot(topRules, method = "grouped")

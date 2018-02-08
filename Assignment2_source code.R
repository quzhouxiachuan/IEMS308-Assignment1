library(plyr)
library(tidyverse)
library(arules)
library(lubridate)
library(arulesViz)
setwd('./Desktop/association rules_hw/')
x=read.csv('./Desktop/association rules_hw/data_top8000.csv')
x=x[complete.cases(x),]

#100 best sellers 
tmp = x %>%
  group_by(sku) %>% 
  summarize(count = sum(quantity)) %>% 
  arrange(desc(count))

tmp[1:50,] %>%
  ggplot(aes(x=reorder(sku,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip() + labs(x='sku_code')

#x_sorted <- x[order(x$trans_id),]
#get the dataset for association rule input 
library(plyr)
itemList <- ddply(x,c("trans_id"), 
                  function(df1)paste(df1$sku, 
                                     collapse = ","))
colnames(itemList)[2] <- c("items")
write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = FALSE, col.names = FALSE)

#delete the ',' at the end of table 
tr <- read.transactions('/Users/yudeng/Desktop/association rules_hw/market_basket.csv', format = 'basket', sep = ',')
summary(tr)
rules <- apriori(tr, parameter = list(supp=0.1, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)
topRules <- rules[1:50]
plot(topRules)
plot(topRules, method="graph")
plot(topRules, method = "grouped")



library(plyr)
library(tidyverse)
library(arules)
library(lubridate)
library(arulesViz)

x=read.csv('./Desktop/test.csv')
x=x[complete.cases(x),]

#how many items each transction include? 
detach("package:plyr", unload=TRUE)
x %>% 
  group_by(trans_id) %>% 
  summarize(n_items = mean(quantity)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred", bins = 100000) + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))


#100 best sellers 
tmp = x %>%
  group_by(sku, description) %>% 
  summarize(count = sum(quantity)) %>% 
  arrange(desc(count))

tmp %>%
  ggplot(aes(x=reorder(description,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip() + labs(x='sku_code')

#x_sorted <- x[order(x$trans_id),]
#get the dataset for association rule input 
itemList <- ddply(x,c("trans_id"), 
                  function(df1)paste(df1$description, 
                                     collapse = ","))
colnames(itemList)[2] <- c("items")
write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = FALSE, col.names = FALSE)

#delete the ',' at the end of table 
tr <- read.transactions('market_basket.csv', format = 'basket', sep = ',')
rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)
topRules <- rules[1:6]
plot(topRules)
plot(topRules, method="graph")
plot(topRules, method = "grouped")
                        
                        
                        

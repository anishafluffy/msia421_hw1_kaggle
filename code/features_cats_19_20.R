library(dplyr)
library(ggplot2)
setwd("C:/Users/anish/Dropbox/Q2_Winter18/MSIA_421_DataMining/msia421_hw1_kaggle")

# read orders
dat = read.csv("data/orders.csv")
dat$orddt = as.Date(dat$orddate, "%d%b%Y")
head(dat)
dat$orddate = NULL

# read dependent variable
y = read.csv("data/booktrain.csv")
head(y)

#look for patterns in training data
train = inner_join(dat, y, by="id")

#update logtarg to if they bought or not
train$logtarg[train$logtarg > 0] = 1

#calc % of total for each category by buy or no buy
cats = train %>%
  group_by(category) %>%
  summarize(qty_0 = sum(qty[logtarg == 0]), qty_1 = sum(qty[logtarg > 0])) %>%
  mutate(pct_0 = qty_0/sum(qty_0), pct_1 = qty_1/sum(qty_1), diff = abs(pct_1 - pct_0)) %>%
  select(category, qty_0, qty_1, pct_0, pct_1, diff)

#plot
ggplot(data = cats, aes(category, diff)) + 
  geom_point()

#categories with most difference
cats[cats$diff > 0.015,]

#create feature variables
cat20 = dat %>%
  filter(category == 20) %>%
  distinct(id) %>%
  mutate(cat20 = 1)
    
cat19 = dat %>%
  filter(category == 19) %>%
  distinct(id) %>%
  mutate(cat19 = 1)

nrow(cat20)    
nrow(cat19)     
    
    
    
    
    
    
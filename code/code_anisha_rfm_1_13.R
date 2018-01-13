library(dplyr)
setwd("C:/Users/anish/Dropbox/Q2_Winter18/MSIA_421_DataMining/msia421_hw1_kaggle")

# read orders
dat = read.csv("data/orders.csv")
dat$orddt = as.Date(dat$orddate, "%d%b%Y")
head(dat)
dat$orddate = NULL
str(dat)
dim(dat)

#min date = "2007-11-04"
#min(dat$orddt)

#max date = "2014-07-31"
#max(dat$orddt)

# read dependent variable
y = read.csv("data/booktrain.csv")
head(y)

# do simple roll up
x = dat %>%
  group_by(id) %>%
  summarise(#f = n(), 
            recency_first = as.numeric(as.Date('2014-08-01') - min(orddt)), #time since first purchase
            recency_last = as.numeric(as.Date('2014-08-01') - max(orddt)), #time since last purchase
            frequency_qty = sum(qty), #number of items
            frequency_ord = n_distinct(ordnum), #number of distinct orders
            monetary_tot = sum(price), #total spent
            monetary_avg = mean(price) #average spent per item
            ) %>% 
  select(id, recency_first, recency_last, frequency_qty, frequency_ord, monetary_tot, monetary_avg)
head(x) 
dim(x)

# merge
all = left_join(x, y, by="id")
dim(all)
fit = lm(logtarg ~ log(frequency_qty + 1), all)
summary(fit)
all$yhat = predict(fit, all)
#length(yhat)
head(all)
test = is.na(all$logtarg)
write.csv(all[test, c(1,4)], "output/test_rfm.csv", row.names=F)

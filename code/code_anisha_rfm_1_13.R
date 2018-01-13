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
cor(x[-1])

# merge
all = left_join(x, y, by="id")
dim(all)

#baseline code: predict logtarg - only using qty
fit = lm(logtarg ~ log(frequency_qty + 1), all)
summary(fit)

#try to predict spend instead of logtarg - transform logtarg to spend
#logtarg = ln(spend = 1)
#spend = e^logtarg -1
all$spend = exp(all$logtarg) - 1
fit = lm(spend ~ recency_first + recency_last + frequency_qty + frequency_ord + monetary_tot + monetary_avg, all)
summary(fit)

#predict spend
all$yhat_spend = predict(fit, all)

#transform prediction back to logtarg
all$yhat = log(all$yhat_spend + 1)

head(all)
test = is.na(all$logtarg)
write.csv(all[test, c('id', 'yhat')], "output/test_rfm.csv", row.names=F)





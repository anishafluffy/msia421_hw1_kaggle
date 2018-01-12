library(dplyr)
#setwd("C:/Users/anish/Dropbox/Q2_Winter18/MSIA_421_DataMining/msia421_hw1_kaggle")

# read orders
dat = read.csv("data/orders.csv")
dat$orddt = as.Date(dat$orddate, "%d%b%Y")
head(dat)
dat$orddate = NULL
str(dat)
dim(dat)

# read dependent variable
y = read.csv("data/booktrain.csv")
head(y)

# do simple roll up
x = dat %>%
  group_by(id) %>%
  summarise(f=n()) %>% 
  select(id, f)
head(x) 
dim(x)

# merge
all = left_join(x, y, by="id")
dim(all)
fit = lm(logtarg ~ log(f+1), all)
summary(fit)
all$yhat = predict(fit, all)
length(yhat)
head(all)
test = is.na(all$logtarg)
write.csv(all[test, c(1,4)], "output/test.csv", row.names=F)

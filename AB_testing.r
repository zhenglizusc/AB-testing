library(sqldf)

# load data
data <- read.csv("abtest_example_ctr.csv")

#summarize data
summary(data)

#Note there are NA's for userid
sum(is.na(data$userid))/nrow(data) #~2%

#check for mixed assignment
sqldf("select count(1) from (select userid, count(distinct(groups)) 
      from data group by userid having count(distinct(groups)) >1) as a")
#there are mixed assigned users

#check if multiple device per user, multiple user per device
sqldf("select count(1) from (select userid, count(distinct(deviceid))
      from data group by userid having count(distinct(deviceid))>1) as a")

sqldf("select count(1) from (select deviceid, count(distinct(userid))
      from data group by deviceid having count(distinct(userid))>1) as a")
# there are some multiple device per user, and multiple user per device

### check NA/mixed/multiple device is random
# create dummy, if any problems 1, else 0. 
Pb_miss=1*is.na(data$userid)

userid_mix=as.numeric(sqldf("select userid from (select userid, count(distinct(groups)) 
                  from data group by userid having count(distinct(groups)) >1) as a")[[1]])
fun_Pb_mix=function(x){x %in%userid_mix}
Pb_mix=1*sapply(data$userid,FUN=fun_Pb_mix)

userid_mulD=as.numeric(sqldf("select userid from (select userid, count(distinct(deviceid)) 
                  from data group by userid having count(distinct(deviceid))>1) as a")[[1]])
fun_Pb_mulD=function(x){x %in%userid_mulD}
Pb_mulD=1*sapply(data$userid,FUN=fun_Pb_mulD)

deviceid_mulU=as.numeric(sqldf("select deviceid from (select deviceid, count(distinct(userid)) 
                  from data group by deviceid having count(distinct(userid)) >1) as a")[[1]])
fun_Pb_mulU=function(x){x %in%deviceid_mulU}
Pb_mulU=1*sapply(data$deviceid,FUN=fun_Pb_mulU)

# For simplicity, I create a combined column 1/0 if any problems. In real projects, 
# you may want to do this separately for each problem cuz they may have different causes
data$pb_all = dd = apply(matrix(cbind(Pb_miss, Pb_mix, Pb_mulD, Pb_mulU), nrow = nrow(data)), 1, max)

# run model of dummy with other covariates, see if any covariates have strong correlation with having problematic assignment
# let's start with a simple model as there are not many covariates
pb_mod = glm(pb_all ~ country + groups + device + date + views + clicks + revenue, data, family = 'binomial')
summary(pb_mod)
# some country, device have significant result, need to deep dive, if any bug exists, consider checking???
# by type of problem (other device type have more deviced IDs? more likely to be missing?)
aggregate(Pb_miss, by = list(data$device), FUN = mean)
aggregate(Pb_mix, by = list(data$device), FUN = mean)
aggregate(Pb_mulD, by = list(data$device), FUN = mean)
aggregate(Pb_mulU, by = list(data$device), FUN = mean)
# Other device has one ID mapping multiple users. Is this expected? Talk to Engineers, is this Logging problem?


# blox of key metrics, views, clicks, ctr, problem
boxplot(data$views~Pb_miss)
boxplot(data$clicks~Pb_miss)

boxplot(data$views~Pb_mix)
boxplot(data$clicks~Pb_mix)

boxplot(data$views~Pb_mulD)
boxplot(data$clicks~Pb_mulD)

boxplot(data$views~Pb_mulU)
boxplot(data$clicks~Pb_mulU)

# For simplicity, throw away the problematic assignments. In reality, do not do this by default. Need to go through
# checks carefully. 
data=data[data$pb_all == 0,]

# sanity check, check before experiment, metrics are comparable, no sig diff between test/control
# day1-day3 data was before experiment start
data$date=as.Date(data$date)
data_before<-data[data$date<(min(data$date)+3),]
data_start<-data[data$date>=(min(data$date)+3),]

# compare aggregated CTR between test/control before experiment start
x1=sum(data_before$clicks[data_before$groups=='treatment'])
x2=sum(data_before$clicks[data_before$groups=='control'])
n1=sum(data_before$views[data_before$groups=='treatment'])
n2=sum(data_before$views[data_before$groups=='control'])
prop.test(x=c(x1,x2),n=c(n1,n2),alternative = 'two.sided')
# also compare clicks, views

# compare other covariates comparable, take device as example
# You can try statistical tests, think about what test? 
# For this case, let's start with simple visualization, if seems imbalanced, run tests to confirm
par(mfrow = c(2,1))
barplot(prop.table(table(data_before[data_before$groups == 'treatment','device'])), main = 'treatment')
barplot(prop.table(table(data_before[data_before$groups == 'control','device'])), main = 'control')

#### Hypothesis Testing #####
# run test, not significant
x1=sum(data_start$clicks[data_start$groups=='treatment'])
x2=sum(data_start$clicks[data_start$groups=='control'])
n1=sum(data_start$views[data_start$groups=='treatment'])
n2=sum(data_start$views[data_start$groups=='control'])
prop.test(x=c(x1,x2),n=c(n1,n2),alternative = 'two.sided')


# by subgroup, write a function, apply, which signficant
ztest_by_subgroup<-function(data_start, bycol, val)
{
  data_use=data_start[data_start[bycol]==val,]
  x1=sum(data_use$clicks[data_use$groups=='treatment'])
  x2=sum(data_use$clicks[data_use$groups=='control'])
  n1=sum(data_use$views[data_use$groups=='treatment'])
  n2=sum(data_use$views[data_use$groups=='control'])
  return(prop.test(x=c(x1,x2),n=c(n1,n2),alternative = 'two.sided'))
}

test_bydevice = data.frame(matrix(nrow = 0, ncol = 6,
                          dimnames = list(NULL, 
                                          c('device','p.value','ctr_treatment',
                                            'ctr_control', 'ci.low','ci.high'))))
for (i in 1:length(unique(data$device))){
  device = as.character(unique(data$device)[i])
  test = ztest_by_subgroup(data_start, 'device', device)
  # you can check available statistics using names(test)
  testresult = data.frame('device' = device, 
                          'p.value' = test$p.value, 
                          'ctr_treatment' = test$estimate[1], 
                          'ctr_control' = test$estimate[2],
                          'ci.low' = test$conf.int[1],
                          'ci.high' = test$conf.int[2])
  test_bydevice = rbind(test_bydevice,testresult)
}
test_bydevice
# Ios significant, figure out why only works for Ios. Discuss with Eng & PM

############### Revenue #####################
hist(data$revenue)
boxplot(Revenue)
# highly skewed
sum(data$revenue == 0)/nrow(data) 

#only consider revenue with revenue>0
Revenue<-data$revenue[data$revenue>0]
hist(Revenue)
boxplot(Revenue)

# winsorization, capping
bound=quantile(Revenue,0.99)
Revenue_win<-Revenue
Revenue_win[Revenue>bound]=bound
hist(Revenue_win)
# or do it with built-in function
# library(robustHD)
# Winsorize(data$revenue, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),  na.rm = FALSE)

# compare normal(CLT) & bootstrap distribution for estimator
#CLT
E_mean=mean(Revenue_win)
E_var=var(Revenue_win)/length(Revenue_win)

#bootstrap
True=mean(Revenue_win)
btsample = rep(0, 1000)
for (i in 1:1000){
  sample = Revenue_win[sample(length(Revenue_win), length(Revenue_win), replace =TRUE)]
  btsample[i] = mean(sample)
}
var_bt = var(btsample)

E_var 
var_bt

# what if the statistics of interest is 75% percentile of revenue if has spending?
mean = quantile(Revenue_win, 0.75)
btsample = rep(0, 1000)
for (i in 1:1000){
  sample = Revenue_win[sample(length(Revenue_win), length(Revenue_win), replace =TRUE)]
  btsample[i] = quantile(sample, 0.75)
}
var_bt = var(btsample)
var_bt

####### result analysis ##########
# Regression Adjustment & diff-in-diff analysis
bound=quantile(Revenue,0.999)
data_before$revenue_win = ifelse(data_before$revenue>bound, bound, data_before$revenue)
data_start$revenue_win = ifelse(data_start$revenue>bound, bound, data_start$revenue)

# regular t.test
x1=data_start$revenue_win[data_start$groups=='treatment']
x2=data_start$revenue_win[data_start$groups=='control']
t.test(x = x1, y = x2,alternative = 'two.sided')

# there might be bias prior to experiment start
daily_rev_post = sqldf("select userid, country, device, groups, sum(revenue_win)/11 as rev_post 
                       from data_start group by 1,2,3,4")
daily_rev_pre = sqldf("select userid, country, device, groups, date, sum(revenue_win)/3 as rev_pre 
                      from data_before group by 1,2,3,4")
daily_rev = sqldf('select a.*, coalesce(rev_pre,0) as rev_pre from daily_rev_post a left outer join daily_rev_pre b on a.userid = b.userid')

# diff in diff t test
x1= with(daily_rev[daily_rev$groups=='treatment',], rev_post - rev_pre)
x2= with(daily_rev[daily_rev$groups=='control',], rev_post - rev_pre)
t.test(x = x1, y = x2,alternative = 'two.sided')

# regression adjustment pre diff
rev_mod = lm(rev_post ~ groups + country + device + rev_pre, data = daily_rev)
summary(rev_mod)

# cohort analysis, change, over time change, 14 dates, 4th day start, line chart with CI by date
# look at users enrolled on day 4
d4 = data_start[data_start$userid %in% data_start[data_start$date == "2017-05-11", 'userid'],]
test_bydate = data.frame(matrix(nrow = 0, ncol = 6,
                                  dimnames = list(NULL, 
                                                  c('date','p.value','ctr_treatment',
                                                    'ctr_control', 'ci.low','ci.high'))))

for (i in 1:(length(unique(data$date))-3)){
  date = as.character(sort(unique(data$date))[i+3])
  test = ztest_by_subgroup(d4, 'date', date)
  # you can check available statistics using names(test)
  testresult = data.frame('date' = date, 
                          'p.value' = test$p.value, 
                          'ctr_treatment' = test$estimate[1], 
                          'ctr_control' = test$estimate[2],
                          'ci.low' = test$conf.int[1],
                          'ci.high' = test$conf.int[2])
  test_bydate = rbind(test_bydate,testresult)
}

par(mfrow = c(1,1))
plot(ctr_treatment - ctr_control ~ date, data = test_bydate, ylim = c(-0.1, 0.1), ylab = 'P-t - P-c')
plot(ci.low ~ date, data = test_bydate, lty = 4, col = 3, add = TRUE, ylab = '')
plot(ci.high ~ date, data = test_bydate, lty = 4, col = 3, add = TRUE, ylab = '')
abline(h = 0, lty = 2)


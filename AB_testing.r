# load data
data = read.csv("abtest_example_ctr.csv")

# transform the groups into 0/1; create a new column: ctr(click through rate)
library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
data <- data %>%
  mutate(groups = ifelse(groups == "treatment",1,0))


ctr = data$clicks/data$views
data = data.frame(data,ctr)

# summary data
summary(data)
##      userid      country       groups          deviceid         device    
##  Min.   : 1000   CA:4571   Min.   :0.0000   Min.   : 5000   Android:7003  
##  1st Qu.: 3256   CN:4576   1st Qu.:0.0000   1st Qu.: 8758   Ios    :4583  
##  Median : 5450   GB:4631   Median :1.0000   Median :12538   Other  :4717  
##  Mean   : 5485   US:9182   Mean   :0.5009   Mean   :12566   Web    :6657  
##  3rd Qu.: 7717             3rd Qu.:1.0000   3rd Qu.:16409                 
##  Max.   :10000             Max.   :1.0000   Max.   :20000                 
##  NA's   :275                                                              
##     sellerid         itemid             date           views       
##  Min.   :100.0   Min.   :1000   2017-05-16: 1721   Min.   : 0.000  
##  1st Qu.:203.0   1st Qu.:1508   2017-05-15: 1701   1st Qu.: 4.000  
##  Median :304.0   Median :1994   2017-05-13: 1674   Median : 6.000  
##  Mean   :302.2   Mean   :1998   2017-05-09: 1657   Mean   : 5.796  
##  3rd Qu.:402.0   3rd Qu.:2497   2017-05-11: 1650   3rd Qu.: 7.000  
##  Max.   :500.0   Max.   :3000   2017-05-17: 1650   Max.   :20.000  
##                                 (Other)   :12907                   
##      clicks         revenue             ctr        
##  Min.   :0.000   Min.   :   0.00   Min.   :0.0000  
##  1st Qu.:0.000   1st Qu.:   0.00   1st Qu.:0.0000  
##  Median :1.000   Median :   0.00   Median :0.2000  
##  Mean   :1.175   Mean   :  11.97   Mean   :0.2018  
##  3rd Qu.:2.000   3rd Qu.:   0.00   3rd Qu.:0.3333  
##  Max.   :8.000   Max.   :1024.12   Max.   :1.0000  
##                                    NA's   :69
# check NA in userid
sum(is.na(data$userid))/nrow(data)
## [1] 0.01197735
Pb_miss=1*is.na(data$userid)

# select records with normal userid
data$pb_okay = apply(matrix(cbind(Pb_miss), nrow = nrow(data)), 1, max)
data=data[data$pb_okay == 0,]

# seperate the data into two groups: 
# day1-day3 data:before experiment start
# day4-day14 data: the experimengt is running
data$date=as.Date(data$date)
data_before<-data[data$date<(min(data$date)+3),]
data_start<-data[data$date>=(min(data$date)+3),]


# AA Testing: compare CTR, clicks, views and revenue between test/control before experiment start
a1=data_before$clicks[data_before$groups==1]
a2=data_before$clicks[data_before$groups==0]
b1=data_before$views[data_before$groups==1]
b2=data_before$views[data_before$groups==0]
c1=data_before$ctr[data_before$groups==1]
c2=data_before$ctr[data_before$groups==0]
d1=data_before$revenue[data_before$groups==1]
d2=data_before$revenue[data_before$groups==0]
t.test(x=a1,y=a2,alternative = 'two.sided')
## 
##  Welch Two Sample t-test
## 
## data:  a1 and a2
## t = 0.52552, df = 4798.3, p-value = 0.5992
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.04257317  0.07375644
## sample estimates:
## mean of x mean of y 
##  1.055441  1.039849
t.test(x=b1,y=b2,alternative = 'two.sided')
## 
##  Welch Two Sample t-test
## 
## data:  b1 and b2
## t = 0.40943, df = 4798.5, p-value = 0.6822
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.1040833  0.1590335
## sample estimates:
## mean of x mean of y 
##  5.318163  5.290688
t.test(x=c1,y=c2,alternative = 'two.sided')
## 
##  Welch Two Sample t-test
## 
## data:  c1 and c2
## t = 0.12361, df = 4777.9, p-value = 0.9016
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.01012417  0.01148683
## sample estimates:
## mean of x mean of y 
## 0.1946201 0.1939388
t.test(x=d1,y=d2,alternative = 'two.sided')
## 
##  Welch Two Sample t-test
## 
## data:  d1 and d2
## t = 1.4133, df = 4433.1, p-value = 0.1576
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.6086642  3.7526440
## sample estimates:
## mean of x mean of y 
## 11.333644  9.761654
# the control and test groups are comparable


#### Hypothesis Testing - CTR #####
# run test, not significant
x1=data_start$ctr[data_start$groups==1]
x2=data_start$ctr[data_start$groups==0]
t.test(x=x1,y=x2,alternative = 'two.sided')
## 
##  Welch Two Sample t-test
## 
## data:  x1 and x2
## t = 1.2029, df = 17834, p-value = 0.229
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.002103147  0.008785661
## sample estimates:
## mean of x mean of y 
## 0.2053755 0.2020342
# the difference between test/control group is not significant


#### Hypothesis Testing - Revenue #####
hist(data$revenue)
 
boxplot(data$revenue)
 
# highly skewed
sum(data$revenue == 0)/nrow(data) 
## [1] 0.8901477
#only consider revenue with revenue>0
revenue_win<-data$revenue[data$revenue>0]
hist(revenue_win)
 
boxplot(revenue_win)
 
#deal with outliers
bound=quantile(revenue_win,0.99)
bound
##      99% 
## 260.9183
revenue_win[revenue_win>bound]=bound
hist(revenue_win)

# Regression Adjustment
bound=quantile(revenue_win,0.999)
data_before$revenue_win = ifelse(data_before$revenue>bound, bound, data_before$revenue)
data_start$revenue_win = ifelse(data_start$revenue>bound, bound, data_start$revenue)

## t test for revenue
y1 = data_start$revenue_win[data_start$groups ==1]
y2 = data_start$revenue_win[data_start$groups ==0]
t.test(x=y1,y=y2,alternative = "two.sided")
## 
##  Welch Two Sample t-test
## 
## data:  y1 and y2
## t = -1.7929, df = 17837, p-value = 0.07301
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.02020748  0.09000804
## sample estimates:
## mean of x mean of y 
##  11.69437  12.65947
# the difference between test/control group is not significant

# set dir
setwd('/Users/mikhailrybalchenko/Projects/Marketing cases/AB_test_analysis/')


#========libraries and data load============
# load libraries
library('tidyverse')
library('ggplot2')
library('broom')
library('rpart')
library('rpart.plot')
library('rattle')	

# load data
testData <- read_csv('test_table.csv')
userData <- read_csv('user_table.csv')

# let's join tables by user_id
data <- inner_join(testData, userData, by='user_id')

# let's check the summary of the data
summary(data)

# check for missing data column-wise
sapply(data, function(x) sum(is.na(x)))
# there are 454 records with missing sex, age and country values

# let's check conversion by country before test
data %>% filter(test==0) %>% group_by(country) %>% summarise(cr = mean(conversion)) %>% arrange(desc(cr))
# Spain has the highest conversion before the test

# Spain translation stays the same, so we'll exclude Spain users from data
dataTest <- data %>% filter(country != 'Spain')

# let's group by country and test and compare conversion for test and control groups for each date
expByCountry <- dataTest %>% group_by(country, test) %>% summarise(cr = mean(conversion))
expByCountry

# let's plot it 
ggplot(expByCountry, aes(x = country,y = cr, group=country, fill=as.factor(test))) + geom_col(position = "dodge2")
# we can see that for some countries CR has improved

# let's run a T-test to compare conversion for test and control groups (Welch test assumes that variances are not equal)

t.test(dataTest$conversion[dataTest$test == 1], dataTest$conversion[dataTest$test == 0])
# the true difference in means for test and control is not equal to zero
# with the higher conversion in control - 4.8% vs 4.3% in test group
# the group difference is between -0.006181421 -0.003579837
# it's a 95 percent confidence interval


# let's calculate t-tests on country level
testByCountry <- dataTest %>% group_by(country) %>% summarize(pvalue = t.test(conversion[test==1], conversion[test==0])$p.value,
             conversion_test = t.test(conversion[test==1],conversion[test==0])$estimate[1],
             conversion_control = t.test(conversion[test==1],conversion[test==0])$estimate[2]) %>% arrange (pvalue)
testByCountry

# assesing test results on country level shows that conversion on the test-sites are not statistically different.


# remove conversion variable from data
#dataTestNoConv <- dataTest[,-8]

# build a decision tree on a data without conversion variable and check how users are splitted
#tree = rpart(test ~ .,dataTestNoConv, control = rpart.control(minbucket = nrow(dataTestNoConv)/100, maxdepth =2))
#rpart.plot(tree)
#fancyRpartPlot(tree)



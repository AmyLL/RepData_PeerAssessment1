# PA1_temlate.Rmd

---

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
knitr::opts_chunk$set(cache=TRUE, echo=TRUE)
```
---
#Loading and Processing the Data

```r
dat <- read.csv(unzip("repdata_data_activity.zip"), stringsAsFactors = FALSE)
```
Confirm data and check for NA's

```r
str(dat)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(dat)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```
There are 17568 observations in the data and `r summary(dat$steps[7]) NA's in the step count.
In order to evaluate the total number of steps taken per day, the data is grouped by day and the step count summed for each date in the data set.

#What is the Mean Total Number of Steps per Day?
Get total number of steps per day

```r
byDay <- group_by(dat, date)
byDay <- summarize(byDay, totSteps = sum(steps, na.rm=TRUE))
mnSteps <- round(mean(byDay$totSteps), digits=0)
mdSteps <- round(median(byDay$totSteps), digits=0)
```
Check results:

```r
head(byDay)
```

```
## Source: local data frame [6 x 2]
## 
##         date totSteps
##        (chr)    (int)
## 1 2012-10-01        0
## 2 2012-10-02      126
## 3 2012-10-03    11352
## 4 2012-10-04    12116
## 5 2012-10-05    13294
## 6 2012-10-06    15420
```

```r
mnSteps
```

```
## [1] 9354
```

```r
mdSteps
```

```
## [1] 10395
```
Histogram of Total Steps per Day

```r
par("usr")
```

```
## [1] 0 1 0 1
```

```r
hist(byDay$totSteps, breaks=10, col = "red", xlab = "Steps per Day", main = "Total Number of Steps per Day")
legend("topleft", legend = c(paste("Mean =", mnSteps), paste("Median =", mdSteps)))
```

![](PA1_template_files/figure-html/plot1-1.png)\
#What is the Daily Activity Pattern?
Get Average Daily Pattern

```r
byInterval <- group_by(dat, interval)
byInterval <- summarize(byInterval, meanSteps = mean(steps, na.rm=TRUE))
```
Plot Daily Pattern

```r
maxStep <- round(max(byInterval$meanSteps))
ind <- which.max(byInterval$meanSteps)
maxInt <- byInterval[ind, 1]
plot(byInterval$interval, byInterval$meanSteps, type="l", ylab=("Average Number of Steps"), xlab=("Time Interval"), main=("Daily Activity Pattern"))
legend("topright", legend = c(paste("Maximum number of steps is ", maxStep), paste("which occurs during interval ", maxInt)))
```

![](PA1_template_files/figure-html/plot2-1.png)\
#Imputing Missing Values
Replace NAs with mean steps for that interval and reanlyze frequencies, mean and median

```r
byInterval.impute <- group_by(dat, interval)
rowIndex <- which(is.na(byInterval.impute$steps)) #missing values in data
for (i in rowIndex){ #row numbers for missing values
     for (j in 1:length(byInterval$interval)){     # get index for interval and mean step activity for that interval
          if (byInterval.impute[i,3] == byInterval[j,1]){byInterval.impute[i,1] = byInterval[j,2]}
     }
}  

byDay.impute <- group_by(byInterval.impute, date) %>% summarize(totSteps.impute =sum(steps))
mnSteps.impute <- round(mean(byDay.impute$totSteps.impute), digits=0)
mdSteps.impute <- round(median(byDay.impute$totSteps.impute), digits=0)
```
Plot Histogram

```r
hist(byDay.impute$totSteps, breaks=10, col="red", xlab = "Steps per Day", main = c("Total Number of Steps per Day:", "NAs Replaced with Interval Mean"))     
legend("topleft", legend = c(paste("Mean =", mnSteps.impute), paste("Median =", mdSteps.impute)))
```

![](PA1_template_files/figure-html/plot3-1.png)\
#Are There Differences In Activity Patterns Between Weekdays and Weekends
Compare Weekday and Weekend Activity (Imputed Means)

```r
byInterval.impute$stepDay <-as.Date(byInterval.impute$date)
weekendNames <- c("Saturday", "Sunday")
byInterval.impute$stepDay <- factor((weekdays(byInterval.impute$stepDay) %in% weekendNames), levels = c("TRUE", "FALSE"), labels = c("weekend", "weekday"))
```
Panel plot

```r
par(mfrow=c(2,1))
weekdayActivity <- subset(byInterval.impute, stepDay == "weekday")
weekdayActivity <- group_by(weekdayActivity, interval)
weekdayActivity <- summarize(weekdayActivity, meanActivity=mean(steps))
with(weekdayActivity, plot(meanActivity~interval, type="l", ylab="Average Number of Steps", xlab="Interval", main="Average Weekday Activity"))

weekendActivity <- subset(byInterval.impute, stepDay == "weekend")
weekendActivity <- group_by(weekendActivity, interval)
weekendActivity <- summarize(weekendActivity, meanActivity=mean(steps))
with(weekendActivity, plot(meanActivity~interval, type="l", ylab="Average Number of Steps", xlab="Interval", main="Average Weekend Activity"))
```

![](PA1_template_files/figure-html/plot4-1.png)\


---
title: "Reproducible Research, Week Two Assignment"
author: "jamaas"
date: "17/07/2014"
output: html_document
---

# Background

This is an assignment to analyse data from a personal activity monitoring device that records, among other variables, the number of steps the person takes per five minute interval.

#Load Data

First, lets make sure there are no variables hanging around in memory so we will clear them all, then to start lets read in the data into a data.frame.


```r
rm(list=ls())
first.data <- read.csv("./activity.csv", header=TRUE)
```

Then look at the size of the data set with dim() and the top five rows, to see what it looks like.

```r
dim(first.data)
```

```
## [1] 17568     3
```

```r
head(first.data, 5)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
```

So from this we can see that we have 17568 records (rows) comprised of three variables (columns), which are named "steps", "date", and "interval" respectively.  We also note that there is a considerable amount of missing data because the first five rows all have a "NA" for number of steps.  This would be logical, because if the recording of number of steps begins at 00:00, or midnight each day, then it is unlikely that the person would take many steps as they are quite likely sleeping!

Therefore to follow on from this and get a little more information, get a quick summary of the data using the summary() command.


```r
summary(first.data)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##  NA's   :2304    (Other)   :15840
```
From this output we can see that the "steps" variable contains 2304 "NA" values

Next, find out how many missing values there are for each of the individual days in the study.  Set a boolean variable for each NA.


```r
steps.per.day <- aggregate(steps ~ date, first.data, sum)
hist(steps.per.day$steps, breaks = 20, main = "Number of Steps per Day",
     xlab = "Number of Steps per Day", ylab = "Frequency")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

#Mean total steps per day

Then we want the mean and median steps per day so again use the aggregate function


```r
total.steps.per.day <- aggregate(first.data$steps, 
                                 list(date = first.data$date), FUN = sum)

head(total.steps.per.day, 5)
```

```
##         date     x
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
```

Lets calculate the mean and median steps per day.


```r
(mean.steps.per.day <- mean(total.steps.per.day$x, na.rm = TRUE) )
```

```
## [1] 10766
```

```r
(median.steps.per.day <- median(total.steps.per.day$x, na.rm = TRUE) )
```

```
## [1] 10765
```
Both correspond quite well with the plot above.

#Daily Activity Pattern

Now to get the mean number of steps, per individual five minute interval, we must aggregate by interval.


```r
mean.steps.per.interval <- aggregate(first.data$steps, list(interval = first.data$interval), 
                                                        FUN = mean, na.rm = TRUE)
head(mean.steps.per.interval, 5)
```

```
##   interval       x
## 1        0 1.71698
## 2        5 0.33962
## 3       10 0.13208
## 4       15 0.15094
## 5       20 0.07547
```

There are twelve, five minute intervals per hour, and 24 hours in a day, the product of which is 288 individual five minute intervals per day.  Therefore the total number of days in the data set is the number of rows divided by 288.


```r
number.days <- nrow(first.data) / 288
```

To create our time series object for plotting the mean number of steps, we create a new time series interval column in the mean.steps.per.interval object.  Then we do the plot.


```r
mean.steps.per.interval$new.interval <- rep(1:288)

plot(mean.steps.per.interval$new.interval, mean.steps.per.interval$x, type = "l",
     main = "Mean Steps Per Interval", xlab = "Interval", ylab = "Mean Steps Per Interval")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

To get the interval that has the maximum number of steps, averaged over all days we need to find the interval number in column 1 of mean.steps.per.interval corresponding to the row where the value of mean.steps.per.interval$x, the number of steps, is the maximum.

The maximum number of steps is calculated as:
(max(mean.steps.per.interval$x))


```r
(max.steps.interval <- mean.steps.per.interval 
            [ mean.steps.per.interval$x == max(mean.steps.per.interval$x), 1])
```

```
## [1] 835
```

The maximum number of steps, averaged over all days, occurs at the time interval corresponding to 835.

#Impute Missing Values

Now some work on missing data and imputation of missing data.  First of all we need to find out how many complete and incomplete cases we have in the original data set.  To do this we sum the number of cases that are not complete in first.data.


```r
(number.incomplete.cases <- sum (!complete.cases(first.data)))
```

```
## [1] 2304
```
Therefore we have 2304 rows containing missing data, specified by a "NA" value. 

My strategy for imputation is simply to impute missing values with the mean across all days for that particular time interval.  This will not affect the overall mean, but might impact the median slightly.

First add a row number variable for the final merge of the imputed values back into the original data set.  The next step is to subset these rows that contain "NA" values into na.rows.  Then the previously calculated means.per.interval data is merged into the na.rows data to replace the missing values.  Then I rearrange the columns back to correspond with those of first.data and correct the column names.  Then the imputed data, newdata, is row-bound onto the bottom of a data set containing all complete cases.  Finally the new imputed total data set is sorted, using the "order" command and the row.num variable to put the rows back in their original order.  The new complete data set is called "second.data."


```r
first.data$row.num <- 1:nrow(first.data)
na.rows <- first.data[!complete.cases(first.data), ]
newdata <- merge(na.rows, mean.steps.per.interval, by = "interval")
newdata <- newdata[ , c(5, 3, 1, 4)]
colnames(newdata) <- c("steps", "date", "interval", "row.num")
second.data <- rbind(first.data[complete.cases(first.data), ], newdata)
second.data <- second.data[with(second.data, order(row.num)), ]
```

Now we will compare some descriptive statistics from the original data set, first.data which contains "NA" values, and the new data set, second.data where missing values have been replaced by the mean of all available values for the corresponding interval.  So lets have a look at total steps per day.


```r
new.total.steps.per.day <- aggregate(second.data$steps, list(date = second.data$date), 
                                                        FUN = sum)
(new.mean.steps.per.day <- mean(new.total.steps.per.day$x, na.rm = TRUE) )
```

```
## [1] 10766
```

```r
(new.median.steps.per.day <- median(new.total.steps.per.day$x, na.rm = TRUE) )
```

```
## [1] 10766
```

Lets have a look at what the histogram looks like using the second data set with imputed missing values.


```r
hist(new.total.steps.per.day$x, breaks = 20, main = "Number of Steps per Day", 
     xlab = "Number of Steps per Day", ylab = "Frequency")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 

```r
(diff.mean.total.steps <- mean.steps.per.day - new.mean.steps.per.day)
```

```
## [1] 0
```

```r
(diff.median.total.steps <- median.steps.per.day - new.median.steps.per.day)
```

```
## [1] -1.189
```
The difference between the mean total steps per day, and median total steps per day of the original and imputed data sets are 0 days and -1.1887 days, respectively.

#Weekend vs. weekday patterns

Now lets look at the difference in number of steps between weekdays and weekends.  First convert the date column to a format that can be used for weekdays, 0 & 6 correspond to Sunday and Saturday, respectively.  Days 1-5 correspond to Monday to Friday, respectively.  Then subset the data into two subsets corresponding to weekend and weekday data.


```r
second.data$new.date <- as.POSIXlt(second.data$date,format="%Y-%m-%d")$wday
second.data.weekend <- subset(second.data, new.date %in% c("0","6"))
second.data.weekday <- subset(second.data, new.date %in% c("1":"5"))
```

Now prepare the data and plots.


```r
weekday.mean.steps.per.interval <- aggregate(second.data.weekday$steps, 
                                             list(interval = second.data.weekday$interval), 
                                             FUN = mean, na.rm = TRUE)

weekend.mean.steps.per.interval <- aggregate(second.data.weekend$steps, 
                                             list(interval = second.data.weekend$interval), 
                                             FUN = mean, na.rm = TRUE)
par(mfrow=c(2,1))

plot(weekend.mean.steps.per.interval$interval, weekend.mean.steps.per.interval$x, type = "l",
     main = "Weekend", xlab = "", ylab = "Mean Steps Per Interval")

plot(weekday.mean.steps.per.interval$interval, weekday.mean.steps.per.interval$x, type = "l",
     main = "Weekday", xlab = "Interval", ylab = "Mean Steps Per Interval")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 
#End of Assignment :-)

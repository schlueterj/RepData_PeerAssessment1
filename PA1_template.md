---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

First, we will unzip and load the data, transform it to a data table and do a quick exploration


```r
unzip("./activity.zip")
activity <- read.csv("./activity.csv")

library(data.table)
activity <- data.table(activity)
summary(activity)
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

```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
head(activity,5)
```

```
##    steps       date interval
## 1:    NA 2012-10-01        0
## 2:    NA 2012-10-01        5
## 3:    NA 2012-10-01       10
## 4:    NA 2012-10-01       15
## 5:    NA 2012-10-01       20
```

```r
lapply(activity, class)
```

```
## $steps
## [1] "integer"
## 
## $date
## [1] "character"
## 
## $interval
## [1] "integer"
```
Before starting our analysis, we will transform the date column and transforming the data frame to a data table

```r
activity$date <- as.POSIXct(activity$date, format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?

We calculate the total number of steps taken per day:


```r
steps_per_day <-activity[,.(steps=sum(steps,na.rm=TRUE)),by=date]
```
 
Here is a histogram of the total number of steps taken each day:


```r
hist(steps_per_day$steps,col = "Steelblue", main = "Total number of steps taken per day", xlab = "Total steps per day")
```

![](PA1_template_files/figure-html/plot1-1.png)<!-- -->

Here is the mean of the total number of steps taken per day:


```r
mean(steps_per_day$steps)
```

```
## [1] 9354.23
```

Here is the median of the total number of steps taken per day:


```r
median(steps_per_day$steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
average_steps_per_interval <-activity[,.(steps_mean=mean(steps,na.rm=TRUE)),by=interval]
with(average_steps_per_interval, plot(interval, steps_mean, type = "l",col = "darkblue", main = "Average number of steps per interval", xlab = "Interval", ylab = "Average number of steps"))
```

![](PA1_template_files/figure-html/plot2-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
average_steps_per_interval[which.max(average_steps_per_interval$steps_mean),]$interval
```

```
## [1] 835
```

## Imputing missing values

There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
imputed_steps <- average_steps_per_interval$steps_mean[match(activity$interval, average_steps_per_interval$interval)]
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_imputed <- transform(activity, steps = ifelse(is.na(activity$steps), yes = imputed_steps, no = activity$steps))
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps_per_day_imputed <-activity_imputed[,.(steps=sum(steps,na.rm=TRUE)),by=date]
```
 
Here is a histogram of the total number of steps taken each day:


```r
hist(steps_per_day_imputed$steps,col = "Steelblue", main = "Total number of steps taken per day", xlab = "Total steps per day")
```

![](PA1_template_files/figure-html/plot3-1.png)<!-- -->

Here is the mean of the total number of steps taken per day:


```r
mean(steps_per_day_imputed$steps)
```

```
## [1] 10766.19
```

Here is the median of the total number of steps taken per day:


```r
median(steps_per_day_imputed$steps)
```

```
## [1] 10766.19
```

These values differ from the estimates from the first part of the assignment.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
##Create weekday column in dataset activity_imputed
activity_imputed$datetype <- sapply(activity_imputed$date, function(x) {
        if (weekdays(x) == "Samstag" | weekdays(x) =="Sonntag") 
                {y <- "weekend"} else 
                {y <- "weekday"}
                y
        })

##Convert column to factor
activity_imputed$datetype <- factor(activity_imputed$datetype)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
average_steps_per_interval_week <- activity_imputed[,.(steps_mean=mean(steps,na.rm=TRUE)),by=.(interval,datetype)]
library(lattice)
xyplot(steps_mean ~ interval | datetype, average_steps_per_interval_week, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/plot4-1.png)<!-- -->


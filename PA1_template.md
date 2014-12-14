# Reproducible Research: Peer Assessment 1


##Loading and preprocessing the data


```r
df_all <- read.csv('activity.csv')
```

##What is mean total number of steps taken per day?
First, we remove all NA's. We then calculate the total number of steps taken each day and plot a histogram

```r
##remove NA
df <- na.omit(df_all)
##load plyr
library(plyr)
##aggregate by date then sum all steps
df_aggr1 <- ddply(df, .(date), colwise(sum, .(steps)))
##plot histogram
hist(df_aggr1$steps, xlab='steps', ylab='frequency', main='total number of steps taken each day')
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

calculate the mean and median 

```r
df_mean <- mean(df_aggr1$steps)
df_median <- median(df_aggr1$steps)
```
**The mean and median total number of steps taken per day are 10766.19 and 10765 respectively.** 

##What is the average daily activity pattern?
First, we calculate the means steps by interval. We then make a time series plot of the interval (x-axis) and the average number of steps taken.

```r
##aggregate by interval and calculate mean steps in each interval
df_aggr2 <- ddply(df, .(interval), colwise(mean, .(steps)))
##time series plot
plot(df_aggr2$interval, df_aggr2$steps, type="l", xlab= "interval", ylab= "steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

calculate the max mean steps and find the interval with the max mean steps

```r
##find the interval with the max mean steps
max_step_interval <- df_aggr2[df_aggr2$steps == max(df_aggr2$steps), 'interval']
```
**The interval that has the maximun number of steps is 835**

##Imputing missing values
calculate total number of missing values in the dataset by subtracting number of rows of the original dataset and the number of rows of the dataset with NA's removed.

```r
na_count <- nrow(df_all) - nrow(df)
```
**The number of missing values is 2304**.

The strategy We use for imputting missing values is **mean steps of each interval**.

```r
##merge original dataset and the dataset with mean steps by interval 
df_new <- merge(df_all, df_aggr2, by='interval')
##if the value in steps.x (from original dataset) is NA, we populate the new column steps with steps.y (the mean steps value), otherwise, we use the value in steps.x
df_new$steps <- ifelse(is.na(df_new$steps.x), df_new$steps.y, df_new$steps.x)
##drop steps.x and steps.y columns
df_new <- df_new[c('steps', 'date', 'interval')]
##aggregate by date then sum all steps
df_new_aggr1 <- ddply(df_new, .(date), colwise(sum, .(steps)))
##plot histogram
hist(df_new_aggr1$steps, xlab='steps', ylab='frequency', main='total number of steps taken each day')
```

![](./PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
##calculate mean and median 
df_new_mean <- mean(df_new_aggr1$steps)
df_new_median <- median(df_new_aggr1$steps)
```
**The mean and median total number of steps taken per day are 10766.19 and 10766.19 respectively.** 

**The mean value is exactly the same as the mean value calculated when NA's are removed, while median value is slightly different. When NA's are present, we have NA's for a whole day and for all the days with NA's, thus, replacing NA's with mean interval values doesn't make any difference to the total mean value. But for median value, the median value is the same as the mean value when mean internal values are used to replace NA's.**

##Are there differences in activity patterns between weekdays and weekends?

```r
##add a new weekday column to identify weekend or weekday
df_new$weekday <-ifelse(weekdays(as.Date(df_new$date)) == 'Saturday' |
                                   weekdays(as.Date(df_new$date)) == 'Sunday',
                                  'Weekend', 'Weekday')
##aggregate by interval and weekday, and calculate the mean steps
df_new_aggr <- ddply(df_new, .(interval, weekday), colwise(mean, .(steps)))
##make weekday a factor column
df_new$weekday <- as.factor(df_new$weekday)
```


```r
library(lattice)
xyplot(steps ~ interval | weekday, df_new_aggr, type='l', layout = c(1,2), ylab='Number of steps')
```

![](./PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

**The activity patterns between weekdays and weekends appear to be different. There are more activities throughout the day during weekeneds, while on weekdays, there are more activities during early morning hours, then again during later afternoon hours.**

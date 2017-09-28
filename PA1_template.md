# Reproducible Research

## Loading and preprocessing the data

```r
library(ggplot2)

activity<-read.csv("activity.csv")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
activity$date <- as.Date(as.character(activity$date))
```

```
## Warning in strptime(xx, f <- "%Y-%m-%d", tz = "GMT"): unknown timezone
## 'default/Asia/Kolkata'
```

```r
cleanActivity <- activity[!is.na(activity$steps),]
```

##Mean total number of steps taken per day

```r
stepsPerDay <- with(activity, aggregate(steps, by = list(date), sum))
names(stepsPerDay)[1]="date"
names(stepsPerDay)[2]="totalSteps"
```
###plot




```r
ggplot(stepsPerDay, aes(x = totalSteps)) +
     geom_histogram(fill = "steelblue", binwidth=1000) +
     labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
##Mean and median of the total number of steps taken per day


```r
mean(stepsPerDay$totalSteps,na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay$totalSteps,na.rm = TRUE)
```

```
## [1] 10765
```
#Average daily activity pattern



```r
MeanDataInterval<- with(cleanActivity, aggregate(steps, by = list(interval), mean))
names(MeanDataInterval)[1]="interval"
names(MeanDataInterval)[2]="steps"
```

###plot

```r
plot(MeanDataInterval,type="l",main = "Sum of Steps by Interval",xlab = "interval", ylab = "steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

###On average across all the days in the dataset, contains the maximum number of steps



```r
maxInterval <- MeanDataInterval[which.max(MeanDataInterval$steps),]
maxInterval
```

```
##     interval    steps
## 104      835 206.1698
```
##Imputing missing values
###Total number of missing values in the dataset




```r
summary(activity$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```

###Strategy for filling in all of the missing values in the dataset.



```r
activity2<-activity
missingData <- is.na(activity2$steps)
MeanData2Interval <- aggregate(cleanActivity$steps, by=list(cleanActivity$interval), sum)
names(MeanData2Interval)[1]="interval"
names(MeanData2Interval)[2]="steps"

meanVals <- tapply(cleanActivity$steps, cleanActivity$interval, mean, na.rm=TRUE, simplify=TRUE)
activity2$steps[missingData] <- meanVals[as.character(activity2$interval[missingData])]

head(activity2)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

###Calculate and report the mean and median total number of steps taken per day.


```r
stepsPerDay2 <- with(activity2, aggregate(steps, by = list(date), sum))
names(stepsPerDay2)[1]="date"
names(stepsPerDay2)[2]="totalsteps"
```

###Histogram of the total number of steps taken each day


```r
ggplot(stepsPerDay2, aes(x = totalsteps)) +
    geom_histogram(fill="red",binwidth = 1000) + 
    labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

### Mean and median total number of steps taken per day.


```r
mean(stepsPerDay2$totalsteps)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay2$totalsteps)
```

```
## [1] 10766.19
```

The effect of using mean data per interval as a data impute method for missing values seems to push overall data towards the mean


#Differences in activity patterns between weekdays and weekends


```r
activity2$weekday <- weekdays(activity2$date)
activity2$weekend <- ifelse (activity2$weekday == "Saturday" | activity2$weekday == "Sunday", "Weekend", "Weekday")
head(activity2)
```

```
##       steps       date interval weekday weekend
## 1 1.7169811 2012-10-01        0  Monday Weekday
## 2 0.3396226 2012-10-01        5  Monday Weekday
## 3 0.1320755 2012-10-01       10  Monday Weekday
## 4 0.1509434 2012-10-01       15  Monday Weekday
## 5 0.0754717 2012-10-01       20  Monday Weekday
## 6 2.0943396 2012-10-01       25  Monday Weekday
```


```r
MeanWeeks <- with(activity2, aggregate(steps, by = list(weekend,interval),mean))
names(MeanWeeks)[1]="weekend"
names(MeanWeeks)[2]="interval"
names(MeanWeeks)[3]="steps"
```


###plot

```r
ggplot(MeanWeeks, aes(x = interval, y=steps, color=weekend)) +
       geom_line() +
       facet_grid(weekend ~ .) +
       labs(title = "Mean of Steps by Interval", x = "interval", y = "steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

There seems to be variation in the beginning of the day during weekdays, likely due to workplace activities.There seems to be an overall slightly larger incidence of steps during the weekends.


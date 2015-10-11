# Reproducible Research: Peer Assessment 1
==========================================


### Loading and Processing the data

```r
setwd("C:/Users/user/Desktop/coursera/RepResearch")
activity <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
activity$month <- as.numeric(format(activity$date, "%m"))
nNA <- na.omit(activity)
rownames(nNA) <- 1:nrow(nNA)
head(nNA)
```

```
##   steps       date interval month
## 1     0 2012-10-02        0    10
## 2     0 2012-10-02        5    10
## 3     0 2012-10-02       10    10
## 4     0 2012-10-02       15    10
## 5     0 2012-10-02       20    10
## 6     0 2012-10-02       25    10
```

```r
dim(nNA)
```

```
## [1] 15264     4
```

### The total number of steps taken

```r
tSteps <- aggregate(nNA$steps, list(Date = nNA$date), FUN = "sum")$x
tSteps
```

```r
## [1]   126 11352 12116 13294 15420 11015 12811  9900 10304 17382 12426 15098 10139
## [14] 15084 13452 10056 11829 10395  8821 13460  8918  8355  2492  6778 10119 11458
## [27]  5018  9819 15414 10600 10571 10439  8334 12883  3219 12608 10765  7336    41
## [40]  5441 14339 15110  8841  4472 12787 20427 21194 14478 11834 11162 13646 10183
## [53]  7047
```

```r
library(ggplot2)
```


### What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

* Histogram of the total number of steps taken each day

```r
ggplot(nNA, aes(date, steps)) + geom_bar(stat = "identity", colour = "black", fill = "black", width = 0.9) + facet_grid(. ~ month, scales = "free") + labs(title = "Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```

![plot of chunk-1](figure/chunk-1.png) 

* Calculate and report the mean and median total number of steps taken per day

The mean of the total number of steps taken per day is calculated as:

```r
tSteps <- aggregate(nNA$steps, list(Date = nNA$date), FUN = "sum")$x
mean(tSteps)
```

```
## [1] 10766
```
The median of the total number of steps taken per day is calculated as:

```r
median(tSteps)
```

```
## [1] 10765
```

### What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avSteps <- aggregate(nNA$steps, list(interval = as.numeric(as.character(nNA$interval))), FUN = "mean")
names(avSteps)[2] <- "meanOfSteps"

ggplot(avSteps, aes(interval, meanOfSteps)) + geom_line(color = "black", size = 0.9) + labs(title = "Time Series Plot of 5 min Interval", x = "5 min intervals", y = "Average Number of Steps Taken")
```

![plot of chunk-2](figure/chunk-2.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avSteps[avSteps$meanOfSteps == max(avSteps$meanOfSteps), ]
```

```
##     interval meanOfSteps
## 104      835       206.2
```

### Inputing missing values
* The total number of rows with NAs:


```r
sum(is.na(activity))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The strategy used was to employ the mean for that 5-minute interval to fill each NA value in the steps column.

* Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
nActivity <- activity 
for (i in 1:nrow(nActivity)) {
    if (is.na(nActivity$steps[i])) {
        nActivity$steps[i] <- avSteps[which(nActivity$interval[i] == avSteps$interval), ]$meanOfSteps
    }
}

head(nActivity)
```

```
##     steps       date interval month
## 1 1.71698 2012-10-01        0    10
## 2 0.33962 2012-10-01        5    10
## 3 0.13208 2012-10-01       10    10
## 4 0.15094 2012-10-01       15    10
## 5 0.07547 2012-10-01       20    10
## 6 2.09434 2012-10-01       25    10
```

```r
sum(is.na(nActivity))
```

```
## [1] 0
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
ggplot(nActivity, aes(date, steps)) + geom_bar(stat = "identity",
                                             colour = "black",
                                             fill = "black",
                                             width = 0.9) + facet_grid(. ~ month, scales = "free") + labs(title = "Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```

![plot of chunk-3](figure/chunk-3.png) 

* Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean total number of steps taken per day:

```r
nTSteps <- aggregate(nActivity$steps, 
                           list(Date = nActivity$date), 
                           FUN = "sum")$x
nMean <- mean(nTSteps)
nMean
```

```
## [1] 10766
```
Median total number of steps taken per day:

```r
nMedian <- median(nTSteps)
nMedian
```

```
## [1] 10766
```
Compare them with the two before imputing missing data:

```r
oMean <- mean(tSteps)
oMedian <- median(tSteps)
nMean - oMean
```

```
## [1] 0
```

```r
nMedian - oMedian
```

```
## [1] 1.189
```
So, after imputing the missing data, the new mean of total steps taken per day is the same as that of the old mean; the new median of total steps taken per day is greater than that of the old median.

### Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
head(nActivity)
```

```
##     steps       date interval month
## 1 1.71698 2012-10-01        0    10
## 2 0.33962 2012-10-01        5    10
## 3 0.13208 2012-10-01       10    10
## 4 0.15094 2012-10-01       15    10
## 5 0.07547 2012-10-01       20    10
## 6 2.09434 2012-10-01       25    10
```

```r
nActivity$weekdays <- factor(format(nActivity$date, "%A"))
levels(nActivity$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(nActivity$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(nActivity$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(nActivity$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
avSteps <- aggregate(nActivity$steps, 
                      list(interval = as.numeric(as.character(nActivity$interval)), 
                           weekdays = nActivity$weekdays),
                      FUN = "mean")
names(avSteps)[3] <- "meanOfSteps"
library(lattice)
xyplot(avSteps$meanOfSteps ~ avSteps$interval | avSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk-4](figure/chunk-4.png) 

Reproducible Research - Assessment 1
=======================================

##Loading and preprocessing the data

1.
Load the data (i.e. read.csv())

2.
Process/transform the data (if necessary) into a format suitable for your analysis


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.1
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.1
```

```r
data <- read.csv("activity.csv", header = TRUE, na.strings = "NA", sep="," ,stringsAsFactors = FALSE)
data_without_na <- na.omit(data)
```

##What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1.
Calculate the total number of steps taken per day

2.
If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
spd <- aggregate(data_without_na$steps, by=list(data_without_na$date), FUN = sum)
colnames(spd) <- c("date","steps")

hist(spd$steps, main="Total number of steps per day", xlab="steps per day", col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

3.
Calculate and report the mean and median of the total number of steps taken per day


```r
mean(spd$steps)
```

```
## [1] 10766.19
```

```r
median(spd$steps)
```

```
## [1] 10765
```

##What is the average daily activity pattern?
1.
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
sii <- aggregate(data_without_na$steps, by=list(data_without_na$interval), FUN=mean)
colnames(sii) <- c("intervals","steps")

ggplot(sii, aes(intervals,steps)) + geom_line() + labs(title="Average number of steps taken across all the days") + xlab("intervals (5 min)")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

2.
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxsteps <- subset(sii, sii$steps == max(sii$steps))
maxsteps[,1]
```

```
## [1] 835
```

##Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)



```r
length(data[is.na(data)==TRUE])
```

```
## [1] 2304
```

2.
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3.
Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data_replaced <- data
means <- mean(data_without_na$steps)
nrows <- nrow(data_replaced)

for (i in 1:nrows) {
     if (is.na(data_replaced[i,1])) {
       data_replaced[i,1] = means
     }
  
}

spd_replaced <- aggregate(data_replaced$steps, by=list(data_replaced$date), FUN = sum)
colnames(spd_replaced) <- c("date","steps")
```

4.
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
hist(spd_replaced$steps, main="Total number of steps per day", xlab="steps per day", col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
mean(spd_replaced$steps)
```

```
## [1] 10766.19
```

```r
median(spd_replaced$steps)
```

```
## [1] 10766.19
```
Because I use the mean for filling NA values, there isn't difference between this mean and the value from the first part. The median is slightly different and is equal to mean, because there are many mean values from this filling. There are many mean data, so mean and median are "stabilized".

##Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data_week <- data_replaced
data_week$date <- as.Date(data_week$date, format="%Y-%m-%d")

data_week$weekdays <- weekdays(data_week$date)

for (i in 1:nrows) {
  if (data_week[i,4] %in% c("sobota","neděle")) {
    data_week[i,4] <- "weekend"
  }else{
    data_week[i,4] <- "weekday"
  }
}

data_weekday <- subset(data_week, data_week$weekdays == "weekday")
data_weekend <- subset(data_week, data_week$weekdays == "weekend")

data_weekday_sii <- aggregate(data_weekday$steps, by=list(data_weekday$interval), FUN=mean)
colnames(data_weekday_sii) <- c("intervals","steps")
data_weekday_sii$weekdays <- "weekday"

data_weekend_sii <- aggregate(data_weekend$steps, by=list(data_weekend$interval), FUN=mean)
colnames(data_weekend_sii) <- c("intervals","steps")
data_weekend_sii$weekdays <- "weekend"

data_week_sii <- rbind(data_weekday_sii, data_weekend_sii)
```

2.
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
ggplot(data_week_sii, aes(intervals,steps)) + geom_line() + 
  labs(title="Average number of steps taken across all the days - weekdays and weekends") + 
  xlab("intervals (5 min)") + 
  facet_grid(weekdays~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 


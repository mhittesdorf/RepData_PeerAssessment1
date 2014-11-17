---
title: "Reproducible Research: Peer Assessment 1"
author: Mick Hittesdorf
date: "11/16/2014"
output: html_document
keep_md: true
---


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv")
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

## What is mean total number of steps taken per day?

```r
require(plyr)
total_steps_per_day <- ddply(data, .(date), summarise, total_steps = sum(steps))
with(total_steps_per_day, hist(total_steps, main="Total Steps Per Day"))
```

![plot of chunk histogram](figure/histogram-1.png) 

#### Calculate and report the mean and median total steps per day


```r
mean_total_steps_per_day <- sprintf("%.2f", with(total_steps_per_day, mean(total_steps, na.rm = TRUE)))
median_total_steps_per_day <- sprintf("%.2f", with(total_steps_per_day, median(total_steps, na.rm = TRUE)))
```
```
Mean total steps per day: 10766.19
Median total steps per day: 10765.00
```

## What is the average daily activity pattern?

#### Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
mean_steps_per_interval <- ddply(data, .(interval), summarise, mean_steps_per_interval = mean(steps, na.rm=TRUE))
with(mean_steps_per_interval, plot(interval, mean_steps_per_interval, type='l', main = "Mean steps per interval averaged over each day"))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_steps_per_interval_index <- with(mean_steps_per_interval, which(mean_steps_per_interval == max(mean_steps_per_interval)))
max_interval_number <- mean_steps_per_interval[max_steps_per_interval_index,1]
```

Interval with the maximum average daily number of steps: 835

## Imputing missing values

#### Calculate and report the total number of missing values in the dataset (i.e .the total number of rows with NA's)

```r
total_number_missing_values <- sum(with(data, is.na(steps)))
```
Total number of missing values (NA's): 2304

#### Create a new dataset that is equal to the original but with the missing data filled in
lookup_mean_steps_per_interval <- function(interval) {
  mean_steps_for_interval <- mean_steps_per_interval[mean_steps_per_interval$interval == interval, 2]
  return(mean_steps_for_interval)
}


```r
merged <- merge(data, mean_steps_per_interval)
ordered <- with(merged, merged[order(date),])
ordered_na_indices <- which(is.na(ordered$steps))
interval_means <- ordered[ordered_na_indices,"mean_steps_per_interval"]
na_indices <- with(data,which(is.na(steps)))
data_no_nas <- data
data_no_nas$steps[na_indices] <- ordered$mean_steps_per_interval[na_indices]
```

#### Plot a histogram again after missing data has been imputed


```r
total_steps_per_day_no_nas <- ddply(data_no_nas, .(date), summarise, total_steps = sum(steps))
with(total_steps_per_day_no_nas, hist(total_steps, main="Total Steps Per Day"))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

#### Recalculate mean and median after missing values have been imputed

```r
mean_total_steps_per_day_no_nas <- sprintf("%.2f", with(total_steps_per_day_no_nas, mean(total_steps, na.rm = TRUE)))
median_total_steps_per_day_no_nas <- sprintf("%.2f", with(total_steps_per_day_no_nas, median(total_steps, na.rm = TRUE)))
```

```
Mean total steps per day: 10766.19
Median total steps per day: 10766.19
```

#### Do these values differ from original calculations? 

Yes. The mean and median are now equal!

## Are there differences in activity patterns between weekdays and weekends?

```r
weekday_or_weekend <- function(date_factor) {
  result <- c()
  if (is.factor(date_factor)) {
    date <- as.Date(date_factor)
    days <- weekdays(date)
    weekend_days <- c("Saturday", "Sunday")
    result <- vector(mode="character", length(days))
    i = 1
    for (day in days) {
      if (day %in% weekend_days) {
        result[i] = "weekend"
      } else {
        result[i] = "weekday"
      }
      i = i + 1
    }
  }
  return(result)
}

data_no_nas_with_factors <- transform(data_no_nas, day_type = weekday_or_weekend(date))
```

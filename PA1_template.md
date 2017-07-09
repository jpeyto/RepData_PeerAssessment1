---
title: "PA1_template"
author: "jpeyto"
date: "8 July 2017"
output:
html_document:
keep_md: true
---



## Loading and preprocessing the data


```r
  if(!file.exists("activity.csv")) {
    if(!file.exists("repdata%2Fdata%2Factivity.zip")) {
      fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
      download.file(fileURL, "repdata%2Fdata%2Factivity.zip")
    }
    unzip("repdata%2Fdata%2Factivity.zip")
  }
  data<-read.csv("activity.csv")
  data<-mutate(data, date=as.POSIXct(data$date,format="%Y-%m-%d", tz="GMT"))
```

## What is mean total number of steps taken per day?

```r
  total_steps<-tapply(data$steps, data$date, sum)
  hist(total_steps)
```

![plot of chunk steps_per_day](figure/steps_per_day-1.svg)

```r
  mean(total_steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
  median(total_steps, na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
  interval_steps<-tapply(data$steps, data$interval, mean, na.rm=TRUE)
  plot(unique(data$interval), interval_steps, type="l", xaxt="n")
  axis(side=1, at = unique(data$interval)[seq(13,288,12)])
```

![plot of chunk steps_per_interval](figure/steps_per_interval-1.svg)

```r
  interval_steps[which.max(interval_steps)]
```

```
##      835 
## 206.1698
```


## Imputing missing values

```r
  sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
  #na_data<-dcast(data, interval ~ date)
  #interpolated<-na.approx(data, rule=2)
  data_continuous_interval<-mutate(data, total=seq(0,17568*5-1,5))
  steps_interpolated_by_interval<-na.approx(data_continuous_interval[,c(4,1)], rule=2)
  data_interpolated<-mutate(data, steps=steps_interpolated_by_interval[,2])
  #
  total_steps_interpolated<-tapply(data_interpolated$steps, data_interpolated$date, sum)
  #interval_steps_nona<-tapply(interpolated$steps, interpolated$interval, mean, na.rm=TRUE)
  hist(total_steps_interpolated)
```

![plot of chunk na](figure/na-1.svg)

```r
  mean(total_steps_interpolated)
```

```
## [1] 9354.23
```

```r
  median(total_steps_interpolated)
```

```
## [1] 10395
```
The mean and median have both decreased. The distribution no longer appears normally distributed.


## Are there differences in activity patterns between weekdays and weekends?

```r
  day<-sapply(data_interpolated$date, weekdays)
  weekend<-day %in% c("Saturday", "Sunday")
  day_factor<-as.factor(ifelse(weekend==TRUE,"Weekend", "Weekday"))
  data_day<-mutate(data_interpolated, day=day_factor)
  data_weekday<-filter(data_day, day==day_factor[1])
  data_weekend<-filter(data_day, day!=day_factor[1])
  weekday_interval_steps<-tapply(data_weekday$steps, data_weekday$interval, mean)
  weekend_interval_steps<-tapply(data_weekend$steps, data_weekend$interval, mean)
  weekend_weekday_steps<-cbind(unique(data$interval), weekday_interval_steps, weekend_interval_steps)
  colnames(weekend_weekday_steps)<-c("Interval","Weekday", "Weekend")
  data_melted<-melt(as.data.frame(weekend_weekday_steps), id.vars="Interval")
  p<-ggplot()+geom_line(data=data_melted, aes(x=Interval, y=value, color=variable))+facet_grid(variable~.)
  print(p)
```

![plot of chunk days](figure/days-1.svg)
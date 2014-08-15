# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
untar("./activity.zip", compressed = 'gzip')
datas <- read.csv('./activity.csv', na.strings="NA")
```
We now have to convert the "date" field to a proper 'Date' datatype

```r
datas[, "date"] <- as.Date(datas$date)
```

### removing NAs value for steps and transform date column to date value 

```r
datas_nona <- datas[!is.na(datas$steps),]
```

## What is mean total number of steps taken per day?
We use here the package PLYR, which is a tool for doing split-apply-combine procedures in a way that resambes the *SQL* command *GROUP BY*

```r
library(plyr)
daily_steps <- ddply(datas_nona, .(date), summarise, steps=sum(steps))
```
Or we can use the function *aggregate*

```r
freqs <- aggregate(datas_nona$steps, by=list(datas_nona$date), FUN=sum)
```
Simple histogram and mean and median

```r
hist(daily_steps$steps, xlab="steps per day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

```r
mean(daily_steps$steps)
```

```
## [1] 10766
```

```r
median(daily_steps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
average_date <- ddply(datas_nona, .(interval), summarise, steps=mean(steps))
plot(average_date$interval, average_date$steps, type="l", xlab="5-minute interval", 
ylab="Average steps",main="Average daily activity")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

```r
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
average_date[average_date$steps==max(average_date$steps),]
```

```
##     interval steps
## 104      835 206.2
```



## Imputing missing values

```r
colnames(average_date)[2] <- "intervalAvg"
head(average_date)
```

```
##   interval intervalAvg
## 1        0     1.71698
## 2        5     0.33962
## 3       10     0.13208
## 4       15     0.15094
## 5       20     0.07547
## 6       25     2.09434
```

```r
merged <- join(datas, average_date)
```

```
## Joining by: interval
```

```r
names(merged)
```

```
## [1] "steps"       "date"        "interval"    "intervalAvg"
```

```r
head(merged)
```

```
##   steps       date interval intervalAvg
## 1    NA 2012-10-01        0     1.71698
## 2    NA 2012-10-01        5     0.33962
## 3    NA 2012-10-01       10     0.13208
## 4    NA 2012-10-01       15     0.15094
## 5    NA 2012-10-01       20     0.07547
## 6    NA 2012-10-01       25     2.09434
```

```r
merged$steps[is.na(merged$steps)] <- merged$intervalAvg[is.na(merged$steps)]
new_daily_steps <- ddply(merged, .(date), summarise, steps=sum(steps))
hist(new_daily_steps$steps, main="Number of Steps", xlab="steps taken each day",,)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 



```r
mean(new_daily_steps$steps)
```

```
## [1] 10766
```

```r
median(new_daily_steps$steps)
```

```
## [1] 10766
```

```r
daily_steps_1 <- sum(datas_nona$steps)
daily_steps_2 <- sum(merged$steps)
diff <- daily_steps_2 -daily_steps_1 []
```
Mean values didn't change as imputation used the average on 5-mi interval

## Are there differences in activity patterns between weekdays and weekends?

```r
weekdays <- weekdays(as.Date(merged$date))
data_weekdays <- cbind(merged, day = weekdays)
data_weekdays$wk <- ifelse(data_weekdays$day %in% c("Saturday", "Sunday"),"weekend", "weekday")
average_week <- ddply(data_weekdays, .(interval, wk), summarise, steps=mean(steps))
library(lattice)

xyplot(steps ~ interval | wk, data = average_week, layout = c(1, 2), type="l")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

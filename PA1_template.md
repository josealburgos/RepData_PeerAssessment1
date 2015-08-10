# Reproducible Research: Peer Assessment 1
This documents presents the number of steps taken per day, the dayly activity pattern, with and without missing values.  In the second part, missing values are filled using the mean of similar observations.

## Loading and preprocessing the data
The following code loads the data in a dataframe dat

```r
setwd("~/github/repdata_peerassessment1")
dat <- read.csv(unzip("activity.zip", "activity.csv"), header = T, sep = ",")
head(dat)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?
The following graph shows the total steps taken per day in a histogram. The red vertical bar represents the daily average.  The median is very close to the average.

```r
#total steps taken per day
library(dplyr)
dat <- tbl_df(dat)
dat1 <- dat %>%
        group_by(date)%>%
        filter(!is.na(steps))%>%
        summarize(steps = sum(steps))
           
with(dat1, hist(steps, main = "Steps taken per day", xlab = "Steps"))
abline(v = mean(dat1$steps), col = "red", lwd = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
summary <- function(x) {c(average = mean(x), median = median(x))}
summary(dat1$steps)
```

```
##  average   median 
## 10766.19 10765.00
```

## What is the average daily activity pattern?
The following graph shows the daily activity pattern.  At the highest, on average in a five minute interval, the individual records over 200 steps.  This happens in the morning around nine.

```r
dat2 <- dat %>%
        group_by(interval)%>%
        filter(!is.na(steps))%>%
        summarize(steps = mean(steps))

with(dat2, plot(x=interval, y = steps, type ="l", main = "Average Daily Activity Pattern"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
#maximun is over 200 steps per interval
dat2[dat2$steps >200,]
```

```
## Source: local data frame [1 x 2]
## 
##   interval    steps
## 1      835 206.1698
```
## Imputing missing values
In this part, missing values are calculated.  In the second part, NAs are replaced by the mean for every interval.

```r
dat <- read.csv(unzip("activity.zip", "activity.csv"), header = T, sep = ",")
#total NAs
sum(is.na(dat$steps))
```

```
## [1] 2304
```


```r
dat3 <- dat
int <- unique(dat3$interval)
for (i in seq_along(int)){
        dat3[is.na(dat3$steps) & dat3$interval == int[i], 
         "steps"] <- mean(dat3[dat3$interval == int[i], 1],  na.rm = T)   
}
```
The following histogram presents the number of steps taken by day

```r
dat3 <- tbl_df(dat3)
dat4 <- dat3 %>%
        group_by(date)%>%
        summarize(steps = sum(steps))
           
with(dat4, hist(steps, main = "Steps taken per day, NA filled", xlab = "Steps"))
abline(v = mean(dat4$steps), col = "red", lwd = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
summary <- function(x) {c(average = mean(x), median = median(x))}
summary(dat4$steps)
```

```
##  average   median 
## 10766.19 10766.19
```
The mean and median when missing values are filled with the mean for every interval is almost the same.

## Are there differences in activity patterns between weekdays and weekends?
This plot shows the differences between activity in weekdays and weekends.

```r
#modifying dat3, creating a varible for weekday or weekend
dat3$date <- as.Date(dat3$date)
dat3$day <- ifelse(weekdays(dat3$date) == "Saturday" | weekdays(dat3$date) ==           "Sunday","weekend","weekday")
#average steps taken by interval for weekdays or weekends
dat3 <- dat3 %>%
        group_by(interval, day)%>%
        summarize(steps = mean(steps))
#graphics between weekdays and weekends by interval
library(ggplot2)
g <- ggplot(dat3, aes(interval, steps)) + geom_line()
g + ggtitle("Average Steps by Interval") + facet_grid(day~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

There is a spike in activity for both around 830.  There is on average more activity (numer of steps) during the weekend.

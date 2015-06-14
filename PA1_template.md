# Reproducible Research: Peer Assessment 1

This report will investigate patterns in activity of an anonymous individual through a sequence of days. To do this we'll use measurements present in the `activity.zip` file on this repository. We choose to use the `ggplot2` library to construct the plots. So, the first step is configure system and load libraries. Note that set locale to US is necessary since we'll work with dates using North American format. We'll also create a new folder `figure` to export plots generated.


```r
Sys.setlocale("LC_ALL", "us")
library(ggplot2)
if (!file.exists("figure")) {
    dir.create("figure")
}
```

## Loading and preprocessing the data

### Loading data

We'll load the data into a data frame called `activity`. To do it we will open
and read the csv inside the zip file and look inside its structure:


```r
activity <- read.csv(unz("activity.zip", "activity.csv"))
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

### Preprocessing data

We can see above that `date` is a factor variable. 
To allow date calculations we need to transform `date` into date format:


```r
activity$date <- as.Date(activity$date)
activity$weekday <- weekdays(activity$date, abbreviate = TRUE)
```


## What is mean total number of steps taken per day?

To answer this question we'll use aggregate data.
Let's create a new data frame `by_date` that aggregates the sum of steps by each day: 


```r
by_date <- aggregate(steps ~ date, data = activity, FUN = sum, na.action = na.pass)
```

The histogram below shows that the frequency of total steps by day resembles a normal distribution:


```r
qplot(steps, data = by_date, geom = "histogram")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
ggsave("figure/plot1.png", dpi = 100)
```

```
## Saving 7 x 5 in image
```

And we can see that **mean** and **median** values by day are very close:


```r
mean(by_date$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(by_date$steps, na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

To analyse the average daily activity by intervals of measurement we'll create
the data frame `by_interval`, that aggregates the mean of steps by each 5-minute interval:


```r
by_interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
qplot(interval, steps, data = by_interval, geom = "line")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
ggsave("figure/plot2.png", dpi = 100)
```

```
## Saving 7 x 5 in image
```

The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps can be obtained using `which.max` function:


```r
by_interval[which.max(by_interval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

The interval with the max average steps is **835**.

## Imputing missing values

Note that there are **2304** measurements missing,
and some days with total absence of measurements (there are 288 5-minute intervals per day):


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
names(table(subset(activity, is.na(steps))$date))
```

```
## [1] "2012-10-01" "2012-10-08" "2012-11-01" "2012-11-04" "2012-11-09"
## [6] "2012-11-10" "2012-11-14" "2012-11-30"
```

To fill each of missing values, we will use the mean value of measurements that:

1. Have the same weekday of missing measurement AND;
2. Have the same 5-minute interval of missing measurement.

We will create a new dataset `filled`, with filled values according the strategy chosen: 


```r
filled <- activity

for (i in 1:nrow(activity)) {
    if (is.na(activity[i, 1])) {
        sub_similar <- subset(activity, interval == activity[i,3] & weekday == activity[i,4])
        filled[i, 1] <- round(mean(sub_similar$steps, na.rm = TRUE)) 
    }
} 
```

Now we need aggregate by day the measurements of the filled dataset.
Let's create a new dataset `by_date_filled`:


```r
by_date_filled <- aggregate(steps ~ date, data = filled, FUN = sum)
```

Let's look at this dataset to compare with previous dataset without filled entries:


```r
qplot(steps, data = by_date_filled, geom = "histogram")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

```r
ggsave("figure/plot3.png", dpi = 100)
```

```
## Saving 7 x 5 in image
```

```r
mean(by_date_filled$steps)
```

```
## [1] 10821.1
```

```r
median(by_date_filled$steps)
```

```
## [1] 11015
```

We can see the results are slightly higher using this method, indicating that putting
missing data contribute to elevate the mean and median.


```r
mean(by_date_filled$steps) - mean(by_date$steps, na.rm = TRUE)
```

```
## [1] 54.90968
```

```r
median(by_date_filled$steps) - median(by_date$steps, na.rm = TRUE)
```

```
## [1] 250
```

## Are there differences in activity patterns between weekdays and weekends?

To do this we'll create a new variable called `is_weekend`:


```r
weekend <- c("Sat", "Sun")
filled$is_weekend <- is.element(filled$weekday, weekend)
filled$is_weekend <- factor(filled$is_weekend, levels = c(FALSE, TRUE), labels = c("weekday", "weekend"))
by_interval_filled <- aggregate(steps ~ interval + is_weekend, filled, FUN = mean)
```

Let's plot some comparison between average activity in weekdays and weekend:


```r
qplot(interval, steps, data = by_interval_filled, facets = is_weekend ~., geom = "line")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png) 

```r
ggsave("figure/plot4.png", dpi = 100)
```

```
## Saving 7 x 5 in image
```

The graph shows that on weekends the person's activities starts later, and are more well distributed through the day.

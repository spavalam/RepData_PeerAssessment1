# Reproducible Research: Peer Assessment 1


This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document.

# Reproducible Research: Peer Assessment 1

# Define some options for knitr


```r
knitr::opts_chunk$set(tidy=FALSE, fig.path='figures/')
```

## Loading and preprocessing the ACTIVITY data which is in the csv file


```r
data <- read.csv("activity.csv")
```

## Computing the MEAN total number of steps taken per day


```r
library(ggplot2)
totalnumber.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(totalnumber.steps, binwidth=750, xlab="Total Number of steps taken each day")
```

![](figures/unnamed-chunk-2-1.png) 

```r
mean(totalnumber.steps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(totalnumber.steps, na.rm=TRUE)
```

```
## [1] 10395
```

## Computing the AVERAGE daily activity pattern


```r
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("Five Minute Interval") +
    ylab("AVERAGE Number of Steps taken")
```

![](figures/unnamed-chunk-3-1.png) 

On average across all the days in the dataset, the Five minute interval contains
the maximum number of steps is indicated below:


```r
averages[which.max(averages$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## IMPUTING Missing values

There are many days/intervals where there are missing values which are coded as `NA`. The presence of missing days may introduce ambiguties in data.

Table Containing Missing Values is depicted below:


```r
missing <- is.na(data$steps)
table(missing)
```

```
## missing
## FALSE  TRUE 
## 15264  2304
```

All the above missing values are filled in with mean value for that FIVE Minute interval.


```r
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}

filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
```
Now, using the filled data set, let's make a histogram of the total number of steps taken each day and calculate the mean and median total number of steps.


```r
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(totalnumber.steps, binwidth=750, xlab="Total Number Of Steps taken each day")
```

![](figures/unnamed-chunk-7-1.png) 

```r
mean(totalnumber.steps)
```

```
## [1] 9354.23
```

```r
median(totalnumber.steps)
```

```
## [1] 10395
```

Mean and median values are higher after imputing missing data. The reason is
that in the original data, there are some days with `steps` values `NA` for 
any `interval`. The total number of steps taken in such days are set to 0s by
default. However, after replacing missing `steps` values with the mean `steps`
of associated `interval` value, these 0 values are replaced with mean steps.

## Are there differences in activity patterns between weekdays and weekends?

As a first step, infer the day of the week for each measurement in the dataset. In
this part, we use the dataset with the filled-in values depending upon the day.


```r
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}

filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)
```

Creating a Panel Plot containing plots of average number of steps taken during Weekdays and Weekends.


```r
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("Five Minute Interval") + ylab("Number Of Steps")
```

![](figures/unnamed-chunk-9-1.png) 

There are diffferences in activity patterns between weekdays and weekends observed in the above plot. During the 500 to 1000 interval, during weekdays the number of steps is more. During the 1000 to 2000 interval the number of steps covered is more during weekend.


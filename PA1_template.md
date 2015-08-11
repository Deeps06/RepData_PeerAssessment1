# Reproducible Research: Assignment 1

## Loading and pre-processing the data
```{r loaddata}
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

The total number of steps taken per day
```{r}
library(ggplot2)
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="Total number of steps taken per day")
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)

  ```
  ![Plot of chunk- Assignment Plot 1.png](Rep data/Assignment Plot 1.png)
  
  ```

## What is the average daily activity pattern?

Time series plot of the 5-minute interval and average number of steps taken averaged across all days
```{r}
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute Interval") +
  ylab("Average number of steps taken")
```
```
![Plot of chunk- Assignment Plot 2.png](Rep data/Assignment Plot 2.png)

```
The 5-minute interval contains the maximum number of steps

```{r}
averages[which.max(averages$steps),]
```

## Imputing missing values

The presence of missing days may introduce bias into some calculations or summaries of the data.

The strategy for filling in all of the missing values in the dataset is to use mean of the day.

```{r how_many_missing}
missing <- is.na(data$steps)
# How many missing
table(missing)
```

All of the missing values are filled in with mean value for that 5-minute
interval.

```{r}
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

#Histogram of the total number of steps taken each day
```{r}
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="Total number of steps taken per day")
```
![Plot of chunk- Assignment Plot 3.png](Rep data/Assignment Plot 3.png)

```

#Mean of total number of steps taken per day
mean(total.steps)

#Median of total number of steps taken per day
median(total.steps)
```

Mean and median values are higher after imputing missing data. The reason is
that in the original data, there are some days with `steps` values `NA` for 
any `interval`. The total number of steps taken in such days are set to 0s by
default. However, after replacing missing `steps` values with the mean `steps`
of associated `interval` value, these 0 values are removed from the histogram
of total number of steps taken each day.

## Are there differences in activity patterns between weekdays and weekends?
First, let's find the day of the week for each measurement in the dataset. In
this part, we use the dataset with the filled-in values.

```{r}
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

Now, let's make a panel plot containing plots of average number of steps taken
on weekdays and weekends.
```{r}
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
xlab("5-minute interval") + ylab("Number of steps")
```
```
![Plot of chunk- Assignment Plot 4.png](Rep data/Assignment Plot 4.png)

```
# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
act <-
	read.csv("activity.csv", colClasses = c("integer", "Date", "integer"))
```

## What is mean total number of steps taken per day?
Let's first plot the average steps per day in a histogram

```r
p1 <- aggregate(steps ~ date, act, sum, na.rm = TRUE)
par(mfrow = c(1, 1))
with(p1, plot(date, steps, type = "h", xlab = "Days", ylab = "Step average", col = "navy"))
```

![Graph 1. Average steps per day](PA1_template_files/figure-html/unnamed-chunk-2-1.png)

And then calculate mean and median per day

```r
tmp.mean <- aggregate(steps ~ date, act, mean, na.rm = TRUE)
tmp.median <- aggregate(steps ~ date, act, median, na.rm = TRUE)

p1$mean <- cbind(tmp.mean$steps)
p1$median <- cbind(tmp.median$steps)

library(knitr)
kable(p1, caption = "Table 1. Step average and median per day")
```



Table: Table 1. Step average and median per day

date          steps         mean   median
-----------  ------  -----------  -------
2012-10-02      126    0.4375000        0
2012-10-03    11352   39.4166667        0
2012-10-04    12116   42.0694444        0
2012-10-05    13294   46.1597222        0
2012-10-06    15420   53.5416667        0
2012-10-07    11015   38.2465278        0
2012-10-09    12811   44.4826389        0
2012-10-10     9900   34.3750000        0
2012-10-11    10304   35.7777778        0
2012-10-12    17382   60.3541667        0
2012-10-13    12426   43.1458333        0
2012-10-14    15098   52.4236111        0
2012-10-15    10139   35.2048611        0
2012-10-16    15084   52.3750000        0
2012-10-17    13452   46.7083333        0
2012-10-18    10056   34.9166667        0
2012-10-19    11829   41.0729167        0
2012-10-20    10395   36.0937500        0
2012-10-21     8821   30.6284722        0
2012-10-22    13460   46.7361111        0
2012-10-23     8918   30.9652778        0
2012-10-24     8355   29.0104167        0
2012-10-25     2492    8.6527778        0
2012-10-26     6778   23.5347222        0
2012-10-27    10119   35.1354167        0
2012-10-28    11458   39.7847222        0
2012-10-29     5018   17.4236111        0
2012-10-30     9819   34.0937500        0
2012-10-31    15414   53.5208333        0
2012-11-02    10600   36.8055556        0
2012-11-03    10571   36.7048611        0
2012-11-05    10439   36.2465278        0
2012-11-06     8334   28.9375000        0
2012-11-07    12883   44.7326389        0
2012-11-08     3219   11.1770833        0
2012-11-11    12608   43.7777778        0
2012-11-12    10765   37.3784722        0
2012-11-13     7336   25.4722222        0
2012-11-15       41    0.1423611        0
2012-11-16     5441   18.8923611        0
2012-11-17    14339   49.7881944        0
2012-11-18    15110   52.4652778        0
2012-11-19     8841   30.6979167        0
2012-11-20     4472   15.5277778        0
2012-11-21    12787   44.3993056        0
2012-11-22    20427   70.9270833        0
2012-11-23    21194   73.5902778        0
2012-11-24    14478   50.2708333        0
2012-11-25    11834   41.0902778        0
2012-11-26    11162   38.7569444        0
2012-11-27    13646   47.3819444        0
2012-11-28    10183   35.3576389        0
2012-11-29     7047   24.4687500        0


## What is the average daily activity pattern?
Let's plot a time-series between average steps from the entire dataset and the period of the day.

```r
p2 <- aggregate(steps ~ interval, act, mean, na.rm = TRUE)
names(p2) <- c("interval", "steps.mean")
par(mfrow = c(1, 1))
with(p2, plot(interval, steps.mean, type = "l", col = "navy"))
```

![Graph 2. Average step counting per daily period](PA1_template_files/figure-html/unnamed-chunk-4-1.png)

From this plot it is possible to notice some activity early in the morning from 5am, a peak around 8:30am and some variation until 7pm, when the activity finally disminishes as a pattern.


```r
# Get peak coordinates
max.steps.y <- max(p2$steps.mean)
max.steps.x <- p2$interval[which(p2$steps.mean == max.steps.y)]
peak.int <- paste(as.character(max.steps.x %/% 100), ":", as.character(max.steps.x %% 100))
```


The peak is noticed at **8 : 35** with an average of **206.2** steps.

## Imputing missing values

```r
# Get number of missing entries
missing <- sum(is.na(act$steps))
miss.pct <- round(sum(is.na(act$steps)) / length(act$steps), 2) * 100
```

There is a total of 2304 missing entries, which represents 13% of the total observations. As this may lead to biased calculations, let's impute some values in the missing step count. We will use daily period step average to fill those gaps.


```r
# Impute implementation for missing step values
# Merge original dataset with the average step per period of the day - these will be the default values for the gaps
act2 <- merge(act, p2, by = "interval", sort = FALSE)
# Order the temporary dataset by date (merge by interval unsorted the dataset, which could cause problems in the replacements)
act2 <- act2[order(act2$date),]
# Creates a list of the nee steps column in te final dataset, keeping the existing values and replacing the missing ones by its corresponding period's step average
steps.new <-	replace(act2$steps, which(is.na(act2$steps)), act2$steps.mean[which(is.na(act2$steps))])
# Creates the new final dataset without the step.mean column
act.new <- act2[,1:3]
# Replaces steps column with the replacement list
act.new$steps <- steps.new
```

```r
# Plots a new histogram with the gaps filled
p3 <- aggregate(steps ~ date, act.new, sum)
par(mfrow = c(1, 1))
with(p3, plot(date, steps, type = "h", xlab = "Days", ylab = "Step average", col = "navy"))
```

![Graph 3. Average steps per day with the missing step counting replaced by daily period average](PA1_template_files/figure-html/unnamed-chunk-8-1.png)

Comparing with the first graph, it is possible to notice less blank spaces between the hist bars, and the outliers become more visible (step counting in Oct 2nd and Nov 15th).


```r
# Calculate mean and median for the new dataset
tmp.mean <- aggregate(steps ~ date, act.new, mean)
tmp.median <- aggregate(steps ~ date, act.new, median)

p3$act.mean <- cbind(tmp.mean$steps)
p3$act.median <- cbind(tmp.median$steps)

library(knitr)
kable(p3, caption = "Table 2. Step average and median per day with the missing step counting replaced by daily period average")
```



Table: Table 2. Step average and median per day with the missing step counting replaced by daily period average

date             steps     act.mean   act.median
-----------  ---------  -----------  -----------
2012-10-01    10766.19   37.3825996     34.11321
2012-10-02      126.00    0.4375000      0.00000
2012-10-03    11352.00   39.4166667      0.00000
2012-10-04    12116.00   42.0694444      0.00000
2012-10-05    13294.00   46.1597222      0.00000
2012-10-06    15420.00   53.5416667      0.00000
2012-10-07    11015.00   38.2465278      0.00000
2012-10-08    10766.19   37.3825996     34.11321
2012-10-09    12811.00   44.4826389      0.00000
2012-10-10     9900.00   34.3750000      0.00000
2012-10-11    10304.00   35.7777778      0.00000
2012-10-12    17382.00   60.3541667      0.00000
2012-10-13    12426.00   43.1458333      0.00000
2012-10-14    15098.00   52.4236111      0.00000
2012-10-15    10139.00   35.2048611      0.00000
2012-10-16    15084.00   52.3750000      0.00000
2012-10-17    13452.00   46.7083333      0.00000
2012-10-18    10056.00   34.9166667      0.00000
2012-10-19    11829.00   41.0729167      0.00000
2012-10-20    10395.00   36.0937500      0.00000
2012-10-21     8821.00   30.6284722      0.00000
2012-10-22    13460.00   46.7361111      0.00000
2012-10-23     8918.00   30.9652778      0.00000
2012-10-24     8355.00   29.0104167      0.00000
2012-10-25     2492.00    8.6527778      0.00000
2012-10-26     6778.00   23.5347222      0.00000
2012-10-27    10119.00   35.1354167      0.00000
2012-10-28    11458.00   39.7847222      0.00000
2012-10-29     5018.00   17.4236111      0.00000
2012-10-30     9819.00   34.0937500      0.00000
2012-10-31    15414.00   53.5208333      0.00000
2012-11-01    10766.19   37.3825996     34.11321
2012-11-02    10600.00   36.8055556      0.00000
2012-11-03    10571.00   36.7048611      0.00000
2012-11-04    10766.19   37.3825996     34.11321
2012-11-05    10439.00   36.2465278      0.00000
2012-11-06     8334.00   28.9375000      0.00000
2012-11-07    12883.00   44.7326389      0.00000
2012-11-08     3219.00   11.1770833      0.00000
2012-11-09    10766.19   37.3825996     34.11321
2012-11-10    10766.19   37.3825996     34.11321
2012-11-11    12608.00   43.7777778      0.00000
2012-11-12    10765.00   37.3784722      0.00000
2012-11-13     7336.00   25.4722222      0.00000
2012-11-14    10766.19   37.3825996     34.11321
2012-11-15       41.00    0.1423611      0.00000
2012-11-16     5441.00   18.8923611      0.00000
2012-11-17    14339.00   49.7881944      0.00000
2012-11-18    15110.00   52.4652778      0.00000
2012-11-19     8841.00   30.6979167      0.00000
2012-11-20     4472.00   15.5277778      0.00000
2012-11-21    12787.00   44.3993056      0.00000
2012-11-22    20427.00   70.9270833      0.00000
2012-11-23    21194.00   73.5902778      0.00000
2012-11-24    14478.00   50.2708333      0.00000
2012-11-25    11834.00   41.0902778      0.00000
2012-11-26    11162.00   38.7569444      0.00000
2012-11-27    13646.00   47.3819444      0.00000
2012-11-28    10183.00   35.3576389      0.00000
2012-11-29     7047.00   24.4687500      0.00000
2012-11-30    10766.19   37.3825996     34.11321

It is possible to notice that median values now appear more frequently.

## Are there differences in activity patterns between weekdays and weekends?

To answer this question, we will plot 2 time series like in the Graph 2, but splitting the data by weekday/weekend factor.


```r
# Create a new variable in the dataset to store a factor variable that determines either the observation was taken in a weekday or weekend
act.new$wday <- as.POSIXlt(act.new$date)$wday
# Convention: wknd flag variable receives 1 for weekdays and 2 for weekends
wday.new <-	ifelse(act.new$wday < 6, 1, 2)
act.new$wknd <- as.factor(wday.new)
```

```r
par(mfrow = c(2, 1), mar = c(4,4,2,1))

# Weekdays
p4.1 <- aggregate(steps ~ interval, subset(act.new, act.new$wday == 1), mean, na.rm = TRUE)
names(p4.1) <- c("interval", "steps.mean")
with(p4.1, plot(interval, steps.mean, type = "l", col = "navy"))
title(main = "Weekdays")

p4.2 <- aggregate(steps ~ interval, subset(act.new, act.new$wday == 2), mean, na.rm = TRUE)
names(p4.2) <- c("interval", "steps.mean")
with(p4.2, plot(interval, steps.mean, type = "l", col = "navy"))
title(main = "Weekends")
```

![Graph 4. Activity patterns in weekdays and weekends](PA1_template_files/figure-html/unnamed-chunk-11-1.png)

According to the graph patterns whereas in weekdays there are specific periods of more intense activity, on weekends there are more peaks during the day. Here we list some findings:

*Weekdays*

* intense activity in the early, but few steps - short distances
* commute around 8am and 6-7pm
* lunch around noon
* some minor peaks after 8pm, again short distances

*Weekends*

* a period of constant activity in the early morning, probably a walk or jogging
* a similar "commuting" behavior that could lead to determine that this person works on Saturdays
* several intermediate peaks denote non-routine activities during weekends
* some (little) activity during 0 to 5am not noticed in the Graph 2.

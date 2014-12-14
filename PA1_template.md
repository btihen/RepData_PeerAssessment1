# Reproducible Research: Peer Assessment 1

Add the libraries needed for this project (supressing warnings and messages):


```r
suppressWarnings(suppressMessages( library(knitr) ))
suppressWarnings(suppressMessages( library(lattice) ))
suppressWarnings(suppressMessages( library(data.table) ))
```

## Loading and preprocessing the data

1. **Load the data (i.e. `read.csv()`)**

* download the data if it is necessary (missing)
* create a data directory
* unpack (unzip the data -- into the data directory)
* load the data

Done with the following code:


```r
# file already included in assignment -- next line show how to get the data if needed
# download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "activity-data.zip", methodi="curl")
# create a data directory & unzip the data
data_path = "data"
dir.create( data_path, showWarnings = FALSE )
unzip("activity.zip", exdir="data", overwrite=TRUE)
data_file = list.files(path = data_path)[1]
path_file = file.path(data_path, data_file)
# READ the data into R
#      missing data is marked as NA (ensure the data is suitable for processing)
#      the second column is a date format (ensure data is suitable for processing)
#activity_df = read.table(path_file, header=TRUE, sep=",", na.strings="NA", row.names=NULL)
#activity_df[[2]] <- as.Date(activity_df[[2]])
activity_df = read.table(path_file, header=TRUE, sep=",", na.strings="NA", colClasses=c(NA,"Date",NA),row.names=NULL)
```


2. **Process/transform the data (if necessary) into a format suitable for your analysis**

This is done on loading the data with the appropriate switches:
* nsure the data is ready for usage by:
* na.strings="NA" -- text marked as NA is read as missing data
* colClasses=c(NA,"Date",NA) -- the second column is recognized as date values and other are the default values

The code that accomplishes this is the read.table command:


```r
activity_df = read.table(path_file, header=TRUE, sep=",", na.strings="NA", colClasses=c(NA,"Date",NA),row.names=NULL)
# equivalent to:
activity_df = read.table(path_file, header=TRUE, sep=",", na.strings="NA", row.names=NULL)
activity_df[[2]] <- as.Date(activity_df[[2]])
```

## What is mean total number of steps taken per day?

1. **Make a histogram of the total number of steps taken each day**


```r
# AVERAGE STEPS / DAY
activity_dt = data.table(activity_df)
steps_each_day = activity_dt[,sum(steps),by=date]
setnames(steps_each_day, "V1", "daily_step_total")
barplot(steps_each_day$daily_step_total, xlab="date", ylab="steps", main="Total Steps taken each Day (Ingoring missing Observations)")
```

![](./PA1_template_files/figure-html/imputed_steps_each_day_plot-1.png) 


2. **Calculate and report the *mean* and *median* total number of steps taken per day**

The mean steps taken per day are: 

```r
mean_steps_per_day = mean( steps_each_day$daily_step_total, na.rm=TRUE )
median_steps_per_day = median( steps_each_day$daily_step_total, na.rm=TRUE )
```

Calculation | Results with missing data
------------|---------------------------
*Mean*      | 10766.2   
*Median*    | 10765 



## What is the average daily activity pattern?

1. **Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**


```r
# calculate interval averages over each day
interval_steps_mean_dt = activity_dt[,mean(steps, na.rm=TRUE), by=interval]
setnames(interval_steps_mean_dt, "V1", "average_inverval_steps")
# graph interval averages as time series plot
plot( interval_steps_mean_dt, type="l", xlab="5-minute intervals", ylab="average steps", main="Mean Steps per Interval over all Days  (ignoring MISSING Observations)" )
```

![](./PA1_template_files/figure-html/interval_activity_plot-1.png) 


2. **Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**


```r
max_interval_steps = interval_steps_mean_dt[,max(average_inverval_steps, na.rm=TRUE)]
max_steps_location = which( interval_steps_mean_dt$average_inverval_steps == max_interval_steps)
max_steps_interval = interval_steps_mean_dt$interval[[max_steps_location]]
```

The **interval with the maximum steps** is: **835**

## Imputing missing values

1. **Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)**


```r
missing_observations = sum(is.na(activity_dt$steps))
total_observations = nrow(activity_dt)
missing_percent = missing_observations / total_observations * 100
```

Description | Count
------------|------
**Missing Observations** | 2304
Total Observations   | 17568

Percentage of missing observations: **13.1%**


2. **Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated.**

The strategy used is to will fill-in missing values with the median intervals value over all the days of the study.  This is probably a reasonable way to impute missing values, since many people have a daily rythm of sleep and activity.  A complicating factor is of course weekends when many people change their activities, but since a simple strategy can be used, this compounding factor will be ignored for now.

3. **Create a new dataset that is equal to the original dataset but with the missing data filled in.**


```r
# calculate each interval's median steps
interval_steps_median_dt = activity_dt[,median(steps, na.rm=TRUE), by=interval]
setnames(interval_steps_median_dt, "V1", "median_inverval_steps")

# build a copy for imputed values
activity_dt <- merge(activity_dt, interval_steps_median_dt, by="interval", all=FALSE)

# mark observations with missing data
activity_dt$na_data <- is.na(activity_dt$steps)

# copy the steps to create an imputed steps field
activity_dt$imputed_steps <- activity_dt$steps

# in all locations where there is missing values replace the missing value in imputed_steps
# with the median value for that time interval
activity_dt[na_data==TRUE, imputed_steps := median_inverval_steps]

# create a new data set just for imputed values
imputed_data_set <- data.frame( activity_dt$date, activity_dt$interval, activity_dt$imputed_steps )
# fix the column names
setnames( imputed_data_set, "activity_dt.date", "date" )
setnames( imputed_data_set, "activity_dt.interval", "interval" )
setnames( imputed_data_set, "activity_dt.imputed_steps", "steps" )
```

resulting is a **new data set with imputed values:**


```r
#kable(head(activity_dt[]), format = "markdown")
kable(head(imputed_data_set[]), format = "markdown")
```



|date       | interval| steps|
|:----------|--------:|-----:|
|2012-10-01 |        0|     0|
|2012-10-02 |        0|     0|
|2012-10-03 |        0|     0|
|2012-10-04 |        0|    47|
|2012-10-05 |        0|     0|
|2012-10-06 |        0|     0|

4. **Make a histogram of the total number of steps taken each day and Calculate and report the *mean* and *median* total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**

* **HISTOGRAM of imputed data**


```r
imputed_steps_each_day = activity_dt[,sum(imputed_steps),by=date]
setnames(imputed_steps_each_day, "V1", "daily_step_total")
barplot(imputed_steps_each_day$daily_step_total, xlab="date", ylab="steps", main="Total Steps taken each Day (with Imputed Observations)")
```

![](./PA1_template_files/figure-html/imputed_histogram-1.png) 


* **CALCULATE -- mean and median**


```r
imputed_steps_each_day = activity_dt[,sum(imputed_steps),by=date]
setnames(imputed_steps_each_day, "V1", "daily_step_total")
imputed_mean_steps_per_day = mean( imputed_steps_each_day$daily_step_total )
imputed_median_steps_per_day = median( imputed_steps_each_day$daily_step_total )
```

Calculation | Results with imputed data
------------|---------------------------
*Mean*      | 9503.9   
*Median*    | 10395 

* **COMPARE -- daily steps mean and median using missing values and imputed values**

Data Used | Mean | Median | Distance between median and mean
----------|------|--------|---------------------------------
**Calulations with Imputed Missing Steps** | 9503.9 | 10395 | **891.1**
**Calculations when Ignoring Missing Steps** | 10766.2 | 10765 | **1.2** 
**Difference between imputed and missing data calculations** | **1262.3** | **370** | 

* **Do the result differ? If so what is the impact on steps per day?**

Ingoring missing values and imputing missing values produces different results. Differences include:
* imputed values produce fewer average steps per interval (indicating possibly inflated results when ignoring missing values).
* the mean and median have a wider split with imputed data (implying that the data is no longer randomly distributed -- with missing values the mean and median were essentially the same)

Ideally the distribution of the original data should be investigated before and after imputing to best understand the data and the effect of the analysis method.


## Are there differences in activity patterns between weekdays and weekends?

1. **Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.**



```r
activity_dt$day_of_week <- as.POSIXlt(activity_dt$date)$wday
activity_dt$day_name <- as.factor( c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")[as.POSIXlt(activity_dt$date)$wday + 1] )
activity_dt$day_type <- as.factor( c("Weekend", "Weekday", "Weekday", "Weekday", "Weekday", "Weekday", "Weekend")[as.POSIXlt(activity_dt$date)$wday + 1] )
kable(head(activity_dt[]), format = "markdown")
```



| interval| steps|date       | median_inverval_steps|na_data | imputed_steps| day_of_week|day_name  |day_type |
|--------:|-----:|:----------|---------------------:|:-------|-------------:|-----------:|:---------|:--------|
|        0|    NA|2012-10-01 |                     0| TRUE   |             0|           1|Monday    |Weekday  |
|        0|     0|2012-10-02 |                     0|FALSE   |             0|           2|Tuesday   |Weekday  |
|        0|     0|2012-10-03 |                     0|FALSE   |             0|           3|Wednesday |Weekday  |
|        0|    47|2012-10-04 |                     0|FALSE   |            47|           4|Thursday  |Weekday  |
|        0|     0|2012-10-05 |                     0|FALSE   |             0|           5|Friday    |Weekday  |
|        0|     0|2012-10-06 |                     0|FALSE   |             0|           6|Saturday  |Weekend  |


2. **Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).**


```r
day_type_steps_each_interval = activity_dt[ ,sum(imputed_steps), by=list(interval, day_type) ]
setnames(day_type_steps_each_interval, "V1", "mean_imputed_interval_steps")
xyplot( mean_imputed_interval_steps ~ interval | day_type, 
        type = "l",
        data = day_type_steps_each_interval,
        main="Steps per Inteval averaged over All days (using imputed data)", 
        ylab="Average Steps", 
        xlab="5-Minute Interval",
        layout=c(1,2) )
```

![](./PA1_template_files/figure-html/weekend_weekday_compare_plot-1.png) 

This graph shows that there are different activity levels on the weekends and weekdays.  Ideally the imputation method could be improved to take this into account too, since one of the imputation assumptions was that the activity level at a specific time of day would be similar each day -- this is clearly not true.

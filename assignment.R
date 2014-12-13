# GETTING / UNPACKING / READING DATA
# file already included in assignment -- next line show how to get the data if needed
# download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "activity-data.zip", methodi="curl")
data_path = "data"
dir.create( data_path )
unzip("activity.zip", exdir="data", overwrite=TRUE)
data_file = list.files(path = data_path)[1]
path_file = file.path(data_path, data_file)
#activity_df = read.table(path_file, header=TRUE, sep=",", na.strings="NA", row.names=NULL)
#activity_df[[2]] <- as.Date(activity_df[[2]])
activity_df = read.table(path_file, header=TRUE, sep=",", na.strings="NA", colClasses=c(NA,"Date",NA),row.names=NULL)

## AVERAGE STEPS / DAY
library(data.table)
activity_dt = data.table(activity_df)
steps_each_day = activity_dt[,sum(steps),by=date]
setnames(steps_each_day, "V1", "daily_step_total")
mean_steps_per_day = mean( steps_each_day$daily_step_total, na.rm=TRUE )
median_steps_per_day = median( steps_each_day$daily_step_total, na.rm=TRUE )

# 5 MINUTE INTERVAL STEPS
# show interval activity (as a time series)
plot( activity_dt$steps ~ activity_dt$interval, type = "l", xlab="interval", ylab="steps", main="Steps at each Interval each Day" )
# show interval activity at a quasi-density plot over time
plot( activity_dt$steps ~ activity_dt$interval, col=as.factor(activity_dt$date), xlab="interval", ylab="steps", main="Steps at each Interval each Day" )

# calculate interval averages over each day
interval_steps_mean_dt = activity_dt[,mean(steps, na.rm=TRUE), by=interval]
setnames(interval_steps_mean_dt, "V1", "average_inverval_steps")
# graph interval averages as time series plot
plot( interval_steps_mean_dt, type="l", xlab="interval", ylab="average steps", main="Average Steps at each Interval over all Days" )

# IMPUTING MISSING VALUES

# missing values
sum(is.na(activity_dt$steps))
# total observations
nrow(activity_dt)

# interval step medians
interval_steps_median_dt = activity_dt[,median(steps, na.rm=TRUE), by=interval]
setnames(interval_steps_median_dt, "V1", "median_inverval_steps")

# build a copy for imputed values
imputed_activity_dt <- merge(activity_dt, interval_steps_median_dt, by="interval", all=FALSE)
# mark observations with missing data
imputed_activity_dt$na_data <- is.na(imputed_activity_dt$steps)
# make a new variable for imputed steps (start with the observed steps)
imputed_activity_dt$imputed_steps <- imputed_activity_dt$steps
# any imputed steps with missing data is replaced with the median value (over the study) for that interval
imputed_activity_dt[na_data==TRUE, imputed_steps := median_inverval_steps]


# imputed graph, mean and median 
imputed_steps_each_day = imputed_activity_dt[,sum(steps),by=date]
setnames(imputed_steps_each_day, "V1", "daily_step_total")
imputed_mean_steps_per_day = mean( imputed_steps_each_day$daily_step_total, na.rm=TRUE )
imputed_median_steps_per_day = median( imputed_steps_each_day$daily_step_total, na.rm=TRUE )

# histogram of steps each day
barplot(imputed_steps_each_day$daily_step_total, xlab="date", ylab="steps", main="Total Steps taken each Day")


 

# graph

# mean & median



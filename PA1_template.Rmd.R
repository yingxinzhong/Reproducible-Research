## Load dataset
activity <- read.csv("Downloads/activity.csv")
###What is mean total number of steps taken per day?
##1.Calculate the total number of steps taken per day
TotalStepsPerDay <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
##2.Make a histogram of the total number of steps taken each day
hist(TotalStepsPerDay, main = "Histogram of the total number of steps taken each day", xlab = "TotalStepsPerDay" )
##3.Calculate and report the mean and median of the total number of steps taken per day
TotalStepsPerDayMean <- mean(TotalStepsPerDay)
TotalStepsPerDayMedian <-median(TotalStepsPerDay)

###What is the average daily activity pattern?
##1.Make a time series plot (i.e. type = "1)  of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
mean_steps <- with(activity, tapply(steps,activity$interval,mean))
new_na<- na.omit(activity)
interval_steps <- aggregate(steps ~ interval, new_na, mean)
plot(interval_steps$interval, interval_steps$steps, type='l', col=1, 
     main="Average number of steps averaged across all days", xlab="Interval", 
     ylab="Average number of steps")
##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?max
max_steps<- which.max(interval_steps$steps)## interval    steps
##104      835 206.1698
###Imputing missing values
##1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
NumMissingValues <- length(which(is.na(activity$steps))) ##2304
##2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
##We can use mean to replace missing values in interval_steps.
##3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
data_imputed <- activity
for (i in 1:nrow(data_imputed)) {
  if (is.na(data_imputed$steps[i])) {
    interval_value <- data_imputed$interval[i]
    steps_value <- interval_steps[
      interval_steps$interval == interval_value,]
    data_imputed$steps[i] <- steps_value$steps
  }
}
data_imputed
is.na(data_imputed$steps)
###Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps 
###taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact
###of imputing missing data on the estimates of the total daily number of steps?
#total number of steps taken each day
TotalStepsPerDayNew <- aggregate(steps ~ date, data_imputed, sum)
hist(TotalStepsPerDayNew$steps, main="Histogram of total number of steps per day", 
     xlab="Total number of steps a day")
#Mean of total number of steps
mean(TotalStepsPerDayNew$steps)
median(TotalStepsPerDayNew$steps)
#Mean of data with NA
mean(TotalStepsPerDay$steps)
#Median of data with NA
median(TotalStepsPerDay$steps)
##Mean values are the same, but median new data set imputting NAs is larger.
###Are there differences in activity patterns between weekdays and weekends?
##1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is 
##a weekday or weekend day. 
install.packages("dates")
data_imputed['daytype'] <- weekdays(as.Date(data_imputed$date))
data_imputed$daytype[data_imputed$daytype %in% c('Saturday','Sunday') ] <- "weekend"
data_imputed$daytype[data_imputed$daytype != "weekend"] <- "weekday"

data_imputed$daytype <- as.factor(data_imputed$daytype)
TotalStepsPerDayNew <- aggregate(steps ~ interval + daytype, data_imputed, mean)
install.packages("ggplot2")
library(ggplot2)
qplot(interval, steps, data = TotalStepsPerDayNew,  type = '1', 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
  facet_wrap(~ daytype, ncol = 1)
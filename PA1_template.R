
library(lubridate)
library(plyr)
library(ggplot2)

#Loading and preprocessing the data
1. Load the data (i.e. read.csv())
source <- read.csv("activity.csv")

2. Process/transform the data (if necessary) into a format suitable for your analysis
# Changing the data type of source$date to Date
source$date <- as.Date(source$date)


#What is mean total number of steps taken per day?

##Make a histogram of the total number of steps taken each day

steps_day <- ddply(source, .(date), summarize, total = sum(steps))

hist(steps_day$total, main = "Total Steps per Day", xlab = "Total Steps",ylab="Day", col = "gray")


##Calculate and report the mean and median total number of steps taken per day
mean(steps_day$total,na.rm=TRUE)

median(steps_day$total,na.rm=TRUE)


#What is the average daily activity pattern?

#1 . Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
tail(source)

avg_steps_day <- ddply(source, .(interval), summarize, mean_steps = mean(steps,na.rm=T))

plot(mean_steps ~ interval, data = avg_steps_day, type = "l"
     ,main = ("Mean of steps vs. Interval "), 
     ylab = "Mean of steps")


# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?

avg_steps_day[which.max(avg_steps_day$mean_steps), ]$interval


##Imputing missing values

# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

sum(is.na(source$steps))


#2. Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

#3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
avg_steps_day <- ddply(source, .(interval), summarize, mean_steps = mean(steps,na.rm=T))

mydata <- merge(source, avg_steps_day, by=c("interval"))

count = 0  # Count the number of data filled in
for (i in 1:nrow(mydata)) {
  if (is.na(mydata[i, ]$steps)) {
    mydata[i, ]$steps <- mydata[i, ]$mean_steps
    count = count + 1
  }
}
cat("Total ", count, "NA values were filled.\n\r")



#4.Make a histogram of the total number of steps taken each day and Calculate and report
#the mean and median total number of steps taken per day. Do these values differ 
#from the estimates from the first part of the assignment? What is the impact
#of imputing missing data on the estimates of the total daily number of steps?

steps_day <- ddply(mydata, .(date), summarize, total = sum(steps))

hist(steps_day$total, main = "Total Steps per Day", xlab = "Total Steps",ylab="Day", col = "gray")


##Calculate and report the mean and median total number of steps taken per day
mean(steps_day$total,na.rm=TRUE)

median(steps_day$total,na.rm=TRUE)

# Are there differences in activity patterns between weekdays and weekends?

#1.Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating
#whether a given date is a weekday or weekend day.

mydata$day <- as.factor(ifelse(weekdays(mydata$date) %in% c("sábado","domingo"),"weekend","weekday"))


#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute 
# interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend 
# days (y-axis). The plot should look something like the following, 
# which was created using simulated data:
  
  
avg_steps_day <- ddply(mydata, .(day,interval), summarize, mean_steps = mean(steps,na.rm=T))


qplot(interval,mean_steps,data=avg_steps_day,ylab="Average Steps" ,main="Average Steps vs. Interval",group=day,geom = "line") + facet_wrap(~ day, ncol=1)


# remove the data frames to free memory
rm(list=ls())




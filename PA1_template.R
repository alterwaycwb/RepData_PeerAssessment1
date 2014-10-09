#Reproducible Research: Peer Assessment 1
#========================================================
  
#  R Properties
#----------------------
  
#  ```{r environment}
R.version
Sys.getlocale()  ## FROM BRAZIL ( Portuguese )
#```


#Loading necessary packages 
#------------------------------------
  
#  ```{r packages,message=FALSE}
require(plyr)
require(ggplot2)
#```



#Loading and preprocessing the data
#--------------------------------------
  
#  1.Load the data (i.e. read.csv())
#```{r load_data}
#read the datasource
source <- read.csv("activity.csv")

#View some values from "source"" dataframe 
head(source)

# View the summary from "source" dataframe
summary(source)

# View some some atribute from "source" dataframe
str(source)

#```


#2.Process/transform the data (if necessary) into a format suitable for your analysis

#```{r }
# Changing the data type of source$date to Date
source$date <- as.Date(source$date)
#```


#What is mean total number of steps taken per day?
#--------------------------------------
  
#  1.Make a histogram of the total number of steps taken each day

#```{r histogram1, fig.align='center',fig.width=6,fig.height=6}

#Summarize the sum of steps grouped by date
steps_day <- ddply(source, .(date), summarize, total = sum(steps))

#Make the histogram 
hist(steps_day$total, main = "Total Steps per Day", xlab = "Total Steps", col = "gray")
#```


#2.Calculate and report the mean and median total number of steps taken per day

#```{r }
mean(steps_day$total,na.rm=TRUE)
median(steps_day$total,na.rm=TRUE)
#```

#```{r echo=FALSE}
mean <- mean(steps_day$total,na.rm=TRUE)
median <- median(steps_day$total,na.rm=TRUE)
#```

#* The mean of total steps taken per day is **`r mean`**.

#* The median of total steps taken per day is  **`r median`**.


#What is the average daily activity pattern?
#--------------------------------------
  
#  1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all days (y-axis).

#```{r  avg_daily_activity,fig.align='center',fig.width=6,fig.height=6}

#getting average daily activity grouping the mean of steps by interval
avg_steps_day <- ddply(source, .(interval), summarize, mean_steps = mean(steps,na.rm=T))

#plotting the average daily activity
plot(mean_steps ~ interval, data = avg_steps_day, type = "l",main = ("Mean of steps vs. Interval "), ylab = "Mean of steps")

#```



#2.Which 5-minute interval, on average across all the days in the dataset,contains the maximum number of steps?

```{r}
#find the maximum number of steps
avg_steps_day[which.max(avg_steps_day$mean_steps), ]$interval

#```

#```{r echo=FALSE}
#find the maximum number of steps
x <- avg_steps_day[which.max(avg_steps_day$mean_steps), ]$interval

#```
#* It is the **`r x`th** interval.

#Imputing missing values
#--------------------------------------
  
#  1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

#```{r}
#Total missing values
sum(is.na(source$steps))
#```


#* There are **`r sum(is.na(source$steps))`** values missing.


#2.Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that #5-minute interval, etc.

#**I chose the strategy of setting  the mean for that 5-minute to all missing values.**
  
#  3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

#```{r}
# Getting the mean of steps for interval
avg_steps_day <- ddply(source, .(interval), summarize, mean_steps = mean(steps,na.rm=T))

#Merging the source dataframe with the avg_steps_day using the interval key
newdatasource <- merge(source, avg_steps_day, by=c("interval"))

#Filling all the missing values from newdatasource by the mean of steps on that interval.
count = 0  # Count the number of data filled in
for (i in 1:nrow(newdatasource)) {
  if (is.na(newdatasource[i, ]$steps)) {
    newdatasource[i, ]$steps <- newdatasource[i, ]$mean_steps
    count = count + 1
  }
}

## Shows the total values filled after the loop.
cat("Total ", count, "NA values were filled.\n\r")

#```


#4.Make a histogram of the total number of steps taken each day and Calculate and report
#the mean and median total number of steps taken per day. 
#Do these values differ from the estimates from the first part of the assignment?
#What is the impact of imputing missing data on the estimates of the total daily number of steps?

#```{r  fig.align='center',fig.width=6,fig.height=6}

# Making a histogram of the total number of steps taken each day
steps_day_no_missing <- ddply(newdatasource, .(date), summarize, total = sum(steps))

hist(steps_day_no_missing$total, main = "Total Steps per Day", xlab = "Total Steps", col = "gray")

#```


#```{r}
#Calculate and report the mean and median total number of steps taken per day
mean(steps_day_no_missing$total)

median(steps_day_no_missing$total)

#```

#```{r echo=FALSE}
mean2 <- mean(steps_day_no_missing$total)
median2 <- median(steps_day_no_missing$total)
#```

#* The mean of total steps taken per day is **`r mean2`**.

#* The median of total steps taken per day is  **`r median2`**.

#*  The mean value is the same as the value before imputing missing data because we put the mean value for that particular 5
#-min interval. The median value shows a little difference .


#Are there differences in activity patterns between weekdays and weekends?
#--------------------------------------
  
#  1.Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating
#whether a given date is a weekday or weekend day.

#```{r}
# I'm using Brazilian Locale ( Portuguese ), so the weekend days are:
# saturday -> sábado 
# sunday  -> domingo 
newdatasource$day <- as.factor(ifelse(weekdays(newdatasource$date) %in% c("sábado","domingo"),"weekend","weekday"))

#```


#2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute 
#interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend 
#days (y-axis). 

#```{r  fig.align='center',fig.width=6,fig.height=6}
avg_steps_day <- ddply(newdatasource, .(day,interval), summarize, mean_steps = mean(steps,na.rm=T))

qplot(interval,mean_steps,data=avg_steps_day,ylab="Average Steps",main="Average Steps vs. Interval" ,group=day,geom = "line") + facet_wrap(~ day, ncol=1)

#```


#Removing all variables from R memory. 
#-------------------------------------------
  
#  ```{r}
# remove the variables to free memory
rm(list=ls())
#```


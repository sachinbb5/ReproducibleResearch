#Load the data (i.e. read.csv())

fURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

setInternet2(use = TRUE) # necessary under windows to download files when using knitr
download.file(fURL, destfile='activity.zip') # add " method='curl' " if downloading fails
unzip('activity.zip')

#Process/transform the data (if necessary) into a format suitable for your analysis
activity <- read.csv('activity.csv')
activity$date <- as.Date(activity$date, format= "%Y-%m-%d")

#Calculate th total number off steps taken per day
stepsByDay<-aggregate(steps ~ date, activity, sum)

#Make a histogram of the total number of steps taken each day
hist(stepsByDay$steps, main= "Total steps by day")

#Calculate and report the mean and median of the total number of steps taken per day
mean(stepsByDay$steps)
median(stepsByDay$steps)

##What is the average daily activity pattern?
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

intervals <- aggregate(steps ~ interval, data=activity,  mean)
plot(intervals$interval, intervals$steps, type="l", 
     ylab="Avg. Steps per day", xlab="5min Interval",
     main="Avg. Daily Activity Pattern") 


#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

maxId <- which.max(intervals$steps)
intvMax <- intervals$interval[maxId]

##Imputing missing values

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
idNAs  <- which(is.na(activity$steps))
numNAs <- length(idNAs)

#Create a new dataset that is equal to the original dataset but with the missing data filled in.

actMS <- activity
actMS$steps[idNAs] <- sapply(actMS[idNAs,"interval"], 
                                   function(a) {
                                     id <- which(intervals[,"interval"]==a)
                                     intervals[id,"steps"]
                                   } ) 
#Histogram of the total number of steps taken each day after imputing
stepsByDayImp <- aggregate(steps ~ date, data=activityImp, FUN = sum)
hist(stepsByDayImp$steps,breaks=seq(0,26000,2000), 
     main = "Histogram of Total-Steps per day", xlab="Total Steps")

#The mean and median of the total number of steps taken per day after impuding

meanByDayImp <- mean(stepsByDayImp$steps)
medianByDayImp <- median(stepsByDayImp$steps)

##Are there differences in activity patterns between weekdays and weekends?
#For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

library(timeDate)
activityWD <- activityImp
activityWD$weekday   <- as.factor(weekdays(activityWD$date)) # not necessary, just for comparison
isw <- gsub("TRUE", "weekday", as.character(isWeekday(activityWD$date)))
activityWD$isweekday <- as.factor(gsub("FALSE", "weekend", isw))


#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
library(lattice)
intervalsWD <- aggregate(steps ~ interval + isweekday, data=activityWD, FUN = mean)
xyplot(steps ~ interval | isweekday, data = intervalsWD, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")


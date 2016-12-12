## Loading and preprocessing the data

# Load the data
df <- read.csv("activity.csv")
# Process/transform the data (if necessary) into a format suitable for your analysis
tns <- aggregate(steps ~ date, data = df, sum, na.rm = TRUE) # Total Steps
msi <- aggregate(steps ~ interval, data = df, mean, na.rm = TRUE) # Steps Interval

## What is mean total number of steps taken per day?

# Calculate the total number of steps taken per day

tns <- aggregate(steps ~ date, data = df, sum, na.rm=TRUE)
#tns$date <- as.Date(tns$date, "%Y-%m-%d")
hist(tns$steps, col ="red", main = "Total Number of Steps taken each day",
	 xlab = "Steps taken each day", ylab = "Frequency")
# Calculate and report the mean and median of the total number of steps taken per day
mean(tns$steps)
median(tns$steps)

## What is the average daily activity pattern?

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

plot(steps ~ interval, data = msi, type = "l",
	 main = "5-minute interval and the average number of steps taken",
	 xlab = "Interval", ylab ="averaged stpes across all days")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

msi[ which.max(msi$steps), "interval" ]

## Imputing missing values

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(df$steps))

# Create a new dataset that is equal to the original dataset but with the missing data filled in
# My strategy for filing in all of the missing values in the dataset was to fill the mean for the 5-minute interval that has missing values.

dfn <- df   # Make a copy of the dataset with the original data

for(i in 1:nrow(df)) {
	if( is.na(dfn[i, "steps"]) ) {
		dfn[i, "steps"] <- msi[ msi$interval == df[i, "interval"], "steps"]
	}
}

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day

tns2 <- aggregate(steps ~ date, data = dfn, sum)
hist(tns2$steps, col ="red", main = "Total Number of Steps taken each day",
	 xlab = "Steps taken each day", ylab = "Frequency (without missing values)")
mean(tns2$steps)
median(tns2$steps)

#Are there differences in activity patterns between weekdays and weekends?

#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

dfn$day <- ifelse(as.POSIXlt(as.Date(dfn$date))$wday %% 6 == 0,
				  "weekend", "weekday")
dfn$day <- factor(dfn$day, levels = c("weekday", "weekend") )

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:
msi2 <- aggregate(steps ~ interval + day, data = dfn, mean)
library(lattice)
xyplot(steps ~ interval | factor(day), data = msi2, layout = c(1,2), type="l")




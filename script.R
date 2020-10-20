link <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(link, destfile = "dataset.zip")
unzip("dataset.zip")
data <- read.csv("activity.csv")
library(ggplot2)
##first part
steps <- aggregate(steps ~ date, data, sum)
hist(steps$steps, main = "Total Steps Each Day", xlab ="# Steps", col = "lightblue")
stepsMean <- mean(steps$steps)
stepsMedian <- median(steps$steps)
## second part
stepsbyinterval <- aggregate(steps ~ interval, data, mean)
ggplot(stepsbyinterval, aes(x = interval, y = steps)) + geom_line(color="darkgreen", size = 1) + labs(title = "Average Number of Steps per Day by Interval",x="Interval",y="Number of Steps")
maxInterval <- stepsbyinterval[which.max(stepsbyinterval$steps),1]

##third part
NATotal <- sum(!complete.cases(data))
StepsAverage <- aggregate(steps ~ interval, data = data, FUN = mean)
fillNA <- numeric()
for (i in 1:nrow(data)) {
  obs <- data[i, ]
  if (is.na(obs$steps)) {
    steps <- subset(StepsAverage, interval == obs$interval)$steps
  } else {
    steps <- obs$steps
  }
  fillNA <- c(fillNA, steps)
}
new_activity <- data
new_activity$steps <- fillNA
rmeantotal <- mean(StepsTotalUnion$steps)
StepsTotalUnion <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)
hist(StepsTotalUnion$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
#Create Histogram to show difference. 
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="green", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "green"), lwd=10)

rmeantotal <- mean(StepsTotalUnion$steps)

rmediantotal <- median(StepsTotalUnion$steps)

rmediandiff <- rmediantotal - rmedian
rmeandiff <- rmeantotal - rmean

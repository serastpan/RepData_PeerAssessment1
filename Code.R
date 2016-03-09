# Assignment 1
library(plyr)
library(dplyr)
library(ggplot2)
data <- read.csv("activity.csv")

# What is mean total number of steps taken per day?
stepsByDay <- with(data, aggregate(steps, by = list(date), FUN = sum))
png(filename = "plot1.png",
    width = 480, height = 480, units = "px")
barplot(stepsByDay$x, xlab = "Day", ylab = "Total Steps", main = "Number of steps taken each day")
dev.off()
summary(stepsByDay$x)[4]
summary(stepsByDay$x)[3]
# R: mean = 10770, median = 10760

# What is the average daily activity pattern?
AvStepsByDay <- with(data, aggregate(steps, by = list(interval), FUN = mean, na.rm = TRUE))
png(filename = "plot2.png",
    width = 480, height = 480, units = "px")
plot(AvStepsByDay$Group.1, AvStepsByDay$x, type = "l", xlab = "Interval", ylab = "Average Steps", main = "Average steps pattern")
dev.off()
AvStepsByDay$Group.1[AvStepsByDay$x == max(AvStepsByDay$x)]
# R: In interval 835 is where the maximum average number of steps is reached

# Imputing missing values
sum(is.na(data$steps))
MeanStepsByDay <- with(data, aggregate(steps, by = list(interval), FUN = mean, na.rm = TRUE))
vect <- is.na(data$steps)
vect2 <- rep(MeanStepsByDay$x, length(vect)/length(MeanStepsByDay$x))
newData <- cbind(vect2, vect)
newData <- cbind(data, newData)
newData$steps[is.na(newData$steps)] <- 0
newData <- mutate(newData, steps = vect2*vect + steps)
newData <- newData[c("steps", "date", "interval")]
newStepsByDay <- with(newData, aggregate(steps, by = list(date), FUN = sum))
png(filename = "plot3.png",
    width = 480, height = 480, units = "px")
barplot(newStepsByDay$x, xlab = "Day", ylab = "Total Steps", main = "Number of steps taken each day")
dev.off()
summary(newStepsByDay$x)[4]
summary(newStepsByDay$x)[3]
# R: Missing values 2304. mean = 10770, median = 10770. Imputing missing data influenced on the median value

# Are there differences in activity patterns between weekdays and weekends?
factVar <- as.POSIXlt(as.Date(newData$date))$wday
factVar[factVar>0 & factVar<6] <- "weekday"
factVar[factVar != "weekday"] <- "weekend"
factVar <- as.factor(factVar)

newData2 <- cbind(newData, factVar)
newData2 <- group_by(newData2, factVar, interval)
results <- summarise(newData2, step = mean(steps))

p <- qplot(interval, step, data = results, facets = factVar~., geom = "line", main = "Average number of steps taken for week and weekend")
png(filename = "plot4.png",
    width = 480, height = 480, units = "px")
plot(p)
dev.off()

library(knitr)
knit2html("PA1_template.Rmd")

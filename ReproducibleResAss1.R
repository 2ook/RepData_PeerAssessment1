##############################################
#Script for Reproducible Research Assignment 1
##############################################

#loading packages needed in script

library(lubridate)
library(ggplot2)
library(lattice)
library(reshape2)
library(psych)
library(knitr)

#reading in data and setting date field to date format

data <- read.csv('activity.csv')
data[,'date'] <- ymd(data[,'date'])

#setting up data to report statistics for total number of steps per day

data.melt <- melt(data,id=c('interval','date'),measure.vars='steps',na.rm=TRUE)
data.dcast <- dcast(data.melt,date~variable,sum)

#plotting histogram of total number of steps per day
#and calculating summary statistics

hist(data.dcast$steps,col='red',breaks=10,main='Histogram of Total Steps Per Day',xlab='Numer of Steps')
describe(data.dcast$steps)

#setting up data to determine mean number of steps by 5 minute interval

data.interval.dcast <- dcast(data.melt,interval~variable,mean)

#plotting mean number of steps per 5 minute interval across all days
#highlighted and labeled point/interval with average maximum number of steps

max.row <- which(data.interval.dcast$steps == max(data.interval.dcast[,'steps']))
ggplot(data.interval.dcast,aes(interval,steps)) + 
  geom_line() + 
  geom_point(data=data.interval.dcast[max.row,],shape=15,colour="red", size=5) +
  geom_text(data=data.interval.dcast[max.row,], label=paste('Max @ Interval ',toString(data.interval.dcast[max.row,'interval'])), hjust=1.1,col='blue') +
  ylab('Number of Steps') +
  xlab('Time Interval') +
  ggtitle('Average Number of Steps Per Time Interval')

#first evaluating the number of NA values across all data columns

sapply(data, function(x) sum(is.na(x)))

# another way of calculating, just focused on total number of rows with NA values

nrow(data) - sum(complete.cases(data))

#creating a data set dealing with NA values

data.filled <- data

#filling in NA values with the average value for the interval

for (interval in unique(data.filled$interval)) {
  val <- data.interval.dcast[data.interval.dcast == interval,'steps'][1]
  data.filled[data.filled$interval == interval & is.na(data.filled$steps),'steps'] <- val
}

#getting new data set with total number of steps per day from filled data set (NA values replaced with interval means)

data.filled.melt <- melt(data.filled,id=c('interval','date'),measure.vars='steps')
data.filled.dcast <- dcast(data.filled.melt,date~variable,sum)

#plotting histogram of total number of steps per daya with missing values filled
#also getting summary statistics for the new data set

hist(data.filled.dcast$steps,col='red',breaks=10,main='Histogram of Total Steps Per Day',xlab='Numer of Steps')
describe(data.filled.dcast$steps)

#now comparing the values from the original data set with missing values to the filled data set

par(mfrow=c(1,2))
hist(data.filled.dcast$steps,col='red',breaks=10,main='Steps Per Day (Missing Filled)',xlab='Numer of Steps',ylim=c(0,25))
hist(data.dcast$steps,col='blue',breaks=10,main='Steps Per Day',xlab='Numer of Steps',ylim=c(0,25))
describe(data.filled.dcast$steps)
describe(data.dcast$steps)


#getting day names for each day using the date column

data.filled$dayNames <- wday(data.filled$date, label = TRUE, abbr = TRUE)

#add weekend and weekday categories using day names

data.filled$weekend <- ifelse(data.filled$dayNames %in% c('Mon','Tues','Wed','Thurs','Fri'),'Weekday','Weekend')

#creating data set to get mean number of steps per interval for both weekends and weekdays

data.filled.weekday.melt <- melt(data.filled,id=c('interval','date','dayNames','weekend'),measure.vars='steps')
data.filled.weekday.dcast <- dcast(data.filled.weekday.melt,interval+weekend~variable,mean)

#plotting difference in activity levels per interval by both weekend and weekday

xyplot(steps ~ interval | weekend,data=data.filled.weekday.dcast,layout=c(1,2),scales = "free",type="l",main='Average Number of Steps Per Interval')

#knitting to HTML

knit2
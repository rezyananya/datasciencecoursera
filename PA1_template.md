1) I have placed my all graphs in PA1_template.rmd file and the same graphs is displaying in my html output.
2) I have placed one separate folder for graphs as well.

3) Below you can see my whole work for this assignment:

#First we are reading the data
FootStep <- read.csv(file="C:/Users/Priyanka/Documents/Reproducible/activity.csv",header = T,sep = ",",stringsAsFactors = FALSE)

FootStep$date <-as.Date(FootStep$date,"%Y-%m-%d")
str(FootStep)


#Histogram of the total number of steps taken each day
MeanTotal <- FootStep %>% group_by(date) %>%
  summarize(TotalSteps = sum(steps,na.rm = T),
  MeanSteps = mean(steps,na.rm = T))
  
  png(filename = 'hist.png', width = 480, height = 480, units='px')
  with(MeanTotal, hist(TotalSteps, xlab = 'Total Steps per day', main = 'Frequency', col = 'red'))
  
  #important to off
  dev.off()
#Graph is showing the hightest steps taken during the period (10000 - 15000).  

#Calculate and report the mean and median of the total number of steps taken per day
  summary(MeanTotal$TotalSteps)
  summary(MeanTotal$MeanSteps)

#What is the average daily activity pattern?
  
  AverageDailyPattern <- FootStep %>% group_by(interval) %>%
    summarize(MeanSteps = mean(steps,na.rm = T))
  
#1.make time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, 
#averaged across all days (y-axis).  
  
  #png graph
  png(filename = 'timeseries.png', width = 480, height = 480, units='px')
  plot(AverageDailyPattern, xlab = 'interval', ylab = 'Average number of steps taken', type = 'l')
  
  #important to off
  dev.off()
#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?
#Ans: Maxium number of steps taken between the interval (500-1000).
  
#Imputing missing values
#1.Calculate and report the total number of missing values in the dataset 
#(i.e the total number of rows with NAs) 
  mean(is.na(FootStep$steps))
  sum(is.na(FootStep$steps))
# Near about thirteen percent of the values are missing from the data.  
  
  
#As we can see we don't have any missing values here now.
  sum(is.na(AverageDailyPattern$MeanSteps))
  
#Create a new dataset (totalNA) that is equal to the original dataset but with the missing data filled in.
  totalNA <- FootStep
  for (j in 1:nrow(totalNA)) {
    if (is.na(totalNA$steps[j])) {
      index <- totalNA$interval[j]
      value <- subset(AverageDailyPattern, interval==index)
      totalNA$steps[j] <- value$MeanSteps}
  }
  
  head(totalNA)

  MeanTotal2 <- totalNA %>% group_by(date)%>%
    summarize (TotalSteps = sum(steps,na.rm=T),
  MeanSteps = mean(steps,na.rm = T))

png(filename = 'hist2.png', width = 480, height = 480, units='px')
with(MeanTotal2, hist(TotalSteps, xlab = 'Total Steps per day', main = 'Frequency', col = 'blue'))

#important to off
dev.off()
#Graph is showing the hightest steps taken during the period (10000 - 15000) again with new dataset
#but this time with a littel increase in the frequency as compare to previous data.Yes there is little difference
#in their mean and median.

summary(MeanTotal$TotalSteps)
summary(MeanTotal2$TotalSteps)

#Are there differences in activity patterns between weekdays and weekends?
totalNA$day<- ifelse(weekdays(totalNA$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

  Weekend <- filter(totalNA, day == "weekend")
  Weekday <- filter(totalNA, day == "weekday")
  Weekend <- Weekend %>% group_by(interval) %>%
  summarize(MeanSteps = mean(steps)) 
  Weekend$day <- "weekend"

  Weekday <- Weekday %>% group_by(interval) %>%
  summarize(MeanSteps = mean(steps)) 
  Weekday$day <- "weekday"

  ActivityPattern <- rbind(Weekend, Weekday)
  ActivityPattern$day <- as.factor(ActivityPattern$day)
  ActivityPattern$day <- relevel(ActivityPattern$day, "weekend")
  
#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval 
#(x-axis) and the average number of steps taken, averaged across all weekday days or weekend 
#days (y-axis).
  
#png graph
  library(ggplot2)
  png(filename = 'timeseries2.png', width = 480, height = 480, units='px')
  
  g <- ggplot(ActivityPattern, aes (interval, MeanSteps))
  g + geom_line() + facet_grid (day~.)+ labs(y = "Average number of steps taken") + labs(x = "Interval")
#important to off
  dev.off()
  
#Conclusion: Yes,there is lot of difference between activity patterns between weekdays and weekends.
#As we can see from the graph that there is no high level activity for weekend data and the activity
#is preity constant through out the period of time but for weekday we can see high level activity at particular
#point of time only.  
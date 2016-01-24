require('ggplot2')

require('data.table')

require('dplyr')

require('tidyr')

require('lubridate')

require('doBy')

activity <- read.csv('activity.csv', header=TRUE)

activity_day <- data.frame(summaryBy(steps ~ date ,data = activity, FUN = list(sum)))

turnaroundsByDaySummary <- 
  data.frame(summaryBy(TURNAROUND/1000 ~ RECHARGETYPE + dayOnly, data = daily1DayCuts, 
                       FUN = list(mean, max, min, median, sd)))

qplot(activity_day$steps, geom = "histogram")

mean_steps <- mean(activity_day$steps.sum, na.rm=TRUE)
median_steps <- median(activity_day$steps.sum, na.rm=TRUE)

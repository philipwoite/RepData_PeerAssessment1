require('ggplot2')

require('data.table')

require('dplyr')

require('tidyr')

require('lubridate')

require('doBy')

activity <- read.csv('activity.csv', header=TRUE)
activity_complete <- activity[complete.cases(activity),]

activity_day <- data.frame(summaryBy(steps ~ date ,data = activity_complete, FUN = list(sum)))


qplot(activity_day$steps, geom = "histogram")

mean_steps <- mean(activity_day$steps.sum, na.rm=TRUE)
median_steps <- median(activity_day$steps.sum, na.rm=TRUE)

activity_intervals <- data.frame(summaryBy(steps ~ interval, data = activity_complete , FUN = list(mean)))

## ggplot(yt.views, aes(Date, Views)) + geom_line() +
##  scale_x_date(format = "%b-%Y") + xlab("") + ylab("Daily Views")

ggplot(activity_intervals, aes(interval, steps.mean))+ geom_line()

# Between 0800 and 0900 in the morning
my_filler <- function(x) {if activity[,steps] == Na {activity[,steps] <- activity_intervals[,steps.mean]}
  
activity_filled <- lapply(activity, ffunction(x) {if activity[,steps] == Na {activity[,steps] <- activity_intervals[,steps.mean]})


ecode <- function(x, search, replace, default = NULL) {
  
  # build a nested ifelse function by recursion
  decode.fun <- function(search, replace, default = NULL)
    if (length(search) == 0) {
      function(x) if (is.null(default)) x else rep(default, length(x))
    } else {
      function(x) ifelse(x == search[1], replace[1],
                         decode.fun(tail(search, -1),
                                    tail(replace, -1),
                                    default)(x))
    }
  
  return(decode.fun(search, replace, default)(x))
}

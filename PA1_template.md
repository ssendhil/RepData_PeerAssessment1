Loading and preprocessing the data
==================================

    setwd("~/Desktop/Coursera/5_Reproducible_Research/Project 1")
    library(plyr)
    library(ggplot2)
    library(lattice) 
    library(knitr)

    url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    file <- "step_data.zip"
    download.file(url, file)
    unzip(file)
    activity <- read.csv("activity.csv", sep = ",")

    dim(activity)   #17568 x 3

    ## [1] 17568     3

    names(activity) #variable names

    ## [1] "steps"    "date"     "interval"

    str(activity)   #transform dates into Date format

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

    activity$date <- as.Date(activity$date, "%Y-%m-%d") #fix dates

What is mean total number of steps taken per day?
=================================================

    summary_df <- aggregate(steps ~ date, data=activity, FUN=sum)
    head(summary_df)

    ##         date steps
    ## 1 2012-10-02   126
    ## 2 2012-10-03 11352
    ## 3 2012-10-04 12116
    ## 4 2012-10-05 13294
    ## 5 2012-10-06 15420
    ## 6 2012-10-07 11015

    ggplot(summary_df, aes(steps)) + 
      geom_histogram(boundary=0, binwidth=2500, col="purple", fill="pink") + 
      ggtitle("Total Steps per Day") + 
      xlab("Steps") + ylab("Frequency")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    # Save file
    #dev.copy(png, file = "histogram1.png", height = 480, width = 480)
    #dev.off()

    round(summary(summary_df$steps)[3:4])

    ## Median   Mean 
    ##  10765  10766

**The median steps per day is 10765.**

**The mean steps per day is 10766.**

What is the average daily activity pattern?
===========================================

    #Remove NAs
    clean_activity <- activity[!is.na(activity$steps),]

    #Create average number of steps per interval
    interval_df <- ddply(clean_activity, .(interval), summarize, Avg = mean(steps))

    #Plot of avg number of steps per interval
    ggplot(interval_df, aes(x=interval, y=Avg),
           xlab = "Interval", ylab="Avg Number of Steps") +
      geom_line(color="blue")+xlab("Interval")+ylab("Avg Number of Steps") + 
      ggtitle("Average Number of Steps per Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    # Save file
    # dev.copy(png, file = "interval_plot.png", height = 480, width = 480)
    # dev.off()

    #Which 5-minute interval contains the maximum number of steps?
    a <- which(interval_df$Avg==max(interval_df$Avg))
    paste("The",interval_df[a,1],"interval contains the max number of steps")

    ## [1] "The 835 interval contains the max number of steps"

Imputing missing values
=======================

    #Number of missing values in the dataset
    nrow(activity[is.na(activity$steps),])

    ## [1] 2304

    #Handle missing data by imputing averages from intervals

    ##Create a new dataset that is equal to the original dataset but with the missing data filled in.
    activity_imputed <- activity

    ##Find which dates have NAs
    dates.na <- names(table(activity[which(is.na(activity$steps)==TRUE),2]))
    dates.na

    ## [1] "2012-10-01" "2012-10-08" "2012-11-01" "2012-11-04" "2012-11-09"
    ## [6] "2012-11-10" "2012-11-14" "2012-11-30"

    ##Find all the rows with NAs
    rows.na <- list() 

    for(i in 1:length(dates.na)){
      rows.na[[i]] <- which(activity_imputed$date==dates.na[i])
    }

    rows_with_nas <- unlist(rows.na)

    ##Impute the missing values (rep x 8 for each of the 8 days with missing values)
    activity_imputed$steps[rows_with_nas] <- rep(round(interval_df$Avg[1:288]),8)

    #Make a histogram of the total number of steps taken each day.
    daily_steps_df <- aggregate(activity_imputed$steps, list(activity_imputed$date), FUN=sum)
    names(daily_steps_df) <- c("date","steps")
    ggplot(daily_steps_df, aes(steps)) + 
      geom_histogram(boundary=0, binwidth=2500, col="purple", fill="pink") + 
      ggtitle("Steps per day") + 
      xlab("Steps") + ylab("Frequency")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    # Save file
    # dev.copy(png, file = "histogram2.png", height = 480, width = 480)
    # dev.off()

    #Calculate and report the mean and median total number of steps taken per day.
    round(summary(daily_steps_df$steps)[3:4])

    ## Median   Mean 
    ##  10762  10766

    #Do these values differ from the estimates from the first part of the assignment? (See below)
    #What is the impact of imputing missing data on the estimates of the total daily number of steps? (See below)

**There are 2304 missing values.**

**The mean steps per day is 10766 and the median steps per day is
10762.**

**The values do not differ much after imputation. The distributions look
the same.**

**The impact of imputing missing data results in the same mean daily
total steps. The median is reduced from 10765 to 10762 total daily
steps.**

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

    #Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

    activity_imputed$day <- NA
    activity_imputed$day <- weekdays(activity_imputed$date)
    activity_imputed$day <- gsub("Monday|Tuesday|Wednesday|Thursday|Friday","Weekday",activity_imputed$day)
    activity_imputed$day <- gsub("Saturday|Sunday","Weekend",activity_imputed$day)
    activity_imputed$day <- as.factor(activity_imputed$day)

    str(activity_imputed) #show that "day" is factor variable

    ## 'data.frame':    17568 obs. of  4 variables:
    ##  $ steps   : num  2 0 0 0 0 2 1 1 0 1 ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
    ##  $ day     : Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...

    table(activity_imputed$day)

    ## 
    ## Weekday Weekend 
    ##   12960    4608

    #Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

    ## Summarize data by interval and type of day
    interval_df2 <- ddply(activity_imputed, .(interval, day), summarize, avg = mean(steps))
    head(interval_df2)

    ##   interval     day       avg
    ## 1        0 Weekday 2.2888889
    ## 2        0 Weekend 0.2500000
    ## 3        5 Weekday 0.4000000
    ## 4        5 Weekend 0.0000000
    ## 5       10 Weekday 0.1555556
    ## 6       10 Weekend 0.0000000

    ##Plot data in a panel plot
    xyplot(avg~interval|day, data=interval_df2, type="l",  layout = c(1,2),
           main="Avg Steps per Interval by Day", 
           ylab="Avg Number of Steps", xlab="Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    # Save file
    # dev.copy(png, file = "panel_plot.png", height = 480, width = 480)
    # dev.off()

**Activity patterns appear about the same for weekdays and weekends.**

 
    library(ggplot2)
    library(lattice)
    setwd("D:/CoursERA/Course5/Assignment1")

#### Loading and preprocessing the data

    unzip(zipfile="./activity.zip", overwrite = TRUE)
    activity      <- read.csv(file = './activity.csv', stringsAsFactors = FALSE)
    activity$date <- as.Date(activity$date,'%Y-%m-%d')
    activityCC    <- subset(activity, complete.cases(activity))

#### What is mean total number of steps taken per day?

#1. Total number of steps taken per day:

    dailyTotSteps<-aggregate(formula = steps~date, data = activity, 
                             FUN =   sum,na.rm=TRUE)
    head(dailyTotSteps)

#2. Histogram of the total number of steps taken each day

    plot(dailyTotSteps$date,dailyTotSteps$steps,type="h", 
    main = "Histogram of the total number of steps taken per day",
    ylab="Total  number of steps taken each day", xlab=" October to November, 2012", 
    lwd=5, col="green")


#3. Mean and Median of the total number of steps taken per day

    cat("Mean of the total number of steps taken per day
        =",round(mean(dailyTotSteps[,2]),digits=0),"\n")

    cat("Median of the total number of steps taken per day
        =",median(dailyTotSteps[,2]),"\n")

  
#### What is the average daily activity pattern?  
#1. Time series plot  


    aveInvl <-  aggregate(formula = steps~interval, data = activityCC, 
                          FUN = mean)

    ggplot(data = aveInvl, aes(x = interval, y = steps)) +
    geom_line(colour="red") + xlab("5-minute interval")+ 
      ylab("Average number of steps taken") 

#2. Five minute inteval for the maximum number of steps

    maxSteps <- activityCC[which(activityCC$steps==max(activityCC$steps)),]
    maxSteps

## Imputing missing values
#1. Calculate and report the number of missing values in the data set
 
    cat("Total number of missing values in the data set = ",
        dim(activity)[1] - dim(activityCC)[1],"\n")

#2. The missing value for steps is imputed with mean steps from corresponding intervals with observed values.

#3. New data set with imputation

# selecting intervals for missing steps 

    miss<-which(is.na(activity$steps))
    data_miss<-activity[miss,]
    data_miss_int<-unique(data_miss$interval)

# selecting non-missing steps for selected intervals
    
    data_nomiss<-activity[activity$interval == data_miss_int 
                          & !is.na(activity$steps),]
    
# creating two files with and without missing
    
    miss_obs<-activity[miss,]
    comp_obs<-activity[-miss,] 
    
# Computing mean steps by selected intervals
    
    intMean <-  aggregate(formula = steps~interval, data = data_nomiss, 
                            FUN = mean, na.rm=TRUE)
    
# Replacing each missing value with the mean value
    
    new_activity<- merge(miss_obs[,2:3],intMean,by="interval")
    new_activity<-rbind(new_activity[,c(3,2,1)],comp_obs)
    head(new_activity)

#4. Histogram of the total number of steps from imputed data set
 
    aveImp <-  aggregate(formula = steps~date, data = new_activity, 
                          FUN = sum)

    plot(aveImp$date,aveImp$steps,type="h", 
    main = "Histogram of the total number of steps taken each day",
    ylab="Total  number of steps day taken each day", xlab=" Date", 
    lwd=5, col="blue")
    
      cat("Mean of the total number of steps taken per day
        =",round(mean(aveImp[,2]),digits=0),"\n")

    cat("Median of the total number of steps taken per day
        =",round(median(aveImp[,2]),digits=0),"\n")

# The mean and median from imputed data set does not differs from those of without imputed data set.

#### Are there differences in activity patterns between weekdays and weekends?

#1. Create a factor variable and panel plot

      new_activity$day<-factor(ifelse(as.integer(format(new_activity$date,
                        format = '%u')) %in% c(1:5), 'weekday', 'weekend'))

      newMean<-aggregate(steps~interval+day, data = new_activity, FUN = mean)

      xyplot(newMean$steps~newMean$interval | newMean$day,layout=c(1,2),
             type="l", xlab="5-minute interval", 
             ylab="Average number of steps taken")


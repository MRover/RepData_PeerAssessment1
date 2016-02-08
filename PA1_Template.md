---
title: "Module5_Project1"
output: html_document
---

# Activity Tracker Study
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as the Fitbit, Nike Fuelband, Galaxy Gear Fit or Jawbone Up. These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or to affiliate themselves with the tech community. But, typically, this data remains under-utilized both because the raw data are hard to obtain and because there is a lack of statistical methods and software for processing and interpreting the data that are easily understood by the layperson.

The following short study makes use of data obtained from a personal activity monitoring device. This device collected pedometer (step tracking) data from an anonymous subject at 5 minute intervals through out the day for two months (October ~ November, 2012).

## Accessing the data
New packages needed for this assignment to be downloaded mannulay on users machine:

1. install.packages("ggplots")
2. install.packages("gridExtra")
3. install.packages("dplyr"). 


```{r packageInstalls, results='hide'}
# load the libraries we will need
library(ggplot2)
library(dplyr)
library(gridExtra)

```

The activity data is provided in the github repo and can be downloaded from the following URL:

https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

R code "Chunk" to check if data file is downloaded or if unzipped and is present in the same directory as .Rmd file.

```{r Look For The Right Data File}
if(file.exists("~/Desktop/R_Classes/Module_5/activity.csv")){
                print("Dataset already downloaded and unzipped")
                print("Loading Data...")
}else if(file.exists("~/Desktop/R_Classes/Module_5/repdata-data-activity.zip")){
                print("Dataset downloaded. Now Unzipping...")
                unzip("~/Desktop/R_Classes/Module_5/repdata-data-activity.zip")
                print("Completed.")
                print("Loading Data...")
}else if(!file.exists("~/Desktop/R_Classes/Module_5/repdata-data-activity.zip")){
                print("Downloading and unzipping dataset...")
                download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","./repdata-data-activity.zip")
                unzip("~/Desktop/R_Classes/Module_5/repdata-data-activity.zip")
                print("Completed.")
                print("Loading Data...")
}

##Use read.csv
Act_Data<-read.csv("./activity.csv", header=TRUE, sep=",", )
##Chnage date -> Date class
Act_Data$date<-as.Date(Act_Data$date)
##Look at Act_Data table, number of rows and columns
summary(Act_Data)
hmVars<-ncol(Act_Data)
hmObs<-nrow(Act_Data)

```

This data set, "activity.csv", consists of `r hmVars` variables and `r hmObs` observation.

Create a new data set to get rid of observations where data is missing.

```{r Omit NA}
Act_DataClean <- na.omit(Act_Data)
Act_DataClean$date <- as.factor(Act_DataClean$date)
##Summarize new Act_DataClean table
summary(Act_DataClean)
## variables for documenting the observations and variables for the activity dataset
hmObs1<-nrow(Act_DataClean)
hmVars1<-ncol(Act_DataClean)
```

New Act_DataClean table has only `r hmObs1` observation of `r hmVars1` variables now.

## Mean and median number of steps taken each day

To subset the data, calculate the sum, median and mean of the daily step count, create a third data set based on the Act_DataCleaned dataset. 

```{r New Group}
Act_Data_Calc <- Act_DataClean %.% 
        group_by(date) %.% 
        summarise (step.sum = sum(steps), step.mean = round(mean(steps)), step.median=median(steps)) 
Act_Data_Calc
```


Next to show the distribution of steps across the two month recording window in a histogram with the daily stepcount mean and median provided in the legend.

```{r}
Act_Data_Calc_Mean <- round(mean(Act_Data_Calc$step.sum))
Act_Data_Calc_Median <- median(Act_Data_Calc$step.sum)

p <- ggplot(Act_Data_Calc, aes(x=step.sum)) + geom_histogram(binwidth=750, colour="black", fill="white")
p <- p+geom_vline(aes(xintercept=Act_Data_Calc_Mean), color="#80F28A", size = 5, linetype = "solid", alpha=0.45)
p <- p+geom_vline(aes(xintercept=Act_Data_Calc_Median), color="#E293C0", linetype = "dotted", size=1, alpha=1)
p <-  p+ggtitle("A Histogram of the Total Step Counts per Day from October ~ November")+ theme(plot.title = element_text(lineheight=.8, face="bold"))
p<-p+xlab("StepCount Daily Totals") + ylab("Frequency of Step Count Totals")
p <- p+geom_text(aes(Act_Data_Calc_Mean,0,label = "mean =", hjust=-1, vjust = -23))
p <- p+geom_text(aes(Act_Data_Calc_Mean,0,label = Act_Data_Calc_Mean, hjust=-2.5, vjust = -23))
p <- p+geom_text(aes(Act_Data_Calc_Median,0,label = Act_Data_Calc_Median, hjust=-2.5, vjust = -21))
p <- p+geom_text(aes(Act_Data_Calc_Median,0,label = "median = ", hjust=-0.5, vjust = -21))
p
```


the mean stepcount (October~November): `r Act_Data_Calc_Mean`

the mean stepcount (October~November): `r Act_Data_Calc_Median`

## What is the average daily activity pattern?

General trend in users activity in that day: Look at the number of steps in each interval across a given day to observe the pedometer wearer's daily activity. Take take the mean of each intervals across every day in the two month recording window to observe some pattern.

```{r fig.width=10}
##New clean data
Act_DataClean2 <- na.omit(Act_Data)
## chnage the interval col to a factor
Act_DataClean2$interval <- as.factor(Act_DataClean2$interval)
## Use interval factor and average to group
Act_Data_Calc2 <- Act_DataClean2 %.% 
        group_by(interval) %.% 
        summarise (step.mean = mean(steps))
Act_Data_Calc2
```

```{r fig.width=10}
l <- ggplot(data=Act_Data_Calc2, aes(x=as.numeric(levels(interval))[interval], y=step.mean, group=1)) + geom_line(colour="#3A75CD", linetype="solid", size=2) + geom_point(colour="#EB92F7", size=1.5, shape=21, fill="#EB92F7")
l <- l + scale_x_continuous(breaks=c(0,500,1000, 1500, 2000, 2500))
l <- l + ggtitle("Average Number of Steps per Interval from October ~ November")+ theme(plot.title = element_text(lineheight=.8, face="bold"))
l <- l + xlab("StepCount Recording Interval (800 = 8:00AM, etc)") + ylab("Average StepCount")
l
```

Use the following code chunk in R to find the most active time of the day, on average, across the 2 month recording window: 
```{r maxactivity}
Act_Data_Calc2[which.max(Act_Data_Calc2$step.mean),]
```

The most active interval for this anon user is ~ 200+ steps at the 8:35AM interval.

## Missing value (NA) Imputing 

Make an array with the missing values to determine how many missing values there are using either summary or the following code chunk:
```{r How many NA}
missingValues<-Act_Data[which(is.na(Act_Data)),]
```
```{r How many NAs}
nrow(missingValues)
```

Before replacing NAs, copy the orginal dataset into a new tmp dataset in order to keep the original data with NAs
In original data set select the intervals where the step count is equal to NA and replace those "steps" with the step.mean of the same index from our Act_Data_Calc2 dataset.


```{r Impute MVs}
Act_New_Data<- Act_Data
for(i in 1:nrow(Act_New_Data)){
        if(is.na(Act_New_Data[i,]$steps)){
                tmp<-Act_New_Data[i,]$interval  
                Act_New_Data[i,]$steps <- Act_Data_Calc2[which(Act_Data_Calc2$interval==tmp),]$step.mean
        }        
}
summary(Act_New_Data)
```

Check the overall effect of the imputation on the mean, median and daily averages.

```{r IMP Summary}
Act_Data_Calc3 <- Act_New_Data %.% 
        group_by(date) %.% 
        summarise (step.sum = sum(steps), step.mean = round(mean(steps)), step.median=median(steps)) 
Act_Data_Calc3
```


```{r imputeAct_Data_Calc_Meanhisto2,fig.width=10}
Act_Data_Calc_Mean3 <- round(mean(Act_Data_Calc3$step.sum))
Act_Data_Calc_Median3 <- round(median(Act_Data_Calc3$step.sum))

p <- ggplot(Act_Data_Calc3, aes(x=step.sum)) + geom_histogram(binwidth=750, colour="black", fill="white")
p <- p+geom_vline(aes(xintercept=Act_Data_Calc_Mean), color="#80F28A", size = 5, linetype = "solid", alpha=0.45)
p <- p+geom_vline(aes(xintercept=Act_Data_Calc_Median), color="#E293C0", linetype = "dotted", size=1, alpha=1)
p <-  p+ggtitle("A Histogram of the Total Step Counts per Day from October ~ November")+ theme(plot.title = element_text(lineheight=.8, face="bold"))
p<-p+xlab("StepCount Daily Totals") + ylab("Frequency of Step Count Totals")
p <- p+geom_text(aes(Act_Data_Calc_Mean3,0,label = "mean =", hjust=-1, vjust = -23))
p <- p+geom_text(aes(Act_Data_Calc_Mean3,0,label = Act_Data_Calc_Mean3, hjust=-2.5, vjust = -23))
p <- p+geom_text(aes(Act_Data_Calc_Median3,0,label = Act_Data_Calc_Median3, hjust=-2.5, vjust = -21))
p <- p+geom_text(aes(Act_Data_Calc_Median3,0,label = "median = ", hjust=-0.5, vjust = -21))

p

```

As observed, the imputation had only a small effect on the overall median. 

The mean stepcount (October~November): `r Act_Data_Calc_Mean3`

The mean stepcount (October~November): `r Act_Data_Calc_Median3`

## Are there differences in activity patterns between weekdays and weekends?
Plot the steps per interval averaged across every day during the two month sample period. Seperate the weekends from the weekdays, by a new factor column, to look at the two time periods seperately. 
Convert date to a Date classa and create a new column in our data frame to be able to select for weekends or weekdays.

```{r Column}
Act_New_Data$date <- as.Date(Act_New_Data$date)
class(Act_New_Data$date)

test2<-Act_New_Data
```

Apply grouping and summary operations as:

```{r New Set}
##mutate a new col using weekdays, an inline ifelse and dplyr
test3 <- mutate(test2, day = ifelse(weekdays(Act_New_Data$date)=="Sunday" | weekdays(Act_New_Data$date)=="Saturday", "weekend", "weekday"))

## convert to a factor
test3$day<-as.factor(test3$day)
##provide a summary to confirm
summary(test3)
```

Group and summarise the data by the weekend and weekday factor as:

```{r }
##group by interval factor and average
test3$interval <- as.factor(test3$interval)

Act_Data_Calc5 <- test3 %.% 
        filter(day=="weekend") %.%
        group_by(interval) %.% 
        summarise (step.mean = mean(steps))
Act_Data_Calc5

Act_Data_Calc4 <- test3 %.% 
        filter(day=="weekday") %.%
        group_by(interval) %.% 
        summarise (step.mean = mean(steps))
Act_Data_Calc4
```

Average weekend and weekday activities, by sample interval, across the two month sample period to create one line plot for each as:

```{r lineplot3, fig.width=10}
l <- ggplot(data=Act_Data_Calc5, aes(x=as.numeric(levels(interval))[interval], y=step.mean, group=1)) + geom_line(colour="#3A75CD", linetype="solid", size=2) + geom_point(colour="#EB92F7", size=1.5, shape=21, fill="#EB92F7")
l <- l + scale_x_continuous(breaks=c(0,500,1000, 1500, 2000, 2500))
l <- l + ggtitle("Average Number of Steps per Interval on Weekends")+ theme(plot.title = element_text(lineheight=.8, face="bold"))
l <- l + xlab("StepCount Recording Interval (800 = 8:00AM, etc)") + ylab("Average StepCount")
```
```{r lineplot4, fig.width=10}
r <- ggplot(data=Act_Data_Calc4, aes(x=as.numeric(levels(interval))[interval], y=step.mean, group=1)) + geom_line(colour="#3A75CD", linetype="solid", size=2) + geom_point(colour="#EB92F7", size=1.5, shape=21, fill="#EB92F7")
r <- r + scale_x_continuous(breaks=c(0,500,1000, 1500, 2000, 2500))
r <- r + ggtitle("Average Number of Steps per Interval on WeekDays")+ theme(plot.title = element_text(lineheight=.8, face="bold"))
r <- r + xlab("StepCount Recording Interval (800 = 8:00AM, etc)") + ylab("Average StepCount")
```

So plot these l and r as:

```{r, echo=TRUE}
plot(l)
plot(r)
```

Observation: higher activity level on average throughout the day on the weekend but a lower peak activity level between 5AM ~ 10AM. 

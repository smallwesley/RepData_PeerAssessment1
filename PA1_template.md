# Reproducible Data Coursera; Peer Assignment 1
Wesley Small  
August 8, 2015  


### Preliminary activities
1. Load libraries DPLYR, LUBRIDATE AND GGPLOT2.


```r
library(dplyr)
library(lubridate)
library(ggplot2)
```

2. Helper functions created for tidy data/plotting; Setup for the step interval scale to the logical hour and associated label.

```r
intervalDivision <- split( 0:288, ceiling(seq_along(0:288)/12))

getStepScaleBreaks <- function() { 
    output <- numeric(length = 25) 
    for(i in 1:25) 
        output[i] <-intervalDivision[[i]][1] 
    output
}

getStepScaleLabels <- function() { 
    output <- numeric(length = 25) 
    for(i in 1:25) {
        dayPeriod <-"am"
        if (i > 13) dayPeriod <- "pm"
        output[i] <- paste0(i-1,":00 ", dayPeriod)
    }
    output
}
```

***
### TASK 1: "Load the data"
Load the dataset and perform some tranformations suitable for further analysis below.

```r
dfActivity <- read.csv("activity.csv")
```

Transformations to original dataset:

```r
dfActivity$date <- ymd(dfActivity$date)
dfActivity$date <- as.factor(dfActivity$date)
dfActivity$interval <- as.factor(dfActivity$interval) 
dtActivity <- tbl_df(dfActivity)
```

The Initial Activity dataset has the following structure:

```r
str(dfActivity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```
***
### TASK 2: "What is mean total number of steps taken per day?""

```r
subsetDTActivityA <- 
    dtActivity %>%
    na.omit() %>%
    select( date, steps) %>%
    filter(!(is.na(steps) | is.nan(steps))) %>%
    group_by(date) %>%
    summarise(
        tot_steps = sum(steps),
        avg_steps = mean(steps, na.rm = TRUE),
        med_steps = median(steps, na.rm = TRUE))
```
The histogram of the total number of steps taken each day

```r
qplot(
    subsetDTActivityA$tot_steps,
    binwidth = 500,
    geom = "histogram", 
    main = "Histogram Registering Number of Days Reaching Step Totals",
    xlab = "Number of Steps Taken",
    ylab = "Number of Days",
    fill = I("lightgreen"), 
    colour = I("black")) 
```

![](PA1_template_files/figure-html/task2step2-1.png) 

Calculate and report the mean and median total number of steps taken per day

```r
stepReport <- subsetDTActivityA %>%
                select(date,avg_steps,med_steps)
knitr::kable(stepReport)
```



date           avg_steps   med_steps
-----------  -----------  ----------
2012-10-02     0.4375000           0
2012-10-03    39.4166667           0
2012-10-04    42.0694444           0
2012-10-05    46.1597222           0
2012-10-06    53.5416667           0
2012-10-07    38.2465278           0
2012-10-09    44.4826389           0
2012-10-10    34.3750000           0
2012-10-11    35.7777778           0
2012-10-12    60.3541667           0
2012-10-13    43.1458333           0
2012-10-14    52.4236111           0
2012-10-15    35.2048611           0
2012-10-16    52.3750000           0
2012-10-17    46.7083333           0
2012-10-18    34.9166667           0
2012-10-19    41.0729167           0
2012-10-20    36.0937500           0
2012-10-21    30.6284722           0
2012-10-22    46.7361111           0
2012-10-23    30.9652778           0
2012-10-24    29.0104167           0
2012-10-25     8.6527778           0
2012-10-26    23.5347222           0
2012-10-27    35.1354167           0
2012-10-28    39.7847222           0
2012-10-29    17.4236111           0
2012-10-30    34.0937500           0
2012-10-31    53.5208333           0
2012-11-02    36.8055556           0
2012-11-03    36.7048611           0
2012-11-05    36.2465278           0
2012-11-06    28.9375000           0
2012-11-07    44.7326389           0
2012-11-08    11.1770833           0
2012-11-11    43.7777778           0
2012-11-12    37.3784722           0
2012-11-13    25.4722222           0
2012-11-15     0.1423611           0
2012-11-16    18.8923611           0
2012-11-17    49.7881944           0
2012-11-18    52.4652778           0
2012-11-19    30.6979167           0
2012-11-20    15.5277778           0
2012-11-21    44.3993056           0
2012-11-22    70.9270833           0
2012-11-23    73.5902778           0
2012-11-24    50.2708333           0
2012-11-25    41.0902778           0
2012-11-26    38.7569444           0
2012-11-27    47.3819444           0
2012-11-28    35.3576389           0
2012-11-29    24.4687500           0

***
### TASK 3: "What is the average daily activity pattern?"
Shown below is a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):

```r
# SUMMARIZE DATASET TO OBTAIN AVERAGE STEPS
subsetDTActivityB <- 
  dtActivity %>% 
  select( interval, steps) %>%
  filter( !is.na(steps) ) %>%
  group_by(interval) %>%
  summarise(average_steps = mean(steps))

subsetDTActivityB$interval <- as.numeric(subsetDTActivityB$interval) 
```

```r
qplot(
      interval, 
      average_steps,
      data = subsetDTActivityB,
      geom = "line",
      main = "Average Number Step Rate Record Over A Day",
      xlab = "Hour of Day [Generated from 288 5 minute Observations]",
      ylab = "Average Steps Rate") + 
    scale_x_continuous(breaks = getStepScaleBreaks(), labels = getStepScaleLabels()) +
    theme(axis.text.x = element_text(angle=90))
```

![](PA1_template_files/figure-html/task3step2-1.png) 

#### "Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?"

```r
subsetDTActivityC <- 
    dtActivity %>% 
    select( interval, steps) %>%
    filter( !is.na(steps) ) %>%
    group_by(interval) %>%
    summarise(average_steps = mean(steps))
maxNumberSteps <- subsetDTActivityC[subsetDTActivityC$average_steps == max(subsetDTActivityC$average_steps),]
```
####ANSWER: 
Time interval labelled: 835 has the maximum average number of steps at 206.1698113

***
### TASK 4: Imputing Missing Values
Replacing the NAs in step count with averages of the total step count.  The Averages are rounded up to the nearest whole integer number.

In the provided Activity dataset, there are 2304 records that have a step count missing out of the total 17568


```r
# FILTER FOR COMPLETE VS INCOMPLETE
dtActivityComplete <- dtActivity[(complete.cases(dtActivity) == TRUE),]
dtActivityMissing <- dtActivity[(complete.cases(dtActivity) == FALSE),]

# CALCULATE INTERVAL BASED STEP AVERAGES
subsetDTActivityStepAvg <- 
  dtActivity %>%
  select( interval, steps) %>%
  group_by(interval) %>%
  summarise(average_steps = mean(steps, na.rm = TRUE))

# LEFT JOIN THE AVERAGES TO INCOMPLETE CASES
dtActivityMissingAvg <- inner_join(x = dtActivityMissing, y = subsetDTActivityStepAvg)
# ROUND UP THE AVERAGES AND ASSIGN TO THE STEPS COLUMN
dtActivityMissingAvg$steps <- ceiling(dtActivityMissingAvg$average_steps)

# OBTAIN A NEW DATASET FOR THE FIXED MISSING CASES
dtActivityNotMissing <- dtActivityMissingAvg[, (colnames(dtActivityMissingAvg) %in% c("steps","date","interval"))]

# RECREATE A NEW COMPLETE DATASET
dtActivityUpdated <- rbind(dtActivityComplete, dtActivityNotMissing)

# SUMMARIZE THE STEPS BY TOTAL #, AVERAGE AND MEDIAN
subsetDTActivityUpdated <- 
  dtActivityUpdated %>% 
  select( date, steps) %>%
  group_by(date) %>%
  summarise(tot_steps = sum(steps),
            avg_steps = mean(steps),
            med_steps = median(steps))
```


```r
# PLOT COMPLETE CASES DATASET
qplot(subsetDTActivityUpdated$tot_steps,
      binwidth = 500,
      geom = "histogram", 
      main = "Histogram Registering Number of Days Reaching Step Totals",
      xlab = "Number of Steps Taken",
      ylab = "Number of Days",
      fill = I("orange"), 
      colour = I("black")) 
```

![](PA1_template_files/figure-html/task4step2-1.png) 

***
### TASK 5: Activity patterns on weekdays vs. weekends?
The follow is the logic used to add the Weekend Vs Weekend Factor Categorical factors into the data.

```r
# CREATE NEW FUNCTION TO RETURN THE FACTOR BASED ON THE WEEKDAYS FUNCTION
getWeekPeriod <- function(x) { 
    if (weekdays(as.POSIXct(x)) %in% c("Saturday","Sunday"))
        as.factor("WEEKEND") 
    else
        as.factor("WEEKDAY") 
}
# BIND THE NEW CONTENT TO THE EXISTING TABLE
dtFinal<- cbind(dtActivityUpdated, sapply(dtActivityUpdated$date,getWeekPeriod))
colnames(dtFinal) <- c("steps","date","interval","weekperiod")

# CALCULATE THE AVERAGES GROUPED BY WEEK-PERIOD, AND INTERVAL
dtFinal$interval <- as.numeric(dtFinal$interval)
dtFinalScrubbed <- 
    dtFinal %>% 
    group_by(weekperiod, interval) %>%
    summarise(average_steps = mean(steps))
```

The following is the plot showing the activity level differences on the weekend vs the work week days.

```r
p <- qplot(
        interval, 
        average_steps, 
        data = dtFinalScrubbed, 
        facets = weekperiod ~ . , 
        geom = "line",
        color = weekperiod,
        main = "Average No. of Steps Taken by Time of Day\nDivided by Weekend Period",
        xlab = "Time Interval (288 5 minute Observations by Hour)",
        ylab = "Average Steps") +
    scale_x_continuous(breaks = getStepScaleBreaks(), labels = getStepScaleLabels()) +
    theme(axis.text.x = element_text(angle=90))
p
```

![](PA1_template_files/figure-html/task5step2-1.png) 

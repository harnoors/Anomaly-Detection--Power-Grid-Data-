library(lubridate)
library(ggplot2)
library("pracma")
set.seed(1)

# Task 4: Point anomaly detection
# Point anomaly detection for the 5 datasets were analysed using the following threshold values
# Weekdays: +/-0.75, +/-1.5, +/-2.25
# Weekends: +/-1.25, +/-2.5, +/-3.75

#Testdata1
#############################################################
#Reading the input File
dataframe <- read.table("./TestData/test1.txt", header = TRUE, sep = ",")
dataframe$Date <- as.POSIXct(dataframe$Date, format = "%d/%m/%Y")
dataframe$Time <- as.POSIXct(dataframe$Time, format = "%H:%M:%S")

# Weekday
dayData = dataframe[dataframe$Date == "2010-01-25",]
minThreshold = -0.75
maxThreshold = 0.75

#Hours for moving average window on a weekday and weekend is from 9am to 1pm
WindowHours <- dayData[hour(dayData$Time) >= 9 & hour(dayData$Time) < 13,]

movavgdata <- movavg(WindowHours$Global_active_power, 7, type="s")
WindowHours["MovingAvg"] <- as.numeric(movavgdata)

for (i in 1:nrow(WindowHours)) {
  difference <- (WindowHours$Global_active_power[i] - WindowHours$MovingAvg[i])
  WindowHours$Avg[i] <- difference
  if (!is.na(difference)){
    if(difference > maxThreshold || difference < minThreshold){
      WindowHours$Status[i] <- "Anomaly"
      WindowHours$ptShape[i] <- 8
      WindowHours$ptColor[i] <- "red"
    }
    else{
      WindowHours$Status[i] <- "Normal"
      WindowHours$ptShape[i] <- 20
      WindowHours$ptColor[i] <- "green"
    }
  }
  else{
    WindowHours$status[i] <- "NA"
  }
}

plot1 = ggplot(data=WindowHours, mapping=aes(x=Time, y=Global_active_power)) +
  geom_point(color=WindowHours$ptColor, shape=WindowHours$ptShape, size=3) +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=MovingAvg), size=1, color="blue") +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=Avg), size=1, color="orange") +
  geom_hline(yintercept=minThreshold, linetype="dashed", size=1, alpha=.5) +
  geom_hline(yintercept=maxThreshold, linetype="dashed", size=1, alpha=.5)+
  ggtitle("Moving Average For Weekday From 9am to 1pm For test data 1, threshold = +/- 0.75")+
  theme(plot.title = element_text(hjust = 0.5))

#Weekend
dayData = dataframe[dataframe$Date == "2010-01-24",]
minThreshold = -1.25
maxThreshold = 1.25

#Hours for moving average window on a weekday and weekend is from 9am to 1pm
WindowHours <- dayData[hour(dayData$Time) >= 9 & hour(dayData$Time) < 13,]

movavgdata <- movavg(WindowHours$Global_active_power, 7, type="s")
WindowHours["MovingAvg"] <- as.numeric(movavgdata)

for (i in 1:nrow(WindowHours)) {
  difference <- (WindowHours$Global_active_power[i] - WindowHours$MovingAvg[i])
  WindowHours$Avg[i] <- difference
  if (!is.na(difference)){
    if(difference > maxThreshold || difference < minThreshold){
      WindowHours$Status[i] <- "Anomaly"
      WindowHours$ptShape[i] <- 8
      WindowHours$ptColor[i] <- "red"
    }
    else{
      WindowHours$Status[i] <- "Normal"
      WindowHours$ptShape[i] <- 20
      WindowHours$ptColor[i] <- "green"
    }
  }
  else{
    WindowHours$status[i] <- "NA"
  }
}

plot2= ggplot(data=WindowHours, mapping=aes(x=Time, y=Global_active_power)) +
  geom_point(color=WindowHours$ptColor, shape=WindowHours$ptShape, size=3) +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=MovingAvg), size=1, color="blue") +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=Avg), size=1, color="orange") +
  geom_hline(yintercept=minThreshold, linetype="dashed", size=1, alpha=.5) +
  geom_hline(yintercept=maxThreshold, linetype="dashed", size=1, alpha=.5) +
  ggtitle("Moving Average For Weekend From 9am to 1pm For test data 1, threshold = +/- 1.25") +
  theme(plot.title = element_text(hjust = 0.5))

#Testdata2
#############################################################
dataframe <- read.table("./TestData/test2.txt", header = TRUE, sep = ",")
dataframe$Date <- as.POSIXct(dataframe$Date, format = "%d/%m/%Y")
dataframe$Time <- as.POSIXct(dataframe$Time, format = "%H:%M:%S")

# Weekday
dayData = dataframe[dataframe$Date == "2010-01-25",]
minThreshold = -0.75
maxThreshold = 0.75

#Hours for moving average window on a weekday and weekend is from 9am to 1pm
WindowHours <- dayData[hour(dayData$Time) >= 9 & hour(dayData$Time) < 13,]

movavgdata <- movavg(WindowHours$Global_active_power, 7, type="s")
WindowHours["MovingAvg"] <- as.numeric(movavgdata)

for (i in 1:nrow(WindowHours)) {
  difference <- (WindowHours$Global_active_power[i] - WindowHours$MovingAvg[i])
  WindowHours$Avg[i] <- difference
  if (!is.na(difference)){
    if(difference > maxThreshold || difference < minThreshold){
      WindowHours$Status[i] <- "Anomaly"
      WindowHours$ptShape[i] <- 8
      WindowHours$ptColor[i] <- "red"
    }
    else{
      WindowHours$Status[i] <- "Normal"
      WindowHours$ptShape[i] <- 20
      WindowHours$ptColor[i] <- "green"
    }
  }
  else{
    WindowHours$status[i] <- "NA"
  }
}

plot3 = ggplot(data=WindowHours, mapping=aes(x=Time, y=Global_active_power)) +
  geom_point(color=WindowHours$ptColor, shape=WindowHours$ptShape, size=3) +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=MovingAvg), size=1, color="blue") +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=Avg), size=1, color="orange") +
  geom_hline(yintercept=minThreshold, linetype="dashed", size=1, alpha=.5) +
  geom_hline(yintercept=maxThreshold, linetype="dashed", size=1, alpha=.5) +
  ggtitle("Moving Average For Weekend From 9am to 1pm For test data 2, threshold = +/- 0.75") +
  theme(plot.title = element_text(hjust = 0.5))

#Weekend
dayData = dataframe[dataframe$Date == "2010-01-24",]
minThreshold = -1.25
maxThreshold = 1.25

#Hours for moving average window on a weekday and weekend is from 9am to 1pm
WindowHours <- dayData[hour(dayData$Time) >= 9 & hour(dayData$Time) < 13,]

movavgdata <- movavg(WindowHours$Global_active_power, 7, type="s")
WindowHours["MovingAvg"] <- as.numeric(movavgdata)

for (i in 1:nrow(WindowHours)) {
  difference <- (WindowHours$Global_active_power[i] - WindowHours$MovingAvg[i])
  WindowHours$Avg[i] <- difference
  if (!is.na(difference)){
    if(difference > maxThreshold || difference < minThreshold){
      WindowHours$Status[i] <- "Anomaly"
      WindowHours$ptShape[i] <- 8
      WindowHours$ptColor[i] <- "red"
    }
    else{
      WindowHours$Status[i] <- "Normal"
      WindowHours$ptShape[i] <- 20
      WindowHours$ptColor[i] <- "green"
    }
  }
  else{
    WindowHours$status[i] <- "NA"
  }
}

plot4= ggplot(data=WindowHours, mapping=aes(x=Time, y=Global_active_power)) +
  geom_point(color=WindowHours$ptColor, shape=WindowHours$ptShape, size=3) +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=MovingAvg), size=1, color="blue") +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=Avg), size=1, color="orange") +
  geom_hline(yintercept=minThreshold, linetype="dashed", size=1, alpha=.5) +
  geom_hline(yintercept=maxThreshold, linetype="dashed", size=1, alpha=.5) +
  ggtitle("Moving Average For Weekend From 9am to 1pm For test data 2, threshold = +/- 1.25") +
  theme(plot.title = element_text(hjust = 0.5))

#Testdata3
#############################################################
dataframe <- read.table("./TestData/test3.txt", header = TRUE, sep = ",")
dataframe$Date <- as.POSIXct(dataframe$Date, format = "%d/%m/%Y")
dataframe$Time <- as.POSIXct(dataframe$Time, format = "%H:%M:%S")

# Weekday
dayData = dataframe[dataframe$Date == "2010-01-25",]
minThreshold = -0.75
maxThreshold = 0.75

#Hours for moving average window on a weekday and weekend is from 9am to 1pm
WindowHours <- dayData[hour(dayData$Time) >= 9 & hour(dayData$Time) < 13,]

movavgdata <- movavg(WindowHours$Global_active_power, 7, type="s")
WindowHours["MovingAvg"] <- as.numeric(movavgdata)

for (i in 1:nrow(WindowHours)) {
  difference <- (WindowHours$Global_active_power[i] - WindowHours$MovingAvg[i])
  WindowHours$Avg[i] <- difference
  if (!is.na(difference)){
    if(difference > maxThreshold || difference < minThreshold){
      WindowHours$Status[i] <- "Anomaly"
      WindowHours$ptShape[i] <- 8
      WindowHours$ptColor[i] <- "red"
    }
    else{
      WindowHours$Status[i] <- "Normal"
      WindowHours$ptShape[i] <- 20
      WindowHours$ptColor[i] <- "green"
    }
  }
  else{
    WindowHours$status[i] <- "NA"
  }
}

plot5 = ggplot(data=WindowHours, mapping=aes(x=Time, y=Global_active_power)) +
  geom_point(color=WindowHours$ptColor, shape=WindowHours$ptShape, size=3) +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=MovingAvg), size=1, color="blue") +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=Avg), size=1, color="orange") +
  geom_hline(yintercept=minThreshold, linetype="dashed", size=1, alpha=.5) +
  geom_hline(yintercept=maxThreshold, linetype="dashed", size=1, alpha=.5) +
  ggtitle("Moving Average For Weekend From 9am to 1pm For test data 3, threshold = +/- 0.75") +
  theme(plot.title = element_text(hjust = 0.5))

#Weekend
dayData = dataframe[dataframe$Date == "2010-01-24",]
minThreshold = -1.25
maxThreshold = 1.25

#Hours for moving average window on a weekday and weekend is from 9am to 1pm
WindowHours <- dayData[hour(dayData$Time) >= 9 & hour(dayData$Time) < 13,]

movavgdata <- movavg(WindowHours$Global_active_power, 7, type="s")
WindowHours["MovingAvg"] <- as.numeric(movavgdata)

for (i in 1:nrow(WindowHours)) {
  difference <- (WindowHours$Global_active_power[i] - WindowHours$MovingAvg[i])
  WindowHours$Avg[i] <- difference
  if (!is.na(difference)){
    if(difference > maxThreshold || difference < minThreshold){
      WindowHours$Status[i] <- "Anomaly"
      WindowHours$ptShape[i] <- 8
      WindowHours$ptColor[i] <- "red"
    }
    else{
      WindowHours$Status[i] <- "Normal"
      WindowHours$ptShape[i] <- 20
      WindowHours$ptColor[i] <- "green"
    }
  }
  else{
    WindowHours$status[i] <- "NA"
  }
}

plot6= ggplot(data=WindowHours, mapping=aes(x=Time, y=Global_active_power)) +
  geom_point(color=WindowHours$ptColor, shape=WindowHours$ptShape, size=3) +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=MovingAvg), size=1, color="blue") +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=Avg), size=1, color="orange") +
  geom_hline(yintercept=minThreshold, linetype="dashed", size=1, alpha=.5) +
  geom_hline(yintercept=maxThreshold, linetype="dashed", size=1, alpha=.5) +
  ggtitle("Moving Average For Weekend From 9am to 1pm For test data 3, threshold = +/- 1.25") +
  theme(plot.title = element_text(hjust = 0.5))

#Testdata4
#############################################################
dataframe <- read.table("./TestData/test4.txt", header = TRUE, sep = ",")
dataframe$Date <- as.POSIXct(dataframe$Date, format = "%d/%m/%Y")
dataframe$Time <- as.POSIXct(dataframe$Time, format = "%H:%M:%S")

# Weekday
dayData = dataframe[dataframe$Date == "2010-01-25",]
minThreshold = -0.75
maxThreshold = 0.75

#Hours for moving average window on a weekday and weekend is from 9am to 1pm
WindowHours <- dayData[hour(dayData$Time) >= 9 & hour(dayData$Time) < 13,]

movavgdata <- movavg(WindowHours$Global_active_power, 10, type="s")
WindowHours["MovingAvg"] <- as.numeric(movavgdata)

for (i in 1:nrow(WindowHours)) {
  difference <- (WindowHours$Global_active_power[i] - WindowHours$MovingAvg[i])
  WindowHours$Avg[i] <- difference
  if (!is.na(difference)){
    if(difference > maxThreshold || difference < minThreshold){
      WindowHours$Status[i] <- "Anomaly"
      WindowHours$ptShape[i] <- 8
      WindowHours$ptColor[i] <- "red"
    }
    else{
      WindowHours$Status[i] <- "Normal"
      WindowHours$ptShape[i] <- 20
      WindowHours$ptColor[i] <- "green"
    }
  }
  else{
    WindowHours$status[i] <- "NA"
  }
}

plot7 = ggplot(data=WindowHours, mapping=aes(x=Time, y=Global_active_power)) +
  geom_point(color=WindowHours$ptColor, shape=WindowHours$ptShape, size=3) +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=MovingAvg), size=1, color="blue") +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=Avg), size=1, color="orange") +
  geom_hline(yintercept=minThreshold, linetype="dashed", size=1, alpha=.5) +
  geom_hline(yintercept=maxThreshold, linetype="dashed", size=1, alpha=.5) +
  ggtitle("Moving Average For Weekend From 9am to 1pm For test data 4, threshold = +/- 0.75") +
  theme(plot.title = element_text(hjust = 0.5))

#Weekend
dayData = dataframe[dataframe$Date == "2010-01-24",]
minThreshold = -1.25
maxThreshold = 1.25

#Hours for moving average window on a weekday and weekend is from 9am to 1pm
WindowHours <- dayData[hour(dayData$Time) >= 9 & hour(dayData$Time) < 13,]

movavgdata <- movavg(WindowHours$Global_active_power, 10, type="s")
WindowHours["MovingAvg"] <- as.numeric(movavgdata)

for (i in 1:nrow(WindowHours)) {
  difference <- (WindowHours$Global_active_power[i] - WindowHours$MovingAvg[i])
  WindowHours$Avg[i] <- difference
  if (!is.na(difference)){
    if(difference > maxThreshold || difference < minThreshold){
      WindowHours$Status[i] <- "Anomaly"
      WindowHours$ptShape[i] <- 8
      WindowHours$ptColor[i] <- "red"
    }
    else{
      WindowHours$Status[i] <- "Normal"
      WindowHours$ptShape[i] <- 20
      WindowHours$ptColor[i] <- "green"
    }
  }
  else{
    WindowHours$status[i] <- "NA"
  }
}

plot8= ggplot(data=WindowHours, mapping=aes(x=Time, y=Global_active_power)) +
  geom_point(color=WindowHours$ptColor, shape=WindowHours$ptShape, size=3) +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=MovingAvg), size=1, color="blue") +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=Avg), size=1, color="orange") +
  geom_hline(yintercept=minThreshold, linetype="dashed", size=1, alpha=.5) +
  geom_hline(yintercept=maxThreshold, linetype="dashed", size=1, alpha=.5) +
  ggtitle("Moving Average For Weekend From 9am to 1pm For test data 4, threshold = +/- 1.25") +
  theme(plot.title = element_text(hjust = 0.5))

#Testdata5
#############################################################
dataframe <- read.table("./TestData/test5.txt", header = TRUE, sep = ",")
dataframe$Date <- as.POSIXct(dataframe$Date, format = "%d/%m/%Y")
dataframe$Time <- as.POSIXct(dataframe$Time, format = "%H:%M:%S")

# Weekday
dayData = dataframe[dataframe$Date == "2010-01-25",]
minThreshold = -0.75
maxThreshold = 0.75

#Hours for moving average window on a weekday and weekend is from 9am to 1pm
WindowHours <- dayData[hour(dayData$Time) >= 9 & hour(dayData$Time) < 13,]

movavgdata <- movavg(WindowHours$Global_active_power, 10, type="s")
WindowHours["MovingAvg"] <- as.numeric(movavgdata)

for (i in 1:nrow(WindowHours)) {
  difference <- (WindowHours$Global_active_power[i] - WindowHours$MovingAvg[i])
  WindowHours$Avg[i] <- difference
  if (!is.na(difference)){
    if(difference > maxThreshold || difference < minThreshold){
      WindowHours$Status[i] <- "Anomaly"
      WindowHours$ptShape[i] <- 8
      WindowHours$ptColor[i] <- "red"
    }
    else{
      WindowHours$Status[i] <- "Normal"
      WindowHours$ptShape[i] <- 20
      WindowHours$ptColor[i] <- "green"
    }
  }
  else{
    WindowHours$status[i] <- "NA"
  }
}

plot9 = ggplot(data=WindowHours, mapping=aes(x=Time, y=Global_active_power)) +
  geom_point(color=WindowHours$ptColor, shape=WindowHours$ptShape, size=3) +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=MovingAvg), size=1, color="blue") +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=Avg), size=1, color="orange") +
  geom_hline(yintercept=minThreshold, linetype="dashed", size=1, alpha=.5) +
  geom_hline(yintercept=maxThreshold, linetype="dashed", size=1, alpha=.5) +
  ggtitle("Moving Average For Weekend From 9am to 1pm For test data 5, threshold = +/- 0.75") +
  theme(plot.title = element_text(hjust = 0.5))

#Weekend
dayData = dataframe[dataframe$Date == "2010-01-24",]
minThreshold = -1.25
maxThreshold = 1.25

#Hours for moving average window on a weekday and weekend is from 9am to 1pm
WindowHours <- dayData[hour(dayData$Time) >= 9 & hour(dayData$Time) < 13,]

movavgdata <- movavg(WindowHours$Global_active_power, 10, type="s")
WindowHours["MovingAvg"] <- as.numeric(movavgdata)

for (i in 1:nrow(WindowHours)) {
  difference <- (WindowHours$Global_active_power[i] - WindowHours$MovingAvg[i])
  WindowHours$Avg[i] <- difference
  if (!is.na(difference)){
    if(difference > maxThreshold || difference < minThreshold){
      WindowHours$Status[i] <- "Anomaly"
      WindowHours$ptShape[i] <- 8
      WindowHours$ptColor[i] <- "red"
    }
    else{
      WindowHours$Status[i] <- "Normal"
      WindowHours$ptShape[i] <- 20
      WindowHours$ptColor[i] <- "green"
    }
  }
  else{
    WindowHours$status[i] <- "NA"
  }
}

plot10= ggplot(data=WindowHours, mapping=aes(x=Time, y=Global_active_power)) +
  geom_point(color=WindowHours$ptColor, shape=WindowHours$ptShape, size=3) +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=MovingAvg), size=1, color="blue") +
  geom_line(data=WindowHours, mapping=aes(x=Time, y=Avg), size=1, color="orange") +
  geom_hline(yintercept=minThreshold, linetype="dashed", size=1, alpha=.5) +
  geom_hline(yintercept=maxThreshold, linetype="dashed", size=1, alpha=.5) +
  ggtitle("Moving Average For Weekend From 9am to 1pm For test data 5, threshold = +/- 1.25") +
  theme(plot.title = element_text(hjust = 0.5))
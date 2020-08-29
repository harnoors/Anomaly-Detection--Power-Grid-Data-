library(lubridate)
library(ggplot2)
# install.packages("pracma")
library("pracma")
library(kernlab)
set.seed(1)

#Testdata1
#############################################################
#Reading the input File
dataframe <- read.table(pwd,"test1.txt", header = TRUE, sep = ",")
dataframe$Date <- as.POSIXct(dataframe$Date, format = "%d/%m/%Y")
dataframe$Time <- as.POSIXct(dataframe$Time, format = "%H:%M:%S")

# Weekday
dayData = dataframe[dataframe$Date == "2010-01-25",]

svm_model=kernlab::ksvm(dayData$Global_active_power,nu=0.09, type="one-svc", kernel="vanilladot")

get_index=predict(svm_model)
head(get_index)
out_index=which(get_index[,1]==TRUE)

plot(dayData$Global_active_power, col="blue", type="l")
points(x=out_index, y=dayData$Global_active_power[out_index], pch=18, col="red")


#Weekend
dayData = dataframe[dataframe$Date == "2010-01-24",]

svm_model=kernlab::ksvm(dayData$Global_active_power,nu=0.09, type="one-svc", kernel="vanilladot")

get_index=predict(svm_model)
head(get_index)
out_index=which(get_index[,1]==TRUE)

plot(dayData$Global_active_power, col="blue", type="l")
points(x=out_index, y=dayData$Global_active_power[out_index], pch=18, col="red")

#Testdata2
#############################################################
#Reading the input File
dataframe <- read.table(pwd,"test2.txt", header = TRUE, sep = ",")
dataframe$Date <- as.POSIXct(dataframe$Date, format = "%d/%m/%Y")
dataframe$Time <- as.POSIXct(dataframe$Time, format = "%H:%M:%S")

# Weekday
dayData = dataframe[dataframe$Date == "2010-01-25",]

svm_model=kernlab::ksvm(dayData$Global_active_power,nu=0.09, type="one-svc", kernel="vanilladot")

get_index=predict(svm_model)
head(get_index)
out_index=which(get_index[,1]==TRUE)

plot(dayData$Global_active_power, col="blue", type="l")
points(x=out_index, y=dayData$Global_active_power[out_index], pch=18, col="red")


#Weekend
dayData = dataframe[dataframe$Date == "2010-01-24",]

svm_model=kernlab::ksvm(dayData$Global_active_power,nu=0.09, type="one-svc", kernel="vanilladot")

get_index=predict(svm_model)
head(get_index)
out_index=which(get_index[,1]==TRUE)

plot(dayData$Global_active_power, col="blue", type="l")
points(x=out_index, y=dayData$Global_active_power[out_index], pch=18, col="red")

#Testdata3
#############################################################
#Reading the input File
dataframe <- read.table(pwd,"test3.txt", header = TRUE, sep = ",")
dataframe$Date <- as.POSIXct(dataframe$Date, format = "%d/%m/%Y")
dataframe$Time <- as.POSIXct(dataframe$Time, format = "%H:%M:%S")

# Weekday
dayData = dataframe[dataframe$Date == "2010-01-25",]

svm_model=kernlab::ksvm(dayData$Global_active_power,nu=0.09, type="one-svc", kernel="vanilladot")

get_index=predict(svm_model)
head(get_index)
out_index=which(get_index[,1]==TRUE)

plot(dayData$Global_active_power, col="blue", type="l")
points(x=out_index, y=dayData$Global_active_power[out_index], pch=18, col="red")


#Weekend
dayData = dataframe[dataframe$Date == "2010-01-24",]

svm_model=kernlab::ksvm(dayData$Global_active_power,nu=0.09, type="one-svc", kernel="vanilladot")

get_index=predict(svm_model)
head(get_index)
out_index=which(get_index[,1]==TRUE)

plot(dayData$Global_active_power, col="blue", type="l")
points(x=out_index, y=dayData$Global_active_power[out_index], pch=18, col="red")

#Testdata4
#############################################################
#Reading the input File
dataframe <- read.table(pwd,"test4.txt", header = TRUE, sep = ",")
dataframe$Date <- as.POSIXct(dataframe$Date, format = "%d/%m/%Y")
dataframe$Time <- as.POSIXct(dataframe$Time, format = "%H:%M:%S")

# Weekday
dayData = dataframe[dataframe$Date == "2010-01-25",]

svm_model=kernlab::ksvm(dayData$Global_active_power,nu=0.09, type="one-svc", kernel="vanilladot")

get_index=predict(svm_model)
head(get_index)
out_index=which(get_index[,1]==TRUE)

plot(dayData$Global_active_power, col="blue", type="l")
points(x=out_index, y=dayData$Global_active_power[out_index], pch=18, col="red")


#Weekend
dayData = dataframe[dataframe$Date == "2010-01-24",]

svm_model=kernlab::ksvm(dayData$Global_active_power,nu=0.09, type="one-svc", kernel="vanilladot")

get_index=predict(svm_model)
head(get_index)
out_index=which(get_index[,1]==TRUE)

plot(dayData$Global_active_power, col="blue", type="l")
points(x=out_index, y=dayData$Global_active_power[out_index], pch=18, col="red")

#Testdata5
#############################################################
#Reading the input File
dataframe <- read.table(pwd,"test5.txt", header = TRUE, sep = ",")
dataframe$Date <- as.POSIXct(dataframe$Date, format = "%d/%m/%Y")
dataframe$Time <- as.POSIXct(dataframe$Time, format = "%H:%M:%S")

# Weekday
dayData = dataframe[dataframe$Date == "2010-01-25",]

svm_model=kernlab::ksvm(dayData$Global_active_power,nu=0.09, type="one-svc", kernel="vanilladot")

get_index=predict(svm_model)
head(get_index)
out_index=which(get_index[,1]==TRUE)

plot(dayData$Global_active_power, col="blue", type="l")
points(x=out_index, y=dayData$Global_active_power[out_index], pch=18, col="red")


#Weekend
dayData = dataframe[dataframe$Date == "2010-01-24",]

svm_model=kernlab::ksvm(dayData$Global_active_power,nu=0.09, type="one-svc", kernel="vanilladot")

get_index=predict(svm_model)
head(get_index)
out_index=which(get_index[,1]==TRUE)

plot(dayData$Global_active_power, col="blue", type="l")
points(x=out_index, y=dayData$Global_active_power[out_index], pch=18, col="red")

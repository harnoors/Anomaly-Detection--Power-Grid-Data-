set.seed(1)
library(corrplot)
library(lubridate)
library(dplyr)
library(ggplot2)

df=read.table('TrainData.txt', header = TRUE, sep = ",", dec = ".")
a=dmy(df$Date)
df$year=year(a)
df$year= as.integer(df$year)

df$Date <- as.POSIXct(df$Date, format = "%d/%m/%Y") 
df$Time <- as.POSIXct(df$Time, format = "%H:%M:%S")

# Task 1: Data Exploration
# Observe the best combination of observed response variables for the provided electricity consumption data
new = select(df,c('Global_active_power','Global_reactive_power','Voltage','Global_intensity','Sub_metering_1','Sub_metering_2','Sub_metering_3'))
new = cor(new,use ="complete.obs", method = "pearson")

corrplot(new)

# Selected main response variable for univariate and multivariate HMM: 'Global_active_power'
# Reason: Active-or real or true-power does the actual work in the load
#         in other words, global active power represents real electrical 
#         resistance power consumption in circuits and electrical appliances
# Other responses with highest correlation to "Global_active_power': 'Global_intensity' (corr=0.7240937)
# All other features have an absolute correlation value of < 0.5 wrt 'Global_active_power', 
# hence not selected
# NOTE: the selection above happens to be similar to our correlation plot analysis in task 1 of assignment 3.
#       However, selection analysis are performed separately, by analysing the new 'train' dataset
#       for the 4 years data on electricity consumption for households

d=strftime(df$Date, format = "%u")
df$day_of_the_week <- as.integer(d)


# Year: 2006-2009
# Mondays
Mondays <- df[df$day_of_the_week == 1,]
Mondays$Hours = format(as.POSIXct(Mondays$Time, format("%H:%M:%S")), "%H")
Monday_global_active <- aggregate(Mondays$Global_active_power, by = list(Mondays$Hours), mean, na.rm=TRUE)

names(Monday_global_active)[names(Monday_global_active) == "x"] <- "GlobalActivePower"
names(Monday_global_active)[names(Monday_global_active) == "Group.1"] <- "Time"


temp <- factor(Monday_global_active$Time)
a <- hm(as.character(temp))
Monday_global_active$logicalTime <- as.integer(hour(a))

# Tuesdays
Tuesdays <- df[df$day_of_the_week == 2,]
Tuesdays$Hours = format(as.POSIXct(Tuesdays$Time, format("%H:%M:%S")), "%H")
Tuesday_global_active <- aggregate(Tuesdays$Global_active_power, by = list(Tuesdays$Hours), mean, na.rm=TRUE)

names(Tuesday_global_active)[names(Tuesday_global_active) == "x"] <- "GlobalActivePower"
names(Tuesday_global_active)[names(Tuesday_global_active) == "Group.1"] <- "Time"


temp <- factor(Tuesday_global_active$Time)
a <- hm(as.character(temp))
Tuesday_global_active$logicalTime <- as.integer(hour(a))

# Wednesdays
Wednesdays <- df[df$day_of_the_week == 3,]
Wednesdays$Hours = format(as.POSIXct(Wednesdays$Time, format("%H:%M:%S")), "%H")
Wednesday_global_active <- aggregate(Wednesdays$Global_active_power, by = list(Wednesdays$Hours), mean, na.rm=TRUE)

names(Wednesday_global_active)[names(Wednesday_global_active) == "x"] <- "GlobalActivePower"
names(Wednesday_global_active)[names(Wednesday_global_active) == "Group.1"] <- "Time"


temp <- factor(Wednesday_global_active$Time)
a <- hm(as.character(temp))
Wednesday_global_active$logicalTime <- as.integer(hour(a))

# Thursdays
Thursdays <- df[df$day_of_the_week == 4,]
Thursdays$Hours = format(as.POSIXct(Thursdays$Time, format("%H:%M:%S")), "%H")
Thursday_global_active <- aggregate(Thursdays$Global_active_power, by = list(Thursdays$Hours), mean, na.rm=TRUE)

names(Thursday_global_active)[names(Thursday_global_active) == "x"] <- "GlobalActivePower"
names(Thursday_global_active)[names(Thursday_global_active) == "Group.1"] <- "Time"


temp <- factor(Thursday_global_active$Time)
a <- hm(as.character(temp))
Thursday_global_active$logicalTime <- as.integer(hour(a))

# Fridays
Fridays <- df[df$day_of_the_week == 5,]
Fridays$Hours = format(as.POSIXct(Fridays$Time, format("%H:%M:%S")), "%H")
Friday_global_active <- aggregate(Fridays$Global_active_power, by = list(Fridays$Hours), mean, na.rm=TRUE)

names(Friday_global_active)[names(Friday_global_active) == "x"] <- "GlobalActivePower"
names(Friday_global_active)[names(Friday_global_active) == "Group.1"] <- "Time"


temp <- factor(Friday_global_active$Time)
a <- hm(as.character(temp))
Friday_global_active$logicalTime <- as.integer(hour(a))

# Saturdays
Saturdays <- df[df$day_of_the_week == 6,]
Saturdays$Hours = format(as.POSIXct(Saturdays$Time, format("%H:%M:%S")), "%H")
Saturday_global_active <- aggregate(Saturdays$Global_active_power, by = list(Saturdays$Hours), mean, na.rm=TRUE)

names(Saturday_global_active)[names(Saturday_global_active) == "x"] <- "GlobalActivePower"
names(Saturday_global_active)[names(Saturday_global_active) == "Group.1"] <- "Time"


temp <- factor(Saturday_global_active$Time)
a <- hm(as.character(temp))
Saturday_global_active$logicalTime <- as.integer(hour(a))

# Sundays
Sundays <- df[df$day_of_the_week == 7,]
Sundays$Hours = format(as.POSIXct(Sundays$Time, format("%H:%M:%S")), "%H")
Sunday_global_active <- aggregate(Sundays$Global_active_power, by = list(Sundays$Hours), mean, na.rm=TRUE)

names(Sunday_global_active)[names(Sunday_global_active) == "x"] <- "GlobalActivePower"
names(Sunday_global_active)[names(Sunday_global_active) == "Group.1"] <- "Time"


temp <- factor(Sunday_global_active$Time)
a <- hm(as.character(temp))
Sunday_global_active$logicalTime <- as.integer(hour(a))


plot1=ggplot() +
  geom_line(data=Monday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Monday"),size=1)+
  geom_line(data=Tuesday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Tuesday"),size=1)+
  geom_line(data=Wednesday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Wednesday"),size=1)+
  geom_line(data=Thursday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Thursday"),size=1)+
  geom_line(data=Friday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Friday"),size=1)+
  geom_line(data=Saturday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Saturday"),size=1)+
  geom_line(data=Sunday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Sunday"),size=1)+
  scale_color_discrete(breaks=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  geom_point(data=Monday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Tuesday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Wednesday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Thursday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Friday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Saturday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Sunday_global_active, aes(x=logicalTime, y=GlobalActivePower))


plot1+ggtitle("Aggregated Data Plot For Each Hour From 12am to 11:59pm For Years 2006-2009")





# Data aggregation and analysis for time interval with multiple observable patterns of interest (6am-10pm)
# Selected days: All days of the week
# Selected Years: 2006-2009

# Mondays
Mondays_seg = Mondays[hour(Mondays$Time) >= 6 & hour(Mondays$Time) < 22,]
Mondays_seg$Hours = format(as.POSIXct(Mondays_seg$Time, format("%H:%M:%S")), "%H")
Monday_global_active <- aggregate(Mondays_seg$Global_active_power, by = list(Mondays_seg$Hours), mean, na.rm=TRUE)

names(Monday_global_active)[names(Monday_global_active) == "x"] <- "GlobalActivePower"
names(Monday_global_active)[names(Monday_global_active) == "Group.1"] <- "Time"


temp <- factor(Monday_global_active$Time)
a <- hm(as.character(temp))
Monday_global_active$logicalTime <- as.integer(hour(a))

# Tuesdays
Tuesdays_seg = Tuesdays[hour(Tuesdays$Time) >= 6 & hour(Tuesdays$Time) < 22,]
Tuesdays_seg$Hours = format(as.POSIXct(Tuesdays_seg$Time, format("%H:%M:%S")), "%H")
Tuesday_global_active <- aggregate(Tuesdays_seg$Global_active_power, by = list(Tuesdays_seg$Hours), mean, na.rm=TRUE)

names(Tuesday_global_active)[names(Tuesday_global_active) == "x"] <- "GlobalActivePower"
names(Tuesday_global_active)[names(Tuesday_global_active) == "Group.1"] <- "Time"


temp <- factor(Tuesday_global_active$Time)
a <- hm(as.character(temp))
Tuesday_global_active$logicalTime <- as.integer(hour(a))

# Wednesdays
Wednesdays_seg = Wednesdays[hour(Wednesdays$Time) >= 6 & hour(Wednesdays$Time) < 22,]
Wednesdays_seg$Hours = format(as.POSIXct(Wednesdays_seg$Time, format("%H:%M:%S")), "%H")
Wednesday_global_active <- aggregate(Wednesdays_seg$Global_active_power, by = list(Wednesdays_seg$Hours), mean, na.rm=TRUE)

names(Wednesday_global_active)[names(Wednesday_global_active) == "x"] <- "GlobalActivePower"
names(Wednesday_global_active)[names(Wednesday_global_active) == "Group.1"] <- "Time"


temp <- factor(Wednesday_global_active$Time)
a <- hm(as.character(temp))
Wednesday_global_active$logicalTime <- as.integer(hour(a))

# Thursdays
Thursdays_seg = Thursdays[hour(Thursdays$Time) >= 6 & hour(Thursdays$Time) < 22,]
Thursdays_seg$Hours = format(as.POSIXct(Thursdays_seg$Time, format("%H:%M:%S")), "%H")
Thursday_global_active <- aggregate(Thursdays_seg$Global_active_power, by = list(Thursdays_seg$Hours), mean, na.rm=TRUE)

names(Thursday_global_active)[names(Thursday_global_active) == "x"] <- "GlobalActivePower"
names(Thursday_global_active)[names(Thursday_global_active) == "Group.1"] <- "Time"


temp <- factor(Thursday_global_active$Time)
a <- hm(as.character(temp))
Thursday_global_active$logicalTime <- as.integer(hour(a))

# Fridays
Fridays_seg = Fridays[hour(Fridays$Time) >= 6 & hour(Fridays$Time) < 22,]
Fridays_seg$Hours = format(as.POSIXct(Fridays_seg$Time, format("%H:%M:%S")), "%H")
Friday_global_active <- aggregate(Fridays_seg$Global_active_power, by = list(Fridays_seg$Hours), mean, na.rm=TRUE)

names(Friday_global_active)[names(Friday_global_active) == "x"] <- "GlobalActivePower"
names(Friday_global_active)[names(Friday_global_active) == "Group.1"] <- "Time"


temp <- factor(Friday_global_active$Time)
a <- hm(as.character(temp))
Friday_global_active$logicalTime <- as.integer(hour(a))

# Saturdays
Saturdays_seg = Saturdays[hour(Saturdays$Time) >= 6 & hour(Saturdays$Time) < 22,]
Saturdays_seg$Hours = format(as.POSIXct(Saturdays_seg$Time, format("%H:%M:%S")), "%H")
Saturday_global_active <- aggregate(Saturdays_seg$Global_active_power, by = list(Saturdays_seg$Hours), mean, na.rm=TRUE)

names(Saturday_global_active)[names(Saturday_global_active) == "x"] <- "GlobalActivePower"
names(Saturday_global_active)[names(Saturday_global_active) == "Group.1"] <- "Time"


temp <- factor(Saturday_global_active$Time)
a <- hm(as.character(temp))
Saturday_global_active$logicalTime <- as.integer(hour(a))

# Sundays
Sundays_seg = Sundays[hour(Sundays$Time) >= 6 & hour(Sundays$Time) < 22,]
Sundays_seg$Hours = format(as.POSIXct(Sundays_seg$Time, format("%H:%M:%S")), "%H")
Sunday_global_active <- aggregate(Sundays_seg$Global_active_power, by = list(Sundays_seg$Hours), mean, na.rm=TRUE)

names(Sunday_global_active)[names(Sunday_global_active) == "x"] <- "GlobalActivePower"
names(Sunday_global_active)[names(Sunday_global_active) == "Group.1"] <- "Time"


temp <- factor(Sunday_global_active$Time)
a <- hm(as.character(temp))
Sunday_global_active$logicalTime <- as.integer(hour(a))

plot2=ggplot() +
  geom_line(data=Monday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Monday"),size=1)+
  geom_line(data=Tuesday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Tuesday"),size=1)+
  geom_line(data=Wednesday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Wednesday"),size=1)+
  geom_line(data=Thursday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Thursday"),size=1)+
  geom_line(data=Friday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Friday"),size=1)+
  geom_line(data=Saturday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Saturday"),size=1)+
  geom_line(data=Sunday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Sunday"),size=1)+
  scale_color_discrete(breaks=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  geom_point(data=Monday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Tuesday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Wednesday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Thursday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Friday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Saturday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Sunday_global_active, aes(x=logicalTime, y=GlobalActivePower))


plot2+ggtitle("Aggregated Data Plot For Each Hour From 6am to 10pm For Years 2006-2009")





# Data aggregation and analysis for time interval with multiple observable patterns of interest (6am-10pm)
# Selected days: All days of the week

# Selected Year(s): 2006-2007
# Reason for combined years: 2006 provides only 2 weeks of data (from December 16th to December 31st)
#                            which is too limited to be analyzed
# Mondays
Mondays_2007 = Mondays[Mondays$Date >= "2006-12-16" & Mondays$Date <= "2007-12-31",]
Mondays_2007 = Mondays_2007[hour(Mondays_2007$Time) >= 6 & hour(Mondays_2007$Time) < 22,]
Mondays_2007$Hours = format(as.POSIXct(Mondays_2007$Time, format("%H:%M:%S")), "%H")
Monday_global_active <- aggregate(Mondays_2007$Global_active_power, by = list(Mondays_2007$Hours), mean, na.rm=TRUE)

names(Monday_global_active)[names(Monday_global_active) == "x"] <- "GlobalActivePower"
names(Monday_global_active)[names(Monday_global_active) == "Group.1"] <- "Time"


temp <- factor(Monday_global_active$Time)
a <- hm(as.character(temp))
Monday_global_active$logicalTime <- as.integer(hour(a))

# Tuesdays
Tuesdays_2007 = Tuesdays[Tuesdays$Date >= "2006-12-16" & Tuesdays$Date <= "2007-12-31",]
Tuesdays_2007 = Tuesdays_2007[hour(Tuesdays_2007$Time) >= 6 & hour(Tuesdays_2007$Time) < 22,]
Tuesdays_2007$Hours = format(as.POSIXct(Tuesdays_2007$Time, format("%H:%M:%S")), "%H")
Tuesday_global_active <- aggregate(Tuesdays_2007$Global_active_power, by = list(Tuesdays_2007$Hours), mean, na.rm=TRUE)

names(Tuesday_global_active)[names(Tuesday_global_active) == "x"] <- "GlobalActivePower"
names(Tuesday_global_active)[names(Tuesday_global_active) == "Group.1"] <- "Time"


temp <- factor(Tuesday_global_active$Time)
a <- hm(as.character(temp))
Tuesday_global_active$logicalTime <- as.integer(hour(a))

# Wednesdays
Wednesdays_2007 = Wednesdays[Wednesdays$Date >= "2006-12-16" & Wednesdays$Date <= "2007-12-31",]
Wednesdays_2007 = Wednesdays_2007[hour(Wednesdays_2007$Time) >= 6 & hour(Wednesdays_2007$Time) < 22,]
Wednesdays_2007$Hours = format(as.POSIXct(Wednesdays_2007$Time, format("%H:%M:%S")), "%H")
Wednesday_global_active <- aggregate(Wednesdays_2007$Global_active_power, by = list(Wednesdays_2007$Hours), mean, na.rm=TRUE)

names(Wednesday_global_active)[names(Wednesday_global_active) == "x"] <- "GlobalActivePower"
names(Wednesday_global_active)[names(Wednesday_global_active) == "Group.1"] <- "Time"


temp <- factor(Wednesday_global_active$Time)
a <- hm(as.character(temp))
Wednesday_global_active$logicalTime <- as.integer(hour(a))

# Thursdays
Thursdays_2007 = Thursdays[Thursdays$Date >= "2006-12-16" & Thursdays$Date <= "2007-12-31",]
Thursdays_2007 = Thursdays_2007[hour(Thursdays_2007$Time) >= 6 & hour(Thursdays_2007$Time) < 22,]
Thursdays_2007$Hours = format(as.POSIXct(Thursdays_2007$Time, format("%H:%M:%S")), "%H")
Thursday_global_active <- aggregate(Thursdays_2007$Global_active_power, by = list(Thursdays_2007$Hours), mean, na.rm=TRUE)

names(Thursday_global_active)[names(Thursday_global_active) == "x"] <- "GlobalActivePower"
names(Thursday_global_active)[names(Thursday_global_active) == "Group.1"] <- "Time"


temp <- factor(Thursday_global_active$Time)
a <- hm(as.character(temp))
Thursday_global_active$logicalTime <- as.integer(hour(a))

# Fridays
Fridays_2007 = Fridays[Fridays$Date >= "2006-12-16" & Fridays$Date <= "2007-12-31",]
Fridays_2007 = Fridays_2007[hour(Fridays_2007$Time) >= 6 & hour(Fridays_2007$Time) < 22,]
Fridays_2007$Hours = format(as.POSIXct(Fridays_2007$Time, format("%H:%M:%S")), "%H")
Friday_global_active <- aggregate(Fridays_2007$Global_active_power, by = list(Fridays_2007$Hours), mean, na.rm=TRUE)

names(Friday_global_active)[names(Friday_global_active) == "x"] <- "GlobalActivePower"
names(Friday_global_active)[names(Friday_global_active) == "Group.1"] <- "Time"


temp <- factor(Friday_global_active$Time)
a <- hm(as.character(temp))
Friday_global_active$logicalTime <- as.integer(hour(a))

# Saturdays
Saturdays_2007 = Saturdays[Saturdays$Date >= "2006-12-16" & Saturdays$Date <= "2007-12-31",]
Saturdays_2007 = Saturdays_2007[hour(Saturdays_2007$Time) >= 6 & hour(Saturdays_2007$Time) < 22,]
Saturdays_2007$Hours = format(as.POSIXct(Saturdays_2007$Time, format("%H:%M:%S")), "%H")
Saturday_global_active <- aggregate(Saturdays_2007$Global_active_power, by = list(Saturdays_2007$Hours), mean, na.rm=TRUE)

names(Saturday_global_active)[names(Saturday_global_active) == "x"] <- "GlobalActivePower"
names(Saturday_global_active)[names(Saturday_global_active) == "Group.1"] <- "Time"


temp <- factor(Saturday_global_active$Time)
a <- hm(as.character(temp))
Saturday_global_active$logicalTime <- as.integer(hour(a))


# Sundays
Sundays <- df[df$day_of_the_week == 7,]
Sundays_2007 = Sundays[Sundays$Date >= "2006-12-16" & Sundays$Date <= "2007-12-31",]
Sundays_2007 = Sundays_2007[hour(Sundays_2007$Time) >= 6 & hour(Sundays_2007$Time) < 22,]
Sundays_2007$Hours = format(as.POSIXct(Sundays_2007$Time, format("%H:%M:%S")), "%H")
Sunday_global_active <- aggregate(Sundays_2007$Global_active_power, by = list(Sundays_2007$Hours), mean, na.rm=TRUE)

names(Sunday_global_active)[names(Sunday_global_active) == "x"] <- "GlobalActivePower"
names(Sunday_global_active)[names(Sunday_global_active) == "Group.1"] <- "Time"


temp <- factor(Sunday_global_active$Time)
a <- hm(as.character(temp))
Sunday_global_active$logicalTime <- as.integer(hour(a))


plot3=ggplot() +
  geom_line(data=Monday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Monday"),size=1)+
  geom_line(data=Tuesday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Tuesday"),size=1)+
  geom_line(data=Wednesday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Wednesday"),size=1)+
  geom_line(data=Thursday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Thursday"),size=1)+
  geom_line(data=Friday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Friday"),size=1)+
  geom_line(data=Saturday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Saturday"),size=1)+
  geom_line(data=Sunday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Sunday"),size=1)+
  scale_color_discrete(breaks=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  geom_point(data=Monday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Tuesday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Wednesday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Thursday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Friday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Saturday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Sunday_global_active, aes(x=logicalTime, y=GlobalActivePower))

plot3+ggtitle("Aggregated Data Plot For Each Hour From 6am to 10pm For Years 2006-2007")


# Selected Year: 2008
# Mondays
Mondays_2008 = Mondays[Mondays$Date >= "2008-01-01" & Mondays$Date <= "2008-12-31",]
Mondays_2008 = Mondays_2008[hour(Mondays_2008$Time) >= 6 & hour(Mondays_2008$Time) < 22,]
Mondays_2008$Hours = format(as.POSIXct(Mondays_2008$Time, format("%H:%M:%S")), "%H")
Monday_global_active <- aggregate(Mondays_2008$Global_active_power, by = list(Mondays_2008$Hours), mean, na.rm=TRUE)

names(Monday_global_active)[names(Monday_global_active) == "x"] <- "GlobalActivePower"
names(Monday_global_active)[names(Monday_global_active) == "Group.1"] <- "Time"


temp <- factor(Monday_global_active$Time)
a <- hm(as.character(temp))
Monday_global_active$logicalTime <- as.integer(hour(a))

# Tuesdays
Tuesdays_2008 = Tuesdays[Tuesdays$Date >= "2008-01-01" & Tuesdays$Date <= "2008-12-31",]
Tuesdays_2008 = Tuesdays_2008[hour(Tuesdays_2008$Time) >= 6 & hour(Tuesdays_2008$Time) < 22,]
Tuesdays_2008$Hours = format(as.POSIXct(Tuesdays_2008$Time, format("%H:%M:%S")), "%H")
Tuesday_global_active <- aggregate(Tuesdays_2008$Global_active_power, by = list(Tuesdays_2008$Hours), mean, na.rm=TRUE)

names(Tuesday_global_active)[names(Tuesday_global_active) == "x"] <- "GlobalActivePower"
names(Tuesday_global_active)[names(Tuesday_global_active) == "Group.1"] <- "Time"


temp <- factor(Tuesday_global_active$Time)
a <- hm(as.character(temp))
Tuesday_global_active$logicalTime <- as.integer(hour(a))

# Wednesdays
Wednesdays_2008 = Wednesdays[Wednesdays$Date >= "2008-01-01" & Wednesdays$Date <= "2008-12-31",]
Wednesdays_2008 = Wednesdays_2008[hour(Wednesdays_2008$Time) >= 6 & hour(Wednesdays_2008$Time) < 22,]
Wednesdays_2008$Hours = format(as.POSIXct(Wednesdays_2008$Time, format("%H:%M:%S")), "%H")
Wednesday_global_active <- aggregate(Wednesdays_2008$Global_active_power, by = list(Wednesdays_2008$Hours), mean, na.rm=TRUE)

names(Wednesday_global_active)[names(Wednesday_global_active) == "x"] <- "GlobalActivePower"
names(Wednesday_global_active)[names(Wednesday_global_active) == "Group.1"] <- "Time"


temp <- factor(Wednesday_global_active$Time)
a <- hm(as.character(temp))
Wednesday_global_active$logicalTime <- as.integer(hour(a))

# Thursdays
Thursdays_2008 = Thursdays[Thursdays$Date >= "2008-01-01" & Thursdays$Date <= "2008-12-31",]
Thursdays_2008 = Thursdays_2008[hour(Thursdays_2008$Time) >= 6 & hour(Thursdays_2008$Time) < 22,]
Thursdays_2008$Hours = format(as.POSIXct(Thursdays_2008$Time, format("%H:%M:%S")), "%H")
Thursday_global_active <- aggregate(Thursdays_2008$Global_active_power, by = list(Thursdays_2008$Hours), mean, na.rm=TRUE)

names(Thursday_global_active)[names(Thursday_global_active) == "x"] <- "GlobalActivePower"
names(Thursday_global_active)[names(Thursday_global_active) == "Group.1"] <- "Time"


temp <- factor(Thursday_global_active$Time)
a <- hm(as.character(temp))
Thursday_global_active$logicalTime <- as.integer(hour(a))

# Fridays
Fridays_2008 = Fridays[Fridays$Date >= "2008-01-01" & Fridays$Date <= "2008-12-31",]
Fridays_2008 = Fridays_2008[hour(Fridays_2008$Time) >= 6 & hour(Fridays_2008$Time) < 22,]
Fridays_2008$Hours = format(as.POSIXct(Fridays_2008$Time, format("%H:%M:%S")), "%H")
Friday_global_active <- aggregate(Fridays_2008$Global_active_power, by = list(Fridays_2008$Hours), mean, na.rm=TRUE)

names(Friday_global_active)[names(Friday_global_active) == "x"] <- "GlobalActivePower"
names(Friday_global_active)[names(Friday_global_active) == "Group.1"] <- "Time"


temp <- factor(Friday_global_active$Time)
a <- hm(as.character(temp))
Friday_global_active$logicalTime <- as.integer(hour(a))

# Saturdays
Saturdays_2008 = Saturdays[Saturdays$Date >= "2008-01-01" & Saturdays$Date <= "2008-12-31",]
Saturdays_2008 = Saturdays_2008[hour(Saturdays_2008$Time) >= 6 & hour(Saturdays_2008$Time) < 22,]
Saturdays_2008$Hours = format(as.POSIXct(Saturdays_2008$Time, format("%H:%M:%S")), "%H")
Saturday_global_active <- aggregate(Saturdays_2008$Global_active_power, by = list(Saturdays_2008$Hours), mean, na.rm=TRUE)

names(Saturday_global_active)[names(Saturday_global_active) == "x"] <- "GlobalActivePower"
names(Saturday_global_active)[names(Saturday_global_active) == "Group.1"] <- "Time"


temp <- factor(Saturday_global_active$Time)
a <- hm(as.character(temp))
Saturday_global_active$logicalTime <- as.integer(hour(a))

# Sundays
Sundays_2008 = Sundays[Sundays$Date >= "2008-01-01" & Sundays$Date <= "2008-12-31",]
Sundays_2008 = Sundays_2008[hour(Sundays_2008$Time) >= 6 & hour(Sundays_2008$Time) < 22,]
Sundays_2008$Hours = format(as.POSIXct(Sundays_2008$Time, format("%H:%M:%S")), "%H")
Sunday_global_active <- aggregate(Sundays_2008$Global_active_power, by = list(Sundays_2008$Hours), mean, na.rm=TRUE)

names(Sunday_global_active)[names(Sunday_global_active) == "x"] <- "GlobalActivePower"
names(Sunday_global_active)[names(Sunday_global_active) == "Group.1"] <- "Time"


temp <- factor(Sunday_global_active$Time)
a <- hm(as.character(temp))
Sunday_global_active$logicalTime <- as.integer(hour(a))


plot4=ggplot() +
  geom_line(data=Monday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Monday"),size=1)+
  geom_line(data=Tuesday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Tuesday"),size=1)+
  geom_line(data=Wednesday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Wednesday"),size=1)+
  geom_line(data=Thursday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Thursday"),size=1)+
  geom_line(data=Friday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Friday"),size=1)+
  geom_line(data=Saturday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Saturday"),size=1)+
  geom_line(data=Sunday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Sunday"),size=1)+
  scale_color_discrete(breaks=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  geom_point(data=Monday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Tuesday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Wednesday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Thursday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Friday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Saturday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Sunday_global_active, aes(x=logicalTime, y=GlobalActivePower))

plot4+ggtitle("Aggregated Data Plot For Each Hour From 6am to 10pm For Year 2008")


# Year: 2009
# Mondays
Mondays_2009 = Mondays[Mondays$Date >= "2009-01-01" & Mondays$Date <= "2009-12-01",]
Mondays_2009 = Mondays_2009[hour(Mondays_2009$Time) >= 6 & hour(Mondays_2009$Time) < 22,]
Mondays_2009$Hours = format(as.POSIXct(Mondays_2009$Time, format("%H:%M:%S")), "%H")
Monday_global_active <- aggregate(Mondays_2009$Global_active_power, by = list(Mondays_2009$Hours), mean, na.rm=TRUE)

names(Monday_global_active)[names(Monday_global_active) == "x"] <- "GlobalActivePower"
names(Monday_global_active)[names(Monday_global_active) == "Group.1"] <- "Time"


temp <- factor(Monday_global_active$Time)
a <- hm(as.character(temp))
Monday_global_active$logicalTime <- as.integer(hour(a))

#Tuesdays
Tuesdays_2009 = Tuesdays[Tuesdays$Date >= "2009-01-01" & Tuesdays$Date <= "2009-12-01",]
Tuesdays_2009 = Tuesdays_2009[hour(Tuesdays_2009$Time) >= 6 & hour(Tuesdays_2009$Time) < 22,]
Tuesdays_2009$Hours = format(as.POSIXct(Tuesdays_2009$Time, format("%H:%M:%S")), "%H")
Tuesday_global_active <- aggregate(Tuesdays_2009$Global_active_power, by = list(Tuesdays_2009$Hours), mean, na.rm=TRUE)

names(Tuesday_global_active)[names(Tuesday_global_active) == "x"] <- "GlobalActivePower"
names(Tuesday_global_active)[names(Tuesday_global_active) == "Group.1"] <- "Time"


temp <- factor(Tuesday_global_active$Time)
a <- hm(as.character(temp))
Tuesday_global_active$logicalTime <- as.integer(hour(a))

#Wednesday
Wednesdays_2009 = Wednesdays[Wednesdays$Date >= "2009-01-01" & Wednesdays$Date <= "2009-12-01",]
Wednesdays_2009 = Wednesdays_2009[hour(Wednesdays_2009$Time) >= 6 & hour(Wednesdays_2009$Time) < 22,]
Wednesdays_2009$Hours = format(as.POSIXct(Wednesdays_2009$Time, format("%H:%M:%S")), "%H")
Wednesday_global_active <- aggregate(Wednesdays_2009$Global_active_power, by = list(Wednesdays_2009$Hours), mean, na.rm=TRUE)

names(Wednesday_global_active)[names(Wednesday_global_active) == "x"] <- "GlobalActivePower"
names(Wednesday_global_active)[names(Wednesday_global_active) == "Group.1"] <- "Time"


temp <- factor(Wednesday_global_active$Time)
a <- hm(as.character(temp))
Wednesday_global_active$logicalTime <- as.integer(hour(a))

#Thursdays
Thursdays_2009 = Thursdays[Thursdays$Date >= "2009-01-01" & Thursdays$Date <= "2009-12-01",]
Thursdays_2009 = Thursdays_2009[hour(Thursdays_2009$Time) >= 6 & hour(Thursdays_2009$Time) < 22,]
Thursdays_2009$Hours = format(as.POSIXct(Thursdays_2009$Time, format("%H:%M:%S")), "%H")
Thursday_global_active <- aggregate(Thursdays_2009$Global_active_power, by = list(Thursdays_2009$Hours), mean, na.rm=TRUE)

names(Thursday_global_active)[names(Thursday_global_active) == "x"] <- "GlobalActivePower"
names(Thursday_global_active)[names(Thursday_global_active) == "Group.1"] <- "Time"


temp <- factor(Thursday_global_active$Time)
a <- hm(as.character(temp))
Thursday_global_active$logicalTime <- as.integer(hour(a))

#Fridays
Fridays_2009 = Fridays[Fridays$Date >= "2009-01-01" & Fridays$Date <= "2009-12-01",]
Fridays_2009 = Fridays_2009[hour(Fridays_2009$Time) >= 6 & hour(Fridays_2009$Time) < 22,]
Fridays_2009$Hours = format(as.POSIXct(Fridays_2009$Time, format("%H:%M:%S")), "%H")
Friday_global_active <- aggregate(Fridays_2009$Global_active_power, by = list(Fridays_2009$Hours), mean, na.rm=TRUE)

names(Friday_global_active)[names(Friday_global_active) == "x"] <- "GlobalActivePower"
names(Friday_global_active)[names(Friday_global_active) == "Group.1"] <- "Time"


temp <- factor(Friday_global_active$Time)
a <- hm(as.character(temp))
Friday_global_active$logicalTime <- as.integer(hour(a))

#Saturdays
Saturdays_2009 = Saturdays[Saturdays$Date >= "2009-01-01" & Saturdays$Date <= "2009-12-01",]
Saturdays_2009 = Saturdays_2009[hour(Saturdays_2009$Time) >= 6 & hour(Saturdays_2009$Time) < 22,]
Saturdays_2009$Hours = format(as.POSIXct(Saturdays_2009$Time, format("%H:%M:%S")), "%H")
Saturday_global_active <- aggregate(Saturdays_2009$Global_active_power, by = list(Saturdays_2009$Hours), mean, na.rm=TRUE)

names(Saturday_global_active)[names(Saturday_global_active) == "x"] <- "GlobalActivePower"
names(Saturday_global_active)[names(Saturday_global_active) == "Group.1"] <- "Time"


temp <- factor(Saturday_global_active$Time)
a <- hm(as.character(temp))
Saturday_global_active$logicalTime <- as.integer(hour(a))

# Sundays
Sundays_2009 = Sundays[Sundays$Date >= "2009-01-01" & Sundays$Date <= "2009-12-01",]
Sundays_2009 = Sundays_2009[hour(Sundays_2009$Time) >= 6 & hour(Sundays_2009$Time) < 22,]
Sundays_2009$Hours = format(as.POSIXct(Sundays_2009$Time, format("%H:%M:%S")), "%H")
Sunday_global_active <- aggregate(Sundays_2009$Global_active_power, by = list(Sundays_2009$Hours), mean, na.rm=TRUE)

names(Sunday_global_active)[names(Sunday_global_active) == "x"] <- "GlobalActivePower"
names(Sunday_global_active)[names(Sunday_global_active) == "Group.1"] <- "Time"


temp <- factor(Sunday_global_active$Time)
a <- hm(as.character(temp))
Sunday_global_active$logicalTime <- as.integer(hour(a))


plot5=ggplot() +
  geom_line(data=Monday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Monday"),size=1)+
  geom_line(data=Tuesday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Tuesday"),size=1)+
  geom_line(data=Wednesday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Wednesday"),size=1)+
  geom_line(data=Thursday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Thursday"),size=1)+
  geom_line(data=Friday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Friday"),size=1)+
  geom_line(data=Saturday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Saturday"),size=1)+
  geom_line(data=Sunday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Sunday"),size=1)+
  scale_color_discrete(breaks=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  geom_point(data=Monday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Tuesday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Wednesday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Thursday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Friday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Saturday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Sunday_global_active, aes(x=logicalTime, y=GlobalActivePower))

plot5+ggtitle("Aggregated Data Plot For Each Hour From 6am to 10pm For Year 2009") 





# Based on analysis of observations on different time intervals within the 4-year aggregated data plot
# and the aggregated data plots for each year, we selected 2 different time intervals on 3 weekdays and 1 weekend day
# as the observed behaviour of the global_active_power data follow a general increasing/decreasing pattern
# within the given time frames on all 3 year-by-year graphs and the combined 4-year graph.
# For clarity and conciseness, the aggregations and graph below represent the 4-year aggregated data.

# First set of analysis for data exploration: (Sample) weekdays and weekend days
# Selected Years: 2006-2009
# Selected days: Mondays, Wednesdays and Fridays for weekdays and Sundays for weekend days
# First Selected Time Interval for analysis (based on data exploration): 9am-1pm
# Reason for selection: global_active_power follows a (generally) decreasing trend on weekdays (analyzed by the hour)
#                       and an increasing trend on weekends (again, analyzed by the hour)

# Mondays
Mondays_seg = Mondays[hour(Mondays$Time) >= 9 & hour(Mondays$Time) <= 12,]
Mondays_seg$Hours = format(as.POSIXct(Mondays_seg$Time, format("%H:%M:%S")), "%H")
Monday_global_active <- aggregate(Mondays_seg$Global_active_power, by = list(Mondays_seg$Hours), mean, na.rm=TRUE)

names(Monday_global_active)[names(Monday_global_active) == "x"] <- "GlobalActivePower"
names(Monday_global_active)[names(Monday_global_active) == "Group.1"] <- "Time"


temp <- factor(Monday_global_active$Time)
a <- hm(as.character(temp))
Monday_global_active$logicalTime <- as.integer(hour(a))

# Wednesdays
Wednesdays_seg = Wednesdays[hour(Wednesdays$Time) >= 9 & hour(Wednesdays$Time) <= 12,]
Wednesdays_seg$Hours = format(as.POSIXct(Wednesdays_seg$Time, format("%H:%M:%S")), "%H")
Wednesday_global_active <- aggregate(Wednesdays_seg$Global_active_power, by = list(Wednesdays_seg$Hours), mean, na.rm=TRUE)

names(Wednesday_global_active)[names(Wednesday_global_active) == "x"] <- "GlobalActivePower"
names(Wednesday_global_active)[names(Wednesday_global_active) == "Group.1"] <- "Time"


temp <- factor(Wednesday_global_active$Time)
a <- hm(as.character(temp))
Wednesday_global_active$logicalTime <- as.integer(hour(a))

# Fridays
Fridays_seg = Fridays[hour(Fridays$Time) >= 9 & hour(Fridays$Time) <= 12,]
Fridays_seg$Hours = format(as.POSIXct(Fridays_seg$Time, format("%H:%M:%S")), "%H")
Friday_global_active <- aggregate(Fridays_seg$Global_active_power, by = list(Fridays_seg$Hours), mean, na.rm=TRUE)

names(Friday_global_active)[names(Friday_global_active) == "x"] <- "GlobalActivePower"
names(Friday_global_active)[names(Friday_global_active) == "Group.1"] <- "Time"


temp <- factor(Friday_global_active$Time)
a <- hm(as.character(temp))
Friday_global_active$logicalTime <- as.integer(hour(a))

# Sundays
Sundays_seg = Sundays[hour(Sundays$Time) >= 9 & hour(Sundays$Time) <= 12,]
Sundays_seg$Hours = format(as.POSIXct(Sundays_seg$Time, format("%H:%M:%S")), "%H")
Sunday_global_active <- aggregate(Sundays_seg$Global_active_power, by = list(Sundays_seg$Hours), mean, na.rm=TRUE)

names(Sunday_global_active)[names(Sunday_global_active) == "x"] <- "GlobalActivePower"
names(Sunday_global_active)[names(Sunday_global_active) == "Group.1"] <- "Time"


temp <- factor(Sunday_global_active$Time)
a <- hm(as.character(temp))
Sunday_global_active$logicalTime <- as.integer(hour(a))

plot6=ggplot() +
  geom_line(data=Monday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Monday"),size=1)+
  geom_line(data=Wednesday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Wednesday"),size=1)+
  geom_line(data=Friday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Friday"),size=1)+
  geom_line(data=Sunday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Sunday"),size=1)+
  scale_color_discrete(breaks=c("Monday","Wednesday","Friday","Sunday"))+
  geom_point(data=Monday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Wednesday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Friday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Sunday_global_active, aes(x=logicalTime, y=GlobalActivePower))


plot6+ggtitle("Hourly Aggregated Data Plot For Mon/Wed/Fri/Sun, 9am-1pm, Years 2006-2009")



# Second set of analysis for data exploration: (Sample) weeknights and weekend nights
# Selected Years: 2006-2009
# Selected days: Mondays, Wednesdays and Fridays for weeknights and Sundays for weekend nights
# Second Selected Time Interval for analysis (based on data exploration): 6pm-10pm
# Reason for selection: global_active_power follows a (generally) increasing trend on both
#                       weeknights and weekend nights (analyzed by the hour)

# Mondays
Mondays_seg = Mondays[hour(Mondays$Time) >= 18 & hour(Mondays$Time) <= 21,]
Mondays_seg$Hours = format(as.POSIXct(Mondays_seg$Time, format("%H:%M:%S")), "%H")
Monday_global_active <- aggregate(Mondays_seg$Global_active_power, by = list(Mondays_seg$Hours), mean, na.rm=TRUE)

names(Monday_global_active)[names(Monday_global_active) == "x"] <- "GlobalActivePower"
names(Monday_global_active)[names(Monday_global_active) == "Group.1"] <- "Time"


temp <- factor(Monday_global_active$Time)
a <- hm(as.character(temp))
Monday_global_active$logicalTime <- as.integer(hour(a))

# Wednesdays
Wednesdays_seg = Wednesdays[hour(Wednesdays$Time) >= 18 & hour(Wednesdays$Time) <= 21,]
Wednesdays_seg$Hours = format(as.POSIXct(Wednesdays_seg$Time, format("%H:%M:%S")), "%H")
Wednesday_global_active <- aggregate(Wednesdays_seg$Global_active_power, by = list(Wednesdays_seg$Hours), mean, na.rm=TRUE)

names(Wednesday_global_active)[names(Wednesday_global_active) == "x"] <- "GlobalActivePower"
names(Wednesday_global_active)[names(Wednesday_global_active) == "Group.1"] <- "Time"


temp <- factor(Wednesday_global_active$Time)
a <- hm(as.character(temp))
Wednesday_global_active$logicalTime <- as.integer(hour(a))

# Fridays
Fridays_seg = Fridays[hour(Fridays$Time) >= 18 & hour(Fridays$Time) <= 21,]
Fridays_seg$Hours = format(as.POSIXct(Fridays_seg$Time, format("%H:%M:%S")), "%H")
Friday_global_active <- aggregate(Fridays_seg$Global_active_power, by = list(Fridays_seg$Hours), mean, na.rm=TRUE)

names(Friday_global_active)[names(Friday_global_active) == "x"] <- "GlobalActivePower"
names(Friday_global_active)[names(Friday_global_active) == "Group.1"] <- "Time"


temp <- factor(Friday_global_active$Time)
a <- hm(as.character(temp))
Friday_global_active$logicalTime <- as.integer(hour(a))

# Sundays
Sundays_seg = Sundays[hour(Sundays$Time) >= 18 & hour(Sundays$Time) <= 21,]
Sundays_seg$Hours = format(as.POSIXct(Sundays_seg$Time, format("%H:%M:%S")), "%H")
Sunday_global_active <- aggregate(Sundays_seg$Global_active_power, by = list(Sundays_seg$Hours), mean, na.rm=TRUE)

names(Sunday_global_active)[names(Sunday_global_active) == "x"] <- "GlobalActivePower"
names(Sunday_global_active)[names(Sunday_global_active) == "Group.1"] <- "Time"


temp <- factor(Sunday_global_active$Time)
a <- hm(as.character(temp))
Sunday_global_active$logicalTime <- as.integer(hour(a))

plot7=ggplot() +
  geom_line(data=Monday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Monday"),size=1)+
  geom_line(data=Wednesday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Wednesday"),size=1)+
  geom_line(data=Friday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Friday"),size=1)+
  geom_line(data=Sunday_global_active, aes(x=logicalTime, y=GlobalActivePower,col="Sunday"),size=1)+
  scale_color_discrete(breaks=c("Monday","Wednesday","Friday","Sunday"))+
  geom_point(data=Monday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Wednesday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Friday_global_active, aes(x=logicalTime, y=GlobalActivePower))+
  geom_point(data=Sunday_global_active, aes(x=logicalTime, y=GlobalActivePower))


plot7+ggtitle("Hourly Aggregated Data Plot For Mon/Wed/Fri/Sun, 6pm-10pm, Years 2006-2009")

# As the variation in activities during night time varies for every household, we selected 9am-1pm during daytime
# as our single observation time window during weekdays and weekend days. The selected time window generally corresponds
# to part of work/school hours during weekdays as well as waking-up and leisure routines on weekends.
# For the specific weekday, we selected Mondays for further data analysis, HMM modeling and anomaly detection.


# Mondays
Mondays_seg = Mondays[hour(Mondays$Time) >= 9 & hour(Mondays$Time) <= 12,]
Mondays_seg$Minute = format(as.POSIXct(Mondays_seg$Time, format("%H:%M:%S")), "%H:%M")
Monday_global_active <- aggregate(Mondays_seg$Global_active_power, by = list(Mondays_seg$Minute), mean, na.rm=TRUE)

names(Monday_global_active)[names(Monday_global_active) == "x"] <- "AvgGlobalActivePower_Weekdays"
names(Monday_global_active)[names(Monday_global_active) == "Group.1"] <- "Time"


temp <- factor(Monday_global_active$Time)
a <- hm(as.character(temp))
Monday_global_active$logicalTime <- as.integer(60*hour(a)+minute(a))

plot8=ggplot(data=Monday_global_active, mapping=aes(x=logicalTime, y=AvgGlobalActivePower_Weekdays)) +
  geom_point(color="red")
plot8+ggtitle("Aggregated Data Plot By Minutes For Mondays From 9am to 1pm For Years 2006-2009")


# Sundays
Sundays_seg = Sundays[hour(Sundays$Time) >= 9 & hour(Sundays$Time) <= 12,]
Sundays_seg$Minute = format(as.POSIXct(Sundays_seg$Time, format("%H:%M:%S")), "%H:%M")
Sunday_global_active <- aggregate(Sundays_seg$Global_active_power, by = list(Sundays_seg$Minute), mean, na.rm=TRUE)

names(Sunday_global_active)[names(Sunday_global_active) == "x"] <- "AvgGlobalActivePower_Weekends"
names(Sunday_global_active)[names(Sunday_global_active) == "Group.1"] <- "Time"


temp <- factor(Sunday_global_active$Time)
a <- hm(as.character(temp))
Sunday_global_active$logicalTime <- as.integer(60*hour(a)+minute(a))

plot8=ggplot(data=Sunday_global_active, mapping=aes(x=logicalTime, y=AvgGlobalActivePower_Weekends)) +
  geom_point(color="blue")
plot8+ggtitle("Aggregated Data Plot By Minutes For Sundays From 9am to 1pm For Years 2006-2009")
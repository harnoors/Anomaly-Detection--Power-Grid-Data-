library("depmixS4")
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)

df=read.table('TrainData.txt', header = TRUE, sep = ",", dec = ".")
a=dmy(df$Date)
df$year=year(a)
df$year= as.integer(df$year)

df$Date <- as.POSIXct(df$Date, format = "%d/%m/%Y") 
df$Time <- as.POSIXct(df$Time, format = "%H:%M:%S") 

d=strftime(df$Date, format = "%u")
df$day_of_the_week <- as.integer(d)

# Task 3: Training and Testing
# Selected Weekday: Mondays
Weekdays <- df[df$day_of_the_week == 1 ,]

# Weekday and weekend time window: 9am-1pm (4 hours)
Weekdayhours = Weekdays[hour(Weekdays$Time) >= 9 & hour(Weekdays$Time) < 13,]

# Model Training
# Taking approximately 1/5 of the given dataset as the test data,
# We select December 16th, 2016 to April 30th, 2009 as our training dataset
# and use the rest of it to test our HMM models
trainingdata_Weekdays <- Weekdayhours[Weekdayhours$Date >= "2006-12-16" & Weekdayhours$Date <= "2009-4-30",]
trainingdata_Weekends <- Weekendhours[Weekendhours$Date >= "2006-12-16" & Weekendhours$Date <= "2009-4-30",]

# No. of weekday time frame repetitions within the given date range
# 240 minutes/day, 124 Mondays within the date range
numWeekdayHours = rep(240, 124)


# Univariate HMM Models for Weekdays data
# N-state range: 5-15 (to determine the most suitable model based on log-Likelihood and BIC values)
wkuni1 <- depmix(Global_active_power~1,data=trainingdata_Weekdays,
                 nstates=5,ntimes=numWeekdayHours,family=gaussian())
wkuni_model1 <- fit(wkuni1)
BIC(wkuni_model1)


wkuni2 <- depmix(Global_active_power~1,data=trainingdata_Weekdays,
                 nstates=6,ntimes=numWeekdayHours,family=gaussian())
wkuni_model2 <- fit(wkuni2)
BIC(wkuni_model2)


wkuni3 <- depmix(Global_active_power~1,data=trainingdata_Weekdays,
                 nstates=7,ntimes=numWeekdayHours,family=gaussian())
wkuni_model3 <- fit(wkuni3)
BIC(wkuni_model3)


wkuni4 <- depmix(Global_active_power~1,data=trainingdata_Weekdays,
                 nstates=8,ntimes=numWeekdayHours,family=gaussian())
wkuni_model4 <- fit(wkuni4)
BIC(wkuni_model4)


wkuni5 <- depmix(Global_active_power~1,data=trainingdata_Weekdays,
                 nstates=9,ntimes=numWeekdayHours,family=gaussian())
wkuni_model5 <- fit(wkuni5)
BIC(wkuni_model5)


wkuni6 <- depmix(Global_active_power~1,data=trainingdata_Weekdays,
                 nstates=10,ntimes=numWeekdayHours,family=gaussian())
wkuni_model6 <- fit(wkuni6)
BIC(wkuni_model6)


wkuni7 <- depmix(Global_active_power~1,data=trainingdata_Weekdays,
                 nstates=11,ntimes=numWeekdayHours,family=gaussian())
wkuni_model7 <- fit(wkuni7)
BIC(wkuni_model7)


wkuni8 <- depmix(Global_active_power~1,data=trainingdata_Weekdays,
                 nstates=12,ntimes=numWeekdayHours,family=gaussian())
wkuni_model8 <- fit(wkuni8)
BIC(wkuni_model8)


wkuni9 <- depmix(Global_active_power~1,data=trainingdata_Weekdays,
                 nstates=13,ntimes=numWeekdayHours,family=gaussian())
wkuni_model9 <- fit(wkuni9)
BIC(wkuni_model9)


wkuni10 <- depmix(Global_active_power~1,data=trainingdata_Weekdays,
                  nstates=14,ntimes=numWeekdayHours,family=gaussian())
wkuni_model10 <- fit(wkuni10)
BIC(wkuni_model10)


wkuni11 <- depmix(Global_active_power~1,data=trainingdata_Weekdays,
                  nstates=15,ntimes=numWeekdayHours,family=gaussian())
wkuni_model11 <- fit(wkuni11)
BIC(wkuni_model11)


df2=read.table('loglik.txt', header = TRUE, sep = ",", dec = ".")
df3=read.table('BIC.txt', header = TRUE, sep = ",", dec = ".")

plot1=ggplot() +
  geom_line(data=df2, aes(x=N_states, y=Value,col="LogLik"))+
  geom_line(data=df3, aes(x=N_states, y=Value,col="BIC")) + 
  geom_point(data=df2, aes(x=N_states, y=Value,col="LogLik"))+
  geom_point(data=df3, aes(x=N_states, y=Value,col="BIC"))

plot1+ggtitle("Log-likelihood and BIC Values for \n trained univariate HMM models (Weekdays), N_states: 5-15")+
  theme(plot.title = element_text(hjust = 0.5))

# Maximum log-likelihood and minimum BIC values are obtained when N-states= 7 (where log-likelihoods are negative)
# hence, 'wkuni_model5' is selected as the best univariate HMM model for the weekdays training data


######################################################################################################################


# Model Testing
testdata_Weekdays <- Weekdayhours[Weekdayhours$Date >= "2009-05-01" & Weekdayhours$Date <= "2009-12-01",]
testdata_weekends <- Weekendhours[Weekendhours$Date >= "2009-05-01" & Weekendhours$Date <= "2009-12-01",]

# No. of weekday time frame repetitions within the given date range
# 240 minutes/day, 31 Mondays within the date range
newNumWeekdayHours = rep(240, 31)

# Weekdays univariate model testing
NewWeekdayModel_uni <- depmix(response = Global_active_power~1,data=testdata_Weekdays,nstates=7,ntimes=newNumWeekdayHours)
NewWeekdayModel_uni <- setpars(NewWeekdayModel_uni, getpars(wkuni_model3))
logLik(NewWeekdayModel_uni)
logLik(wkuni_model3)

######################################################################################################################


# Task 4: Contextual Anomaly Detection
df_test1=read.table("./TestData/test1.txt", header = TRUE, sep = ",", dec = ".")
a=dmy(df_test1$Date)
df_test1$year=year(a)
df_test1$year= as.integer(df_test1$year)
d=strftime(df_test1$Date, format = "%u")
df_test1$day_of_the_week <- as.integer(d)

df_test1$Date <- as.POSIXct(df_test1$Date, format = "%d/%m/%Y") 
df_test1$Time <- as.POSIXct(df_test1$Time, format = "%H:%M:%S")
# Selected Weekday: Mondays
Weekdays_test1 <- df_test1[df_test1$day_of_the_week == 1 ,]
# Selected Window: 9am-1pm
Weekdays_test1 <- Weekdays_test1[hour(Weekdays_test1$Time) >= 9 & hour(Weekdays_test1$Time) < 13,]



df_test2=read.table("./TestData/test2.txt", header = TRUE, sep = ",", dec = ".")
a=dmy(df_test2$Date)
df_test2$year=year(a)
df_test2$year= as.integer(df_test2$year)
d=strftime(df_test2$Date, format = "%u")
df_test2$day_of_the_week <- as.integer(d)

df_test2$Date <- as.POSIXct(df_test2$Date, format = "%d/%m/%Y") 
df_test2$Time <- as.POSIXct(df_test2$Time, format = "%H:%M:%S")
# Selected Weekday: Mondays
Weekdays_test2 <- df_test2[df_test2$day_of_the_week == 1 ,]
# Selected Window: 9am-1pm
Weekdays_test2 <- Weekdays_test2[hour(Weekdays_test2$Time) >= 9 & hour(Weekdays_test2$Time) < 13,]



df_test3=read.table("./TestData/test3.txt", header = TRUE, sep = ",", dec = ".")
a=dmy(df_test3$Date)
df_test3$year=year(a)
df_test3$year= as.integer(df_test3$year)
d=strftime(df_test3$Date, format = "%u")
df_test3$day_of_the_week <- as.integer(d)

df_test3$Date <- as.POSIXct(df_test3$Date, format = "%d/%m/%Y") 
df_test3$Time <- as.POSIXct(df_test3$Time, format = "%H:%M:%S")
# Selected Weekday: Mondays
Weekdays_test3 <- df_test3[df_test3$day_of_the_week == 1 ,]
# Selected Window: 9am-1pm
Weekdays_test3 <- Weekdays_test3[hour(Weekdays_test3$Time) >= 9 & hour(Weekdays_test3$Time) < 13,]



df_test4=read.table("./TestData/test4.txt", header = TRUE, sep = ",", dec = ".")
a=dmy(df_test4$Date)
df_test4$year=year(a)
df_test4$year= as.integer(df_test4$year)
d=strftime(df_test4$Date, format = "%u")
df_test4$day_of_the_week <- as.integer(d)

df_test4$Date <- as.POSIXct(df_test4$Date, format = "%d/%m/%Y") 
df_test4$Time <- as.POSIXct(df_test4$Time, format = "%H:%M:%S")
# Selected Weekday: Mondays
Weekdays_test4 <- df_test4[df_test4$day_of_the_week == 1 ,]
# Selected Window: 9am-1pm
Weekdays_test4 <- Weekdays_test4[hour(Weekdays_test4$Time) >= 9 & hour(Weekdays_test4$Time) < 13,]



df_test5=read.table("./TestData/test5.txt", header = TRUE, sep = ",", dec = ".")
a=dmy(df_test5$Date)
df_test5$year=year(a)
df_test5$year= as.integer(df_test5$year)
d=strftime(df_test5$Date, format = "%u")
df_test5$day_of_the_week <- as.integer(d)

df_test5$Date <- as.POSIXct(df_test5$Date, format = "%d/%m/%Y") 
df_test5$Time <- as.POSIXct(df_test5$Time, format = "%H:%M:%S")
# Selected Weekday: Mondays
Weekdays_test5 <- df_test5[df_test5$day_of_the_week == 1 ,]
# Selected Window: 9am-1pm
Weekdays_test5 <- Weekdays_test5[hour(Weekdays_test5$Time) >= 9 & hour(Weekdays_test5$Time) < 13,]


# N-states of optimal Weekday Univariate model: 7
# No. of weekday time frame repetitions within the given date range
# 240 minutes/day, 51 Mondays within the date range
testNumReps_weekdays = rep(240, 51)


wkday_uni_test1 <- depmix(response = Global_active_power~1,data=Weekdays_test1,nstates=7,ntimes=testNumReps_weekdays)
wkday_uni_test1 <- setpars(wkday_uni_test1, getpars(wkuni3))
logLik(wkday_uni_test1)

wkday_uni_test2 <- depmix(response = Global_active_power~1,data=Weekdays_test2,nstates=7,ntimes=testNumReps_weekdays)
wkday_uni_test2 <- setpars(wkday_uni_test2, getpars(wkuni3))
logLik(wkday_uni_test2)

wkday_uni_test3 <- depmix(response = Global_active_power~1,data=Weekdays_test3,nstates=7,ntimes=testNumReps_weekdays)
wkday_uni_test3 <- setpars(wkday_uni_test3, getpars(wkuni3))
logLik(wkday_uni_test3)

wkday_uni_test4 <- depmix(response = Global_active_power~1,data=Weekdays_test4,nstates=7,ntimes=testNumReps_weekdays)
wkday_uni_test4 <- setpars(wkday_uni_test4, getpars(wkuni3))
logLik(wkday_uni_test4)

wkday_uni_test5 <- depmix(response = Global_active_power~1,data=Weekdays_test5,nstates=7,ntimes=testNumReps_weekdays)
wkday_uni_test5 <- setpars(wkday_uni_test5, getpars(wkuni3))
logLik(wkday_uni_test5)

######################################################################################################################
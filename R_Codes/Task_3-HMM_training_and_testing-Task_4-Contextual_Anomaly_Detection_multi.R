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
# Selected Weekend day: Sundays
Weekends <- df[df$day_of_the_week == 7,]

# Weekday and weekend time window: 9am-1pm (4 hours)
Weekdayhours = Weekdays[hour(Weekdays$Time) >= 9 & hour(Weekdays$Time) < 13,]
Weekendhours = Weekends[hour(Weekends$Time) >= 9 & hour(Weekends$Time) < 13,]

# Model Training
# Taking approximately 1/5 of the given dataset as the test data,
# We select December 16th, 2016 to April 30th, 2009 as our training dataset
# and use the rest of it to test our HMM models
trainingdata_Weekdays <- Weekdayhours[Weekdayhours$Date >= "2006-12-16" & Weekdayhours$Date <= "2009-4-30",]
trainingdata_Weekends <- Weekendhours[Weekendhours$Date >= "2006-12-16" & Weekendhours$Date <= "2009-4-30",]
numWeekdayHours = rep(240, 124)
numWeekendHours = rep(240, 124)

# NOTE: the training and testing of multivariate HMMs for weekdays and weekends are completed using the code below.
#       both HMM models are tested for 5-25 states, with appropriate values being used in each model for weekdays training
#       and subsituted for weekend days training. Graphs and summary of results for both models are included in the report.
BIC_array=array(data = 0, dim =25)


wkmulti0 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                   nstates=5,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model0 <- fit(wkmulti0)
BIC_array[1]<-BIC(wkmulti_model0)


wkmulti1 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                   nstates=6,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model1 <- fit(wkmulti1)
BIC_array[2]<-BIC(wkmulti_model1)


wkmulti2 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                   nstates=7,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model2 <- fit(wkmulti2)
BIC_array[3]<-BIC(wkmulti_model2)


wkmulti3 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                   nstates=8,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model3 <- fit(wkmulti3)
BIC_array[4]<-BIC(wkmulti_model3)


wkmulti4 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                   nstates=9,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model4 <- fit(wkmulti4)
BIC_array[5]<-BIC(wkmulti_model4)


wkmulti5 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                   nstates=10,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model5 <- fit(wkmulti5)
BIC_array[6]<-BIC(wkmulti_model5)


wkmulti6 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                   nstates=11,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model6 <- fit(wkmulti6)
BIC_array[7]<-BIC(wkmulti_model6)


wkmulti7 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                   nstates=12,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model7 <- fit(wkmulti7)
BIC_array[8]<-BIC(wkmulti_model7)


wkmulti8 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                   nstates=13,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model8 <- fit(wkmulti8)
BIC_array[9]<-BIC(wkmulti_model8)


wkmulti9 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                   nstates=14,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model9 <- fit(wkmulti9)
BIC_array[10]<-BIC(wkmulti_model9)


wkmulti10 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                    nstates=15,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model10 <- fit(wkmulti10)
BIC_array[11]<-BIC(wkmulti_model10)


wkmulti11 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                    nstates=16,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model11 <- fit(wkmulti11)
BIC_array[12]<-BIC(wkmulti_model11)


wkmulti12 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                    nstates=17,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model12 <- fit(wkmulti12)
BIC_array[13]<-BIC(wkmulti_model12)


wkmulti13 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                    nstates=18,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model13 <- fit(wkmulti13)
BIC_array[14]<-BIC(wkmulti_model13)


wkmulti14 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                    nstates=19,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model14 <- fit(wkmulti14)
BIC_array[15]<-BIC(wkmulti_model14)


wkmulti15 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                    nstates=20,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model15 <- fit(wkmulti15)
BIC_array[16]<-BIC(wkmulti_model15)


wkmulti16 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                    nstates=21,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model16 <- fit(wkmulti16)
BIC_array[17]<-BIC(wkmulti_model16)


wkmulti17 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                    nstates=22,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model17 <- fit(wkmulti17)
BIC_array[18]<-BIC(wkmulti_model17)


wkmulti18 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                    nstates=23,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model18 <- fit(wkmulti18)
BIC_array[19]<-BIC(wkmulti_model18)


wkmulti19 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                    nstates=24,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model19 <- fit(wkmulti19)
BIC_array[20]<-BIC(wkmulti_model19)


wkmulti20 <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainingdata_Weekdays,
                    nstates=25,ntimes=numWeekdayHours,family=list(gaussian(), gaussian()))
wkmulti_model20 <- fit(wkmulti20)
BIC_array[21]<-BIC(wkmulti_model20)


logLik_array=array(data = 0, dim =25)

logLik_array[1]<-logLik(wkmulti_model0)
logLik_array[2]<-logLik(wkmulti_model1)
logLik_array[3]<-logLik(wkmulti_model2)
logLik_array[4]<-logLik(wkmulti_model3)
logLik_array[5]<-logLik(wkmulti_model4)
logLik_array[6]<-logLik(wkmulti_model5)
logLik_array[7]<-logLik(wkmulti_model6)
logLik_array[8]<-logLik(wkmulti_model7)
logLik_array[9]<-logLik(wkmulti_model8)
logLik_array[10]<-logLik(wkmulti_model9)
logLik_array[11]<-logLik(wkmulti_model10)
logLik_array[12]<-logLik(wkmulti_model11)
logLik_array[13]<-logLik(wkmulti_model12)
logLik_array[14]<-logLik(wkmulti_model13)
logLik_array[15]<-logLik(wkmulti_model14)
logLik_array[16]<-logLik(wkmulti_model15)
logLik_array[17]<-logLik(wkmulti_model16)
logLik_array[18]<-logLik(wkmulti_model17)
logLik_array[19]<-logLik(wkmulti_model18)
logLik_array[20]<-logLik(wkmulti_model19)
logLik_array[21]<-logLik(wkmulti_model20)

Nstates=5:25
newframe= data.frame("states" = Nstates, "BIC" = BIC_array, "log" = logLik_array)
data <- melt(newframe, "states")
ggplot(data, aes(x=states, y=value, color=variable)) + geom_line() +geom_point()

################################

# Weekdays multivariate model testing

testdata_Weekdays <- Weekdayhours[Weekdayhours$Date >= "2009-05-01" & Weekdayhours$Date <= "2009-12-01",]
testdata_weekends <- Weekendhours[Weekendhours$Date >= "2009-05-01" & Weekendhours$Date <= "2009-12-01",]

#N=25 STATES
NewWeekdayModel_multi <- depmix(list(Global_active_power~1, Global_intensity~1),data=testdata_Weekdays,
                                nstates=25,ntimes=newNumWeekdayHours,family=list(gaussian(), gaussian()))
NewWeekdayModel_multi <- setpars(NewWeekdayModel_multi, getpars(wkmulti_model20))
logLik(NewWeekdayModel_multi)
loglika=logLik(wkmulti_model20)
loglika/4

#N=24 STATES
NewWeekdayModel_multi <- depmix(list(Global_active_power~1, Global_intensity~1),data=testdata_Weekdays,
                                nstates=24,ntimes=newNumWeekdayHours,family=list(gaussian(), gaussian()))
NewWeekdayModel_multi <- setpars(NewWeekdayModel_multi, getpars(wkmulti_model19))
logLik(NewWeekdayModel_multi)
loglika=logLik(wkmulti_model19)
loglika/4

#N=23 STATES
NewWeekdayModel_multi <- depmix(list(Global_active_power~1, Global_intensity~1),data=testdata_Weekdays,
                                nstates=23,ntimes=newNumWeekdayHours,family=list(gaussian(), gaussian()))
NewWeekdayModel_multi <- setpars(NewWeekdayModel_multi, getpars(wkmulti_model18))
logLik(NewWeekdayModel_multi)
loglika=logLik(wkmulti_model18)
loglika/4

#N=22 STATES
NewWeekdayModel_multi <- depmix(list(Global_active_power~1, Global_intensity~1),data=testdata_Weekdays,
                                nstates=22,ntimes=newNumWeekdayHours,family=list(gaussian(), gaussian()))
NewWeekdayModel_multi <- setpars(NewWeekdayModel_multi, getpars(wkmulti_model17))
logLik(NewWeekdayModel_multi)
loglika=logLik(wkmulti_model17)
loglika/4

#N=21 STATES
NewWeekdayModel_multi <- depmix(list(Global_active_power~1, Global_intensity~1),data=testdata_Weekdays,
                                nstates=21,ntimes=newNumWeekdayHours,family=list(gaussian(), gaussian()))
NewWeekdayModel_multi <- setpars(NewWeekdayModel_multi, getpars(wkmulti_model16))
logLik(NewWeekdayModel_multi)
loglika=logLik(wkmulti_model16)
loglika/4

# Based on the testing results, both weekdays and weekend day models obtain the lowest different between training and testing loglikelihoods
# at N-states=22 (log-likelihood values in the training sets are rather close, so the comparison of the difference was done to find the
# rightly fitted models)

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
# Selected Weekend day: Sundays
Weekends_test1 <- df_test1[df_test1$day_of_the_week == 7,]
# Selected Window: 9am-1pm
Weekends_test1 <- Weekends_test1[hour(Weekends_test1$Time) >= 9 & hour(Weekends_test1$Time) < 13,]

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
# Selected Weekend day: Sundays
Weekends_test2 <- df_test2[df_test2$day_of_the_week == 7,]
# Selected Window: 9am-1pm
Weekends_test2 <- Weekends_test2[hour(Weekends_test2$Time) >= 9 & hour(Weekends_test2$Time) < 13,]

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
# Selected Weekend day: Sundays
Weekends_test3 <- df_test3[df_test3$day_of_the_week == 7,]
# Selected Window: 9am-1pm
Weekends_test3 <- Weekends_test3[hour(Weekends_test3$Time) >= 9 & hour(Weekends_test3$Time) < 13,]


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
# Selected Weekend day: Sundays
Weekends_test4 <- df_test4[df_test4$day_of_the_week == 7,]
# Selected Window: 9am-1pm
Weekends_test4 <- Weekends_test4[hour(Weekends_test4$Time) >= 9 & hour(Weekends_test4$Time) < 13,]

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
# Selected Weekend day: Sundays
Weekends_test5 <- df_test5[df_test5$day_of_the_week == 7,]
# Selected Window: 9am-1pm
Weekends_test5 <- Weekends_test5[hour(Weekends_test5$Time) >= 9 & hour(Weekends_test5$Time) < 13,]

testNumReps_weekdays = rep(240, 51)
testNumReps_weekends = rep(240, 52)

# Similar to above, testing of anomalous datasets using multivariate HMMs for weekdays and weekends are completed with the code below.
# Appropriate vakues for weekday models and weekend day models separately.
# test data #1
wkmulti_test1 <- depmix(list(Global_active_power~1, Global_intensity~1),data=Weekdays_test1,nstates=22,ntimes=testNumReps_weekdays,family=list(gaussian(), gaussian()))
wkmulti_test1 <- setpars(wkmulti_test1, getpars(wkmulti_model17))
logLik(wkmulti_test1)

# test data #2
wkmulti_test2 <- depmix(response = Global_active_power~1,data=Weekdays_test2,nstates=22,ntimes=testNumReps_weekdays)
wkmulti_test2 <- setpars(wkmulti_test2, getpars(wkmulti_model17))
logLik(wkmulti_test2)

# test data #3
wkmulti_test3 <- depmix(response = Global_active_power~1,data=df_test3,nstates=22,ntimes=testNumReps_weekdays)
wkmulti_test3 <- setpars(wkmulti_test3, getpars(wkmulti_model17))
logLik(wkmulti_test3)

# test data #4
wkmulti_test4 <- depmix(response = Global_active_power~1,data=df_test4,nstates=22,ntimes=testNumReps_weekdays)
wkmulti_test4 <- setpars(wkmulti_test4, getpars(wkmulti_model17))
logLik(wkmulti_test4)

# test data #5
wkmulti_test5 <- depmix(response = Global_active_power~1,data=df_test5,nstates=22,ntimes=testNumReps_weekdays)
wkmulti_test5 <- setpars(wkmulti_test5, getpar(wkmulti_model17))
logLik(wkmulti_test5)

######################################################################################################################
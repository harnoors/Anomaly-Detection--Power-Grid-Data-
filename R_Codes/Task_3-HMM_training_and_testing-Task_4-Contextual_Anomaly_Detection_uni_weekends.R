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
# Selected Weekend day: Sundays
Weekends <- df[df$day_of_the_week == 7,]

# Weekday and weekend time window: 9am-1pm (4 hours)
Weekendhours = Weekends[hour(Weekends$Time) >= 9 & hour(Weekends$Time) < 13,]

# Model Training
# Taking approximately 1/5 of the given dataset as the test data,
# We select December 16th, 2016 to April 30th, 2009 as our training dataset
# and use the rest of it to test our HMM models.
trainingdata_Weekends <- Weekendhours[Weekendhours$Date >= "2006-12-16" & Weekendhours$Date <= "2009-4-30",]

# No. of weekend time frame repetitions within the given date range
# 240 minutes/day, 124 Sundays within the date range
numWeekendHours = rep(240, 124)


BIC_array=array(data = 0, dim = 18)
logLik_array=array(data = 0, dim = 18)

# Univariate HMM Models for weekends data
# N-state range: 5-22 (to determine the most suitable model based on log-Likelihood and BIC values)
wkduni1 <- depmix(response = Global_active_power~1,data=trainingdata_Weekends,
                  nstates=5,ntimes=numWeekendHours)
wkduni_model1 <- fit(wkduni1)
logLik(wkduni_model1)
BIC(wkduni_model1)


wkduni2 <- depmix(response = Global_active_power~1,data=trainingdata_Weekends,
                  nstates=6,ntimes=numWeekendHours)
wkduni_model2 <- fit(wkduni2)
logLik(wkduni_model2)
BIC(wkduni_model2)


wkduni3 <- depmix(response = Global_active_power~1,data=trainingdata_Weekends,
                  nstates=7,ntimes=numWeekendHours)
wkduni_model3 <- fit(wkduni3)
logLik(wkduni_model3)
BIC(wkduni_model3)


wkduni4 <- depmix(response = Global_active_power~1,data=trainingdata_Weekends,
                  nstates=8,ntimes=numWeekendHours)
wkduni_model4 <- fit(wkduni4)
logLik(wkduni_model4)
BIC(wkduni_model4)


wkduni5 <- depmix(response = Global_active_power~1,data=trainingdata_Weekends,
                  nstates=9,ntimes=numWeekendHours)
wkduni_model5 <- fit(wkduni5)
logLik(wkduni_model5)
BIC(wkduni_model5)


wkduni6 <- depmix(response = Global_active_power~1,data=trainingdata_Weekends,
                  nstates=10,ntimes=numWeekendHours)
wkduni_model6 <- fit(wkduni6)
logLik(wkduni_model6)
BIC(wkduni_model6)


wkduni7 <- depmix(response = Global_active_power~1,data=trainingdata_Weekends,
                  nstates=11,ntimes=numWeekendHours)
wkduni_model7 <- fit(wkduni7)
logLik(wkduni_model7)
BIC(wkduni_model7)


wkduni8 <- depmix(response = Global_active_power~1,data=trainingdata_Weekends,
                  nstates=12,ntimes=numWeekendHours)
wkduni_model8 <- fit(wkduni8)
logLik(wkduni_model8)
BIC(wkduni_model8)


wkduni9 <- depmix(response = Global_active_power~1,data=trainingdata_Weekends,
                  nstates=13,ntimes=numWeekendHours)
wkduni_model9 <- fit(wkduni9)
logLik(wkduni_model9)
BIC(wkduni_model9)


wkduni10 <- depmix(response = Global_active_power~1,data=trainingdata_Weekends,
                   nstates=14,ntimes=numWeekendHours)
wkduni_model10 <- fit(wkduni10)
logLik(wkduni_model10)
BIC(wkduni_model10)


wkduni11 <- depmix(response = Global_active_power~1,data=trainingdata_Weekends,
                   nstates=15,ntimes=numWeekendHours)
wkduni_model11 <- fit(wkduni11)
logLik(wkduni_model11)
BIC(wkduni_model11)


wkduni12 <- depmix(response = Global_active_power~1,data=trainingdata_Weekends,
                   nstates=16,ntimes=numWeekendHours)
wkduni_model12 <- fit(wkduni12)
logLik(wkduni_model12)
BIC(wkduni_model12)


wkduni13<- depmix(response = Global_active_power~1,data=trainingdata_Weekends,
                  nstates=17,ntimes=numWeekendHours)
wkduni_model13 <- fit(wkduni13)
logLik(wkduni_model13)
BIC(wkduni_model13)


wkduni14<- depmix(response = Global_active_power~1,data=trainingdata_Weekends,
                  nstates=18,ntimes=numWeekendHours)
wkduni_model14 <- fit(wkduni14)
logLik(wkduni_model14)
BIC(wkduni_model14)


wkduni15<- depmix(response = Global_active_power~1,data=trainingdata_Weekends,
                  nstates=19,ntimes=numWeekendHours)
wkduni_model15 <- fit(wkduni15)
logLik(wkduni_model15)
BIC(wkduni_model15)


wkduni16<- depmix(response = Global_active_power~1,data=trainingdata_Weekends,
                  nstates=20,ntimes=numWeekendHours)
wkduni_model16 <- fit(wkduni16)
logLik(wkduni_model16)
BIC(wkduni_model16)


wkduni17<- depmix(response = Global_active_power~1,data=trainingdata_Weekends,
                  nstates=21,ntimes=numWeekendHours)
wkduni_model17 <- fit(wkduni17)
logLik(wkduni_model17)
BIC(wkduni_model17)

# logLik: -5660.665
# BIC: 16286.37

wkduni18<- depmix(response = Global_active_power~1,data=trainingdata_Weekends,
                  nstates=22,ntimes=numWeekendHours)
wkduni_model18 <- fit(wkduni18)
logLik(wkduni_model18)
BIC(wkduni_model18)

# logLik: -5522.682
# BIC: 16473.95

BIC_array[1]<-BIC(wkduni_model1)
logLik_array[1]<-logLik(wkduni_model1)

BIC_array[2]<-BIC(wkduni_model2)
logLik_array[2]<-logLik(wkduni_model2)

BIC_array[3]<-BIC(wkduni_model3)
logLik_array[3]<-logLik(wkduni_model3)

BIC_array[4]<-BIC(wkduni_model4)
logLik_array[4]<-logLik(wkduni_model4)

BIC_array[5]<-BIC(wkduni_model5)
logLik_array[5]<-logLik(wkduni_model5)

BIC_array[6]<-BIC(wkduni_model6)
logLik_array[6]<-logLik(wkduni_model6)

BIC_array[7]<-BIC(wkduni_model7)
logLik_array[7]<-logLik(wkduni_model7)

BIC_array[8]<-BIC(wkduni_model8)
logLik_array[8]<-logLik(wkduni_model8)

BIC_array[9]<-BIC(wkduni_model9)
logLik_array[9]<-logLik(wkduni_model9)

BIC_array[10]<-BIC(wkduni_model10)
logLik_array[10]<-logLik(wkduni_model10)

BIC_array[11]<-BIC(wkduni_model11)
logLik_array[11]<-logLik(wkduni_model11)

BIC_array[12]<-BIC(wkduni_model12)
logLik_array[12]<-logLik(wkduni_model12)

BIC_array[13]<-BIC(wkduni_model13)
logLik_array[13]<-logLik(wkduni_model13)

BIC_array[14]<-BIC(wkduni_model14)
logLik_array[14]<-logLik(wkduni_model14)

BIC_array[15]<-BIC(wkduni_model15)
logLik_array[15]<-logLik(wkduni_model15)

BIC_array[16]<-BIC(wkduni_model16)
logLik_array[16]<-logLik(wkduni_model16)

BIC_array[17]<-BIC(wkduni_model17)
logLik_array[17]<-logLik(wkduni_model17)

BIC_array[18]<-BIC(wkduni_model18)
logLik_array[18]<-logLik(wkduni_model18)

newframe = data.frame("N_states" = 5:22, "BIC" = BIC_array, "logLik" = logLik_array)
data <- melt(newframe, "N_states")
loglik_BIC_plot = ggplot(data, aes(x=N_states, y=value, color=variable)) + geom_point() + geom_line()
loglik_BIC_plot+ggtitle("Log-likelihood and BIC Values for \n trained univariate HMM models (Weekends), N_states: 5-22")+
  theme(plot.title = element_text(hjust = 0.5))


# Based on the 'logLik_BIC_plot', the graph appears to flatten for both BIC and logLik values for trained HMMs.
# This implies that additional states do not result in further increased performance of the model other than adding to its complexity.
# Minimum BIC value is obtained when N-states=19 (logLik values for states 19, 21 and 22 are very close)
# hence, 'wkduni_model15' is selected as the best univariate HMM model for the weekends training data


######################################################################################################################


# Model Testing
testdata_weekends <- Weekendhours[Weekendhours$Date >= "2009-05-01" & Weekendhours$Date <= "2009-12-01",]

# No. of weekend time frame repetitions within the given date range
# 240 minutes/day, 31 Sundays within the date range
newNumWeekendHours = rep(240, 31)

# Weekends univariate model testing
NewWeekendModel_uni <- depmix(response = Global_active_power~1,data=testdata_weekends,nstates=19,ntimes=newNumWeekendHours)
NewWeekendModel_uni <- setpars(NewWeekendModel_uni, getpars(wkduni_model15))
logLik(NewWeekendModel_uni)
1/4*logLik(wkduni_model15)
BIC(NewWeekendModel_uni)

######################################################################################################################


# Task 4: Contextual Anomaly Detection
df_test1=read.table('./TestData/test1.txt', header = TRUE, sep = ",", dec = ".")
a=dmy(df_test1$Date)
df_test1$year=year(a)
df_test1$year= as.integer(df_test1$year)
d=strftime(df_test1$Date, format = "%u")
df_test1$day_of_the_week <- as.integer(d)

df_test1$Date <- as.POSIXct(df_test1$Date, format = "%d/%m/%Y") 
df_test1$Time <- as.POSIXct(df_test1$Time, format = "%H:%M:%S")
# Selected Weekend day: Sundays
Weekends_test1 <- df_test1[df_test1$day_of_the_week == 7,]
# Selected Window: 9am-1pm
Weekends_test1 <- Weekends_test1[hour(Weekends_test1$Time) >= 9 & hour(Weekends_test1$Time) < 13,]



df_test2=read.table('./TestData/test2.txt', header = TRUE, sep = ",", dec = ".")
a=dmy(df_test2$Date)
df_test2$year=year(a)
df_test2$year= as.integer(df_test2$year)
d=strftime(df_test2$Date, format = "%u")
df_test2$day_of_the_week <- as.integer(d)

df_test2$Date <- as.POSIXct(df_test2$Date, format = "%d/%m/%Y") 
df_test2$Time <- as.POSIXct(df_test2$Time, format = "%H:%M:%S")
# Selected Weekend day: Sundays
Weekends_test2 <- df_test2[df_test2$day_of_the_week == 7,]
# Selected Window: 9am-1pm
Weekends_test2 <- Weekends_test2[hour(Weekends_test2$Time) >= 9 & hour(Weekends_test2$Time) < 13,]



df_test3=read.table('./TestData/test3.txt', header = TRUE, sep = ",", dec = ".")
a=dmy(df_test3$Date)
df_test3$year=year(a)
df_test3$year= as.integer(df_test3$year)
d=strftime(df_test3$Date, format = "%u")
df_test3$day_of_the_week <- as.integer(d)

df_test3$Date <- as.POSIXct(df_test3$Date, format = "%d/%m/%Y") 
df_test3$Time <- as.POSIXct(df_test3$Time, format = "%H:%M:%S")
# Selected Weekend day: Sundays
Weekends_test3 <- df_test3[df_test3$day_of_the_week == 7,]
# Selected Window: 9am-1pm
Weekends_test3 <- Weekends_test3[hour(Weekends_test3$Time) >= 9 & hour(Weekends_test3$Time) < 13,]



df_test4=read.table('./TestData/test4.txt', header = TRUE, sep = ",", dec = ".")
a=dmy(df_test4$Date)
df_test4$year=year(a)
df_test4$year= as.integer(df_test4$year)
d=strftime(df_test4$Date, format = "%u")
df_test4$day_of_the_week <- as.integer(d)

df_test4$Date <- as.POSIXct(df_test4$Date, format = "%d/%m/%Y") 
df_test4$Time <- as.POSIXct(df_test4$Time, format = "%H:%M:%S")
# Selected Weekend day: Sundays
Weekends_test4 <- df_test4[df_test4$day_of_the_week == 7,]
# Selected Window: 9am-1pm
Weekends_test4 <- Weekends_test4[hour(Weekends_test4$Time) >= 9 & hour(Weekends_test4$Time) < 13,]



df_test5=read.table('./TestData/test5.txt', header = TRUE, sep = ",", dec = ".")
a=dmy(df_test5$Date)
df_test5$year=year(a)
df_test5$year= as.integer(df_test5$year)
d=strftime(df_test5$Date, format = "%u")
df_test5$day_of_the_week <- as.integer(d)

df_test5$Date <- as.POSIXct(df_test5$Date, format = "%d/%m/%Y") 
df_test5$Time <- as.POSIXct(df_test5$Time, format = "%H:%M:%S")
# Selected Weekend day: Sundays
Weekends_test5 <- df_test5[df_test5$day_of_the_week == 7,]
# Selected Window: 9am-1pm
Weekends_test5 <- Weekends_test5[hour(Weekends_test5$Time) >= 9 & hour(Weekends_test5$Time) < 13,]



# Nstates of optimal Weekend Univariate model: 19
# No. of weekend time frame repetitions within the given date range
# 240 minutes/day, 52 Sundays within the date range
testNumReps_weekends = rep(240, 52)

# test data #1
52/124*logLik(wkduni_model15)
# 52/124*loglik for training data set (nstates=19): -2506.275
wkdModel_uni_test1 <- depmix(response = Global_active_power~1,data=Weekends_test1,nstates=19,ntimes=testNumReps_weekends)
wkdModel_uni_test1 <- setpars(wkdModel_uni_test1, getpars(wkduni_model15))
logLik(wkdModel_uni_test1)
# Anomaly expectation: anomalous data set with few clusters deviating from 'normal' behaviour based on the optimal HMM model
# and the loglik result obtained for trained data set

# test data #2
wkdModel_uni_test2 <- depmix(response = Global_active_power~1,data=Weekends_test2,nstates=19,ntimes=testNumReps_weekends)
wkdModel_uni_test2 <- setpars(wkdModel_uni_test2, getpars(wkduni_model15))
logLik(wkdModel_uni_test2)
# Anomaly expectation: anomalous data set with few clusters deviating from 'normal' behaviour based on the optimal HMM model
# and the loglik result obtained for trained data set
# (slightly more anomalies present compared to test data #1)

# test data #3
wkdModel_uni_test3 <- depmix(response = Global_active_power~1,data=Weekends_test3,nstates=19,ntimes=testNumReps_weekends)
wkdModel_uni_test3 <- setpars(wkdModel_uni_test3, getpars(wkduni_model15))
logLik(wkdModel_uni_test3)
# Anomaly expectation: anomalous data set with few clusters deviating from 'normal' behaviour based on the optimal HMM model
# and the loglik result obtained for trained data set
# (Similar number of anomalies present compared to test data #1)

# test data #4
wkdModel_uni_test4 <- depmix(response = Global_active_power~1,data=Weekends_test4,nstates=19,ntimes=testNumReps_weekends)
wkdModel_uni_test4 <- setpars(wkdModel_uni_test4, getpars(wkduni_model15))
logLik(wkdModel_uni_test4)
# Anomaly expectation: HIGHLY anomalous data set with many clusters deviating from 'normal' behaviour based on the optimal HMM model
# and the loglik result obtained for trained data set
# (Much higher number of anomalies present compared to test data #1)

# test data #5
wkdModel_uni_test5 <- depmix(response = Global_active_power~1,data=Weekends_test5,nstates=19,ntimes=testNumReps_weekends)
wkdModel_uni_test5 <- setpars(wkdModel_uni_test5, getpars(wkduni_model15))
logLik(wkdModel_uni_test5)
# Anomaly expectation: HIGHLY anomalous data set with many clusters deviating from 'normal' behaviour based on the optimal HMM model
# and the loglik result obtained for trained data set
# (Much higher number of anomalies present compared to test data #1)

######################################################################################################################
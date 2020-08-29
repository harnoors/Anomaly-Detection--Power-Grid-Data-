library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggbiplot)

df=read.table('TrainData.txt', header = TRUE, sep = ",", dec = ".")

a=dmy(df$Date)
df$year=year(a)
df$year= as.integer(df$year)

df$Date <- as.POSIXlt(df$Date, format = "%d/%m/%Y")
betterdate=parse_date_time(df$Date, orders = c("ymd", "dmy", "mdy"))
df$week_day=strftime(betterdate, format = "%u", tz = "", usetz = FALSE)

temp <- factor(df$Time)
a <- hms(as.character(temp))
df$hours <- as.integer(hour(a))  

df= df[!is.na(df$Global_active_power), ]
df= df[!is.na(df$Global_reactive_power), ]

year= df[(df$hours>=9) & (df$hours<=12),]
year= year[!is.na(year$Global_reactive_power), ]
year.pca <- prcomp(year[,c(3:6)], center = TRUE,scale. = TRUE)
summary(year.pca)

plot1=ggbiplot(year.pca, scale = 1, pc.biplot =TRUE, groups = year$hours, labels =NULL,labels.size = 3, alpha = 1,
               var.axes = TRUE, circle=TRUE, circle.prob = 0.80, varname.size = 9,varname.adjust =2.0, varname.abbrev = FALSE)
plot1=plot1+ggtitle("PCA of TrainData dataset for All Years")+
  theme_minimal()+
  theme(legend.position = "bottom")

allyear <-aggregate(df, by=list(df$hours),FUN=mean, na.rm=FALSE)
allyear.pca <- prcomp(allyear[,c(4:7)], center = TRUE,scale. = TRUE)


summary(allyear.pca)

plot1=ggbiplot(allyear.pca, scale = 1, labels=rownames(allyear),labels.size = 6, alpha = 1, circle.prob = 0.3, varname.size = 7)
plot1=plot1+ggtitle("PCA1 vs PCA2")+
  theme_minimal()+
  theme(legend.position = "bottom")
plot1=plot1+theme(axis.text=element_text(size=20),
                  axis.title=element_text(size=20,face="bold"),plot.title=element_text(size=20,face="bold"))



year1<-df[df$year == 2006,]
year2<-df[df$year == 2007,]
year3<-df[df$year == 2008,]
year4<-df[df$year == 2009,]

first <-aggregate(year1, by=list(year1$hours),FUN=mean, na.rm=FALSE)
first.pca <- prcomp(first[,c(4:7)], center = TRUE,scale. = TRUE)

summary(first.pca)


plotA=ggbiplot(first.pca,labels=rownames(first),labels.size = 6, alpha = 1, circle.prob = 0.05, varname.size = 7)

plotA=plotA+ggtitle("PCA1 vs PCA2")+
  theme_minimal()+
  theme(legend.position = "bottom")
plotA=plotA+theme(axis.text=element_text(size=20),
                  axis.title=element_text(size=20,face="bold"),plot.title=element_text(size=20,face="bold"))



second <-aggregate(year2, by=list(year2$hours),FUN=mean, na.rm=FALSE)
second.pca <- prcomp(second[,c(4:7)], center = TRUE,scale. = TRUE)

summary(second.pca)

plotB=ggbiplot(second.pca,labels=rownames(second),labels.size = 6, alpha = 1, circle.prob = 0.05, varname.size = 7)

plotB=plotB+ggtitle("PCA1 vs PCA2")+
  theme_minimal()+
  theme(legend.position = "bottom")
plotB=plotB+theme(axis.text=element_text(size=20),
                  axis.title=element_text(size=20,face="bold"),plot.title=element_text(size=20,face="bold"))


third <-aggregate(year3, by=list(year3$hours),FUN=mean, na.rm=FALSE)
third.pca <- prcomp(third[,c(4:7)], center = TRUE,scale. = TRUE)

summary(third.pca)


plotC=ggbiplot(third.pca,labels=rownames(third),labels.size = 6, alpha = 1, circle.prob = 0.1, varname.size = 7)

plotC=plotC+ggtitle("PCA1 vs PCA2")+
  theme_minimal()+
  theme(legend.position = "bottom")
plotC=plotC+theme(axis.text=element_text(size=20),
                  axis.title=element_text(size=20,face="bold"),plot.title=element_text(size=20,face="bold"))


forth <-aggregate(year3, by=list(year3$hours),FUN=mean, na.rm=FALSE)
forth.pca <- prcomp(forth[,c(4:7)], center = TRUE,scale. = TRUE)

summary(forth.pca)

plotD=ggbiplot(forth.pca,labels=rownames(forth),labels.size = 6, alpha = 1, circle.prob = 0.1, varname.size = 7)

plotD=plotD+ggtitle("PCA1 vs PCA2")+
  theme_minimal()+
  theme(legend.position = "bottom")
plotD=plotD+theme(axis.text=element_text(size=20),
                  axis.title=element_text(size=20,face="bold"),plot.title=element_text(size=20,face="bold"))


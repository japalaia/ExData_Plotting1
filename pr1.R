---
title: "ExploratoryDataAnalysisProject1"
author: "Joe Palaia"
date: "Wednesday, December 09, 2015"
output: html_document
---



setwd("C:/RWork/ExploratoryDataAnalysis/Project1/")

library(utils)
library(dplyr)
library(png)
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
                , "C:/RWork/ExploratoryDataAnalysis/Project1/household_power_consumption.zip",  )

unzip("household_power_consumption.zip")


#read in the file 
#data<-read.table("household_power_consumption.txt", sep=";",  skip=21997,  nrows = 525600)
data<-read.table("household_power_consumption.txt", sep=";")
names(data)<- c("date", "time", "global_active_power", "global_reactive_power", "voltage", "global_intensity", "sub_metering_1", "sub_metering_2", "sub_metering_3" )
head(data)
tail(data)

#get count of days
nrow(unique(data[1]))
data$datef<- as.Date(data$date, "%d/%m/%Y")

s2day<-data[data$datef=='2007-02-01'| data$datef =='2007-02-02',]
nrow(s2day)
s<-s2day
#s$TIME<- format(strptime(s2day$time, "%X"), "%H:%M:%S")
s$global_active_power<- as.numeric(as.character(as.factor(s2day$global_active_power)))
s$global_reactive_power<- as.numeric(as.character(as.factor(s2day$global_reactive_power)))
s$weekday<- weekdays(s$datef)
s<-s[2:2881,]
s<- s[ is.na(s$time)!=TRUE,]
s[s$sub_metering_2==14,]
s$sub_metering_1<-as.numeric(as.character(as.factor(s$sub_metering_1)))
s$sub_metering_2<-as.numeric(as.character(as.factor(s$sub_metering_2)))
s$sub_metering_3<-as.numeric(as.character(as.factor(s$sub_metering_3)))
s$index<-row(s[1])
s2day$index<-row(s2day[1]) 
s$voltage2<-as.numeric(as.character(as.factor(s$voltage)))
summary(s)
head(s)

png(filename = "plot1.png",   width = 480, height = 480)
hist( s$global_active_power ,   axes=TRUE, col="red", main="Global Active Power", ylab="Frequency", xlab="Global Active Power (kilowatts)"   )
dev.off()

png(filename = "plot2.png",   width = 480, height = 480)
plot(x=s$index, y=s$global_active_power, type="l", xaxt="n", ylab="Global Active Power (Kilowatt)", xlab="")
axis(1,  at= c(0, 1500, 2881), labels=c("Thurs", "Fri", "Sat") )
dev.off() 
 

png(filename = "plot3.png",   width = 480, height = 480 , legend = TRUE)
plot(x=s$index, y=s$sub_metering_1 , type="l" , xaxt="n", ylab="Energy Sub Metering" , xlab="")
lines(s$sub_metering_1, col="black")
lines(s$sub_metering_2 , col ="red")
lines(s$sub_metering_3, col="blue")
legend(2000, 33.5, c("sub_metering_1", "sub_metering_2", "sub_metering_3"),  lty=c(1,1,1), lwd=c(2.5, 2.5, 2.5), col=c("black", "red", "blue"))
axis(1,  at= c(0, 1500, 2879), labels=c("Thurs", "Fri", "Sat") )

dev.off()

png(filename = "plot4.png",   width = 480, height = 480 )
par(mfrow=c(2,2))

plot(x=s$index, y=s$global_active_power, type="l", xaxt="n", ylab="Global Active Power (Kilowatt)", xlab="")
axis(1,  at= c(0, 1500, 2881), labels=c("Thurs", "Fri", "Sat") )

plot(x=s$index, y=s$voltage2 , type="l" , xaxt="n", ylab="Voltage" , xlab="")
axis(1,  at= c(0, 1500, 2881), labels=c("Thurs", "Fri", "Sat") )

plot(x=s$index, y=s$sub_metering_1 , type="l" , xaxt="n", ylab="Energy Sub Metering" , xlab="")
lines(s$sub_metering_1, col="black")
lines(s$sub_metering_2 , col ="red")
lines(s$sub_metering_3, col="blue")
legend(1200, 38.5, c("sub_metering_1", "sub_metering_2", "sub_metering_3"), bty="n" , lty=c(1,1,1), lwd=c(2.5, 2.5, 2.5), col=c("black", "red", "blue"))
axis(1,  at= c(0, 1500, 2879), labels=c("Thurs", "Fri", "Sat") )


plot(x=s$index, y=s$global_reactive_power , xaxt="n" , type="l" , ylab="Global_reactive_power" , xlab="")
axis(1,  at= c(0, 1500, 2881), labels=c("Thurs", "Fri", "Sat") )

dev.off()


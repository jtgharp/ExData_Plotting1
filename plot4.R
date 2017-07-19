library(lubridate)
library(grDevices)


#SET YOUR WORKING DIRECTORY ANd PLACE THE CODE IN THAT DIRECTORY THEN RUN & ENJOY

plot4 <-function(){
        #calculate how many rows to skip and how many rows to read Detail explanation in plot1.R file
        f <- read.table("household_power_consumption.txt", sep=";",na.strings = "?",colClasses = c("character","character",rep("numeric",7)), nrows = 3, header=TRUE, stringsAsFactors = FALSE ) 
        d1<-dmy_hms(paste(f$Date[1] , f$Time[1])) ## first date and time in the first row
        d2 <- dmy("1/2/2007") # we start reading row from 1st Feb 2007 00 hrs
        rowstoskip <- difftime(d2,d1,units="mins")
        rowstoskip <- rowstoskip-1500
        rowstoread <- (24*60*2)+1500
       
        #now let us read and subset just for the two dates
        f1 <- read.table("household_power_consumption.txt", nrows = 3,  sep=";",na.strings = "?",colClasses = c("character","character",rep("numeric",7)), header=TRUE, stringsAsFactors = FALSE ) 
        f2 <- read.table("household_power_consumption.txt", skip = rowstoskip, nrows =rowstoread,  sep=";",na.strings = "?",colClasses = c("character","character",rep("numeric",7)), header=TRUE, stringsAsFactors = FALSE ) 
        colnames(f2)<-colnames(f1)
        ss <- subset(f2, (f2$Date=="1/2/2007") | (f2$Date=="2/2/2007")) # for 2 days we will get 2880 rows
       
        #first get a combined array of Date and Time columns combined as date_time
        cdttm<-dmy_hms(paste(ss$Date , ss$Time))
        
        #create 4 functions one for each plot
        plot_tl <- function() {
                #plot top left
                acp<-ss$Global_active_power
                plot(cdttm,acp,type="l", xlab="", ylab="Global Active Power (kilowatts)")
        }
        
        plot_tr <- function() {
                #plot top right
                vol<-ss$Voltage
                plot(cdttm,vol,type="l", xlab="datetime", ylab="Voltage (Volts)")
        }
        
        plot_bl <-function(){
                #Plot bottom left
                sub1 <- ss$Sub_metering_1
                plot(cdttm,sub1,type="l", xlab="", ylab="Energy sub metering")
                sub2 <- ss$Sub_metering_2
                par(col="red")
                lines(cdttm,sub2,type="l")
                sub3 <- ss$Sub_metering_3
                par(col="blue")
                lines(cdttm,sub3,type="l")
                par(col="black")
                legend("topright",legend= c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col = c("black","red","blue"),lty= c(1,1,1) )
        
        }
        
        plot_br <- function() {
                #plot bottom right
                
                racp<-ss$Global_reactive_power
                plot(cdttm,racp,type="l", xlab="datetime", ylab="Global Rective Power (kilowatts)")
        }
        
        #set png file and device
        png(filename = "plot4.png",width=480,height=480, bg="white")
        par(mfrow=c(2,2))
        plot_tl()
        plot_tr()
        plot_bl()
        plot_br()
        #close device
        dev.off()
}
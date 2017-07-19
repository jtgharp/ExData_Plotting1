library(lubridate)
library(grDevices)


#SET YOUR WORKING DIRECTORY ANd PLACE THE CODE IN THAT DIRECTORY THEN RUN & ENJOY

plot3 <-function(){
        #calculate how many rows to skip and how many rows to read Detail explanation in plot1.R file
        f <- read.table("household_power_consumption.txt", sep=";",na.strings = "?",colClasses = c("character","character",rep("numeric",7)), nrows = 3, header=TRUE, stringsAsFactors = FALSE ) 
        d1<-dmy_hms(paste(f$Date[1] , f$Time[1])) ## first date and time in the first row
        d2 <- dmy("1/2/2007") # we start reading row from 1st Feb 2007 00 hrs
        rowstoskip <- difftime(d2,d1,units="mins")
        rowstoskip <- rowstoskip-1500
        rowstoread <- (24*60*2)+1500
       
        #now let us read
        f1 <- read.table("household_power_consumption.txt", nrows = 3,  sep=";",na.strings = "?",colClasses = c("character","character",rep("numeric",7)), header=TRUE, stringsAsFactors = FALSE ) 
        f2 <- read.table("household_power_consumption.txt", skip = rowstoskip, nrows =rowstoread,  sep=";",na.strings = "?",colClasses = c("character","character",rep("numeric",7)), header=TRUE, stringsAsFactors = FALSE ) 
       
        #also note the following trick is to keep the column descriptions
        colnames(f2)<-colnames(f1)
        #so now the subset has the column headings as we want (not X1 X2 etc of a dataframe)
        ss <- subset(f2, (f2$Date=="1/2/2007") | (f2$Date=="2/2/2007")) # for 2 days we will get 2880 rows
       
        #first get a combined array of Date and Time columns combined as date_time
        cdttm<-dmy_hms(paste(ss$Date , ss$Time))
        
        #set png file and device
        png(filename = "plot3.png",width=480,height=480, bg="white")
       
        #now get array of the sub metering 1
        sub1 <- ss$Sub_metering_1
        
        #Now plot
        plot(cdttm,sub1,type="l", xlab="", ylab="Energy sub metering")
        #WOW ! plot is very smart.  based on date.time objects for x axis it figured out x tics
        
        #now get array of the sub metering 2
        sub2 <- ss$Sub_metering_2
        
        #set line color = red
        par(col="red")
        
        #now add lines for sub metering 2
        lines(cdttm,sub2,type="l")
        
        #now get array of the sub metering 3
        sub3 <- ss$Sub_metering_3
        
        #set line color = blue
        par(col="blue")
        
        #now add lines for sub metering 3
        lines(cdttm,sub3,type="l")
        
        #now set col to black so legend will be written in black text
        par(col="black")
        
        #finally add legend
        legend("topright",legend= c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col = c("black","red","blue"),lty= c(1,1,1) )
        
        #close device
        dev.off()
}
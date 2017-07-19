library(lubridate)
library(grDevices)


#SET YOUR WORKING DIRECTORY ANd PLACE THE CODE IN THAT DIRECTORY THEN RUN & ENJOY

plot2 <-function(){
        #calculate howmany rows to skip and how many rows to read Detail explanation in plot1.R file
        f <- read.table("household_power_consumption.txt", sep=";",na.strings = "?",colClasses = c("character","character",rep("numeric",7)), nrows = 3, header=TRUE, stringsAsFactors = FALSE ) 
        d1<-dmy_hms(paste(f$Date[1] , f$Time[1])) ## first date and time in the first row
        d2 <- dmy("1/2/2007") # we start reading row from 1st Feb 2007 00 hrs
        rowstoskip <- difftime(d2,d1,units="mins")
        rowstoskip <- rowstoskip-1500
        rowstoread <- (24*60*2)+1500
        
        #now let us read
        f1 <- read.table("household_power_consumption.txt", nrows = 3,  sep=";",na.strings = "?",colClasses = c("character","character",rep("numeric",7)), header=TRUE, stringsAsFactors = FALSE ) 
        f2 <- read.table("household_power_consumption.txt", skip = rowstoskip, nrows =rowstoread,  sep=";",na.strings = "?",colClasses = c("character","character",rep("numeric",7)), header=TRUE, stringsAsFactors = FALSE ) 
        colnames(f2)<-colnames(f1)
        ss <- subset(f2, (f2$Date=="1/2/2007") | (f2$Date=="2/2/2007")) # for 2 days we will get 2880 rows
        #check is any NA in f2Global_active_power
        if(sum(is.na(ss))>0) {message("NA's were found")} # I checked none were found
       
        # first get a combined arracy of Date and Time columns combined as date_time
        cdttm<-dmy_hms(paste(ss$Date , ss$Time))
       
        #now get array of the global active  power
        acp<-ss$Global_active_power
       
        #set png file and device
        png(filename = "plot2.png",width=480,height=480, bg="white")
       
        #Now plot
        plot(cdttm,acp,type="l", xlab="", ylab="Global Active Power (kilowatts)")
        #WOW ! plot is very smart.  based on date.time objects for x axis it figured out x tics
       
        #close device
        dev.off()
        
}
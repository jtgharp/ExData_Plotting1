library(lubridate)
library(grDevices)


#SET YOUR WORKING DIRECTORY ANd PLACE THE CODE IN THAT DIRECTORY THEN RUN & ENJOY

plot1 <-function(){
        # First we have to estimate how many rows to skip till we reach the rows we are interested Feb 1st and 2nd 2007
        #first read header and first  3 lines while setting colclasses
        f <- read.table("household_power_consumption.txt", sep=";",na.strings = "?",colClasses = c("character","character",rep("numeric",7)), nrows = 3, header=TRUE, stringsAsFactors = FALSE ) 
        # When you do head(f) The first row shows a date of 16/12/2006 and "17H 24M 0S.  
        #So let us calculate how many minutes are from 16/12/2006 and "17H 24M 0S to Feb, 1 2007.  
        d1<-dmy_hms(paste(f$Date[1] , f$Time[1])) ## first date and time in the first row
        d2 <- dmy("1/2/2007") # we start reading row from 1st Feb 2007 00 hrs
        rowstoskip <- difftime(d2,d1,units="mins")
        
        # so we skip no of rows = how many minutes.  
        # how many rows do we read?  Answer is till last reading on february 2nd 2007
        # there are 24 hours in a day so 24*60 = 1440 readings in a day. so over two days 2880 minutes so we should get 2880 rows
        # to be on safe side we will read 1500 rows ahead of start of 1st Feb 2007 and go past 1500 rows on 2nd feb
        rowstoskip <- rowstoskip-1500
        rowstoread <- (24*60*2)+1500
        
        #now read subset
        f1 <- read.table("household_power_consumption.txt", nrows = 3,  sep=";",na.strings = "?",colClasses = c("character","character",rep("numeric",7)), header=TRUE, stringsAsFactors = FALSE ) 
        f2 <- read.table("household_power_consumption.txt", skip = rowstoskip, nrows =rowstoread,  sep=";",na.strings = "?",colClasses = c("character","character",rep("numeric",7)), header=TRUE, stringsAsFactors = FALSE ) 
        
        #also note the following trick is to keep the column descriptions
        colnames(f2)<-colnames(f1)
       
        ss <- subset(f2, (f2$Date=="1/2/2007") | (f2$Date=="2/2/2007")) # for 2 days we will get 2880 rows
       
        #check is any NA in f2Global_active_power
        if(sum(is.na(ss))>0) {message("NA's were found")} # I checked none were found
        
        png(filename = "plot1.png",width=480,height=480, bg="white")
        hist(ss$Global_active_power,main = "Global Active Power", xlab = "Global Active Power (kilowatts)", ylab =  "Frequency" , col="red")
        dev.off()
        
}
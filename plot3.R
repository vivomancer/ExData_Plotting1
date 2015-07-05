plot3 = function(){
  ##Reading in the pre-data to set classes for faster load speed
  tab5rows <- read.table("household_power_consumption.txt",sep=";", 
                         header = TRUE, nrows = 5, na.strings=c("?"))
  classes <- sapply(tab5rows, class)
  ##Only reading in necessary classes
  classes[c(-1,-2,-7,-8,-9)] <- "NULL"
  myData <- read.table("household_power_consumption.txt",header=TRUE,
                       sep=";", na.strings=c("?"), colClasses = classes)
  
  ##Restricting data to a date range
  Date1 <- as.Date("2007-02-01", format = "%Y-%m-%d")
  Date2 <- as.Date("2007-02-02", format = "%Y-%m-%d")
  
  myData <- subset(myData, as.Date(Date, format = "%d/%m/%Y") == Date1 |
                     as.Date(Date, format = "%d/%m/%Y") == Date2) 
  
  #convert Time column to a Time class
  myData$Date <- as.POSIXct(paste(as.character(myData$Date), 
                                  as.character(myData$Time)),
                            format="%m/%d/%Y %H:%M:%S")
  myData <- myData[,-2]
  #print plot
  dev.copy(png,'plot3.png')
  matplot(myData, type = "l",col = c("blue","black","red"),
          xaxt = "n", ylab="Energy sub metering",lty=1)
  legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         col=c("blue","black","red"), pch="", lty=c(1,1,1))
  axis(side=1, labels=c("Thur", "Fri", "Sat"), 
       at=c(0,nrow(myData) / 2,nrow(myData))) 
  dev.off()
}
plot4 <- function(){
  ##Reading in the pre-data to set classes for faster load speed
  tab5rows <- read.table("household_power_consumption.txt",sep=";", 
                         header = TRUE, nrows = 5, na.strings=c("?"))
  classes <- sapply(tab5rows, class)
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
  
  #separating data into appropriate plots
  plot1 <- myData[,c(1,3)]
  plot2 <- myData[,c(1,5)]
  plot3 <- myData[,c(1,7,8,9)]
  plot4 <- myData[,c(1,4)]
  
  #print plot
  dev.copy(png,'plot4.png')
  par(mfrow = c(2, 2), mar = c(5, 4, 2, 1))
  #plot1
  plot(myData$Global_active_power ~ as.numeric(as.factor(myData$Date)),xaxt = "n",
       ylab="Global Active Power", xlab="", type="l")
  axis(side=1, labels=c("Thur", "Fri", "Sat"), 
       at=c(0,nrow(myData) / 2,nrow(myData))) 
  #plot2
  plot(myData$Voltage ~ as.numeric(as.factor(myData$Date)),xaxt = "n",
       ylab="Voltage", xlab="", type="l")
  axis(side=1, labels=c("Thur", "Fri", "Sat"), 
       at=c(0,nrow(myData) / 2,nrow(myData))) 
  
  #plot3
  matplot(plot3, type = "l",col = c("blue","black","red"),
          xaxt = "n", ylab="Energy sub metering",lty=1)
  legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         col=c("blue","black","red"), pch="", lty=c(1,1,1), cex = 0.5)
  axis(side=1, labels=c("Thur", "Fri", "Sat"), 
       at=c(0,nrow(myData) / 2,nrow(myData)))
  
  #plot4
  plot(myData$Global_reactive_power ~ as.numeric(as.factor(myData$Date)),axes = FALSE,
       ylab="Global_reactive_power", xlab="datetime", type="l", ylim=c(0.0,0.5))
  axis(side=1, labels=c("Thur", "Fri", "Sat"), 
       at=c(0,nrow(myData) / 2,nrow(myData))) 
  axis(side = 2, at = c(0.0,0.1,0.2,0.3,0.4,0.5), las=1)
  dev.off()
}
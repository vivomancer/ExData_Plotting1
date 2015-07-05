plot1 <- function(){
  ##Reading in the pre-data to set classes for faster load speed
  tab5rows <- read.table("household_power_consumption.txt",sep=";", header = TRUE, nrows = 5, na.strings=c("?"))
  classes <- sapply(tab5rows, class)
  ##Only reading in necessary classes
  classes[c(-1,-3)] <- "NULL"
  myData <- read.table("household_power_consumption.txt",header=TRUE,
                       sep=";", na.strings=c("?"), colClasses = classes)
  
  ##Restricting data to a date range
  Date1 <- as.Date("2007-02-01", format = "%Y-%m-%d")
  Date2 <- as.Date("2007-02-02", format = "%Y-%m-%d")
  
  myData <- subset(myData, as.Date(Date, format = "%d/%m/%Y") == Date1 |
           as.Date(Date, format = "%d/%m/%Y") == Date2) 
  
  #preparing to save to png
  dev.copy(png,'plot1.png')
  
  #ploting hist
  hist(as.numeric(myData$Global_active_power), col="red", 
       breaks=seq(min(as.numeric(myData$Global_active_power)),
                  max(as.numeric(myData$Global_active_power)),l=16),
       main="Global Active Power", xlab="Global Active Power (kilowatts)",
       ylim=c(0,1200))
  
  #saving to png
  dev.off()
}
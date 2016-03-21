# clear existing default settings
plot.new()

# loads data from only Feb 1, 2007 and Feb 2, 2007
loaddata <- function() {
  library(sqldf)
  powerdata <- read.csv.sql("../household_power_consumption.txt",
                            header=TRUE, sep=";", eol="\n",
                            sql="select * from file where Date = '1/2/2007' OR Date = '2/2/2007'")
  return(powerdata)
}

# adds a datetime column to the dataframe
adddatetimev <- function(powerdata) {
  datetimev <- numeric(0)
  for (i in 1:nrow(powerdata)) {
    pasted <- paste(powerdata$Date[i], powerdata$Time[i])
    asposix <- as.POSIXct(strptime(pasted, format="%d/%m/%Y %H:%M:%S"))
    datetimev[i] <- asposix
  }
  
  powerdata$datetime <- datetimev
  return(powerdata)
}

# creates plot chart #2 (#4.1)
createplot2 <- function(powerdata) {
  with(powerdata, plot(datetime, Global_active_power, type="l",
                       xlab = "", ylab="Global Active Power (kilowatts)",
                       cex.lab = 0.75, cex.axis = 0.75, xaxt="n"))
  axis(1, at=c(min(powerdata$datetime), median(powerdata$datetime),
               max(powerdata$datetime)), labels = c("Thu", "Fri", "Sat"),
       cex.axis = 0.75)
}

# creates plot chart #4.2
createplot4dot2 <- function(powerdata) {
  with(powerdata, plot(datetime, Voltage, type="l",
                       xlab = "datetime", ylab="Voltage",
                       cex.lab = 0.75, cex.axis = 0.75, xaxt="n"))
  axis(1, at=c(min(powerdata$datetime), median(powerdata$datetime),
               max(powerdata$datetime)), labels = c("Thu", "Fri", "Sat"),
       cex.axis = 0.75) 
}

# creates plot chart #3 (#4.3)
createplot3 <- function(powerdata) {
  with(powerdata, plot(datetime, Sub_metering_1, type="l",
                       xlab = "", ylab="Energy sub metering",
                       cex.lab = 0.75, cex.axis = 0.75, xaxt="n"))
  axis(1, at=c(min(powerdata$datetime), median(powerdata$datetime),
               max(powerdata$datetime)), labels = c("Thu", "Fri", "Sat"),
       cex.axis = 0.75)
  with(powerdata, lines(datetime, Sub_metering_2, col="red"), lty = 1)
  with(powerdata, lines(datetime, Sub_metering_3, col="blue", lty = 1))
  legend("topright", lty = 1, col = c("black", "blue", "red"),
         legend = c("Sub_Metering_1", "Sub_Metering_2", "Sub_Metering_3"),
         cex = 0.6)
}

# creates plot chart #4.4
createplot4dot4 <- function(powerdata) {
  with(powerdata, plot(datetime, Global_reactive_power, type="l",
                       xlab = "datetime", ylab="Global_reactive_power",
                       cex.lab = 0.75, cex.axis = 0.5, xaxt="n"))
  axis(1, at=c(min(powerdata$datetime), median(powerdata$datetime),
               max(powerdata$datetime)), labels = c("Thu", "Fri", "Sat"),
       cex.axis = 0.75)   
}

# execute script
powerdata <- loaddata()
powerdata <- adddatetimev(powerdata)

# set how many charts to display
par(mfrow = c(2, 2), mar = c(4, 5, 1, 1))

# draw all of the charts
createplot2(powerdata)
createplot4dot2(powerdata)
createplot3(powerdata)
createplot4dot4(powerdata)

# create PNG file
dev.copy(png,'plot4.png')
dev.off()
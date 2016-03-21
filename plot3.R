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

# creates plot chart #3
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

# execute script
powerdata <- loaddata()
powerdata <- adddatetimev(powerdata)
createplot3(powerdata)

# create PNG file
dev.copy(png,'plot3.png')
dev.off()

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

# draws histogram, plot #1
createplot1 <- function(powerdata) {
  hist(powerdata$Global_active_power, main="Global Active Power",
       xlab="Global Active Power (kilowatts)", col="red", cex.lab=0.75,
       cex.axis=0.60)
}

# execute script
powerdata <- loaddata()
createplot1(powerdata)

# create PNG file
dev.copy(png,'plot3.png')
dev.off()

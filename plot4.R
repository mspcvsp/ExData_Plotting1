# Generates the following combined plot:
# - Global average power as a function of time
# - Voltage as a function of time
# - Energy sub-metering as a function of time
# - Global_reactive_power as a function of time
#
# from the University of California, Irvine Machine Learning Repository 
# Individual Household Electric Power Consumption data set as a function of 
# time.
# 
# http://archive.ics.uci.edu/ml/datasets/
#   Individual+household+electric+power+consumption
#
# Bache, K. & Lichman, M. (2013). UCI Machine Learning Repository 
#   [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, 
#   School of Information and Computer Science.
#
# R programming style guide reference:
# -----------------------------------
# https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml

# Reset R environment
# http://stackoverflow.com/questions/14187048/
#       r-language-clean-variables-and-close-connections
closeAllConnections()
rm(list=ls())

library(lubridate)

initializeDataFileRows <- function(dataFilePath,
                                   startDate,
                                   endDate) {
    #---------------------------------------------------------------------------
    # Initializes a vector that contains the the household_power_consumption.txt 
    # rows corresponding to a specific date range
    #
    # Args:
    #   dataFilePath: String that specifies the path to
    #   household_power_consumption.txt
    #
    #   startDate: String that specifies the start date in the "Year Month Date" 
    #   format
    #
    #   endDate: String that specifies the end date in the "Year Month Date" 
    #   format
    #
    # Return Value:
    #   dataFileRows: Vector that contains the the 
    #   household_power_consumption.txt rows corresponding to a specific date 
    #   range
    #---------------------------------------------------------------------------
    
    # Dr. Peng's tutorial regarding how to read large tables into R
    # http://www.biostat.jhsph.edu/~rpeng/docs/R-large-tables.html
    tab5rows <- read.table(dataFilePath,
                           sep=";",
                           header = TRUE,
                           nrows = 5,
                           stringsAsFactors=FALSE)
    
    classes <- sapply(tab5rows, 
                      class)
    
    # http://stackoverflow.com/questions/5788117/
    #   only-read-limited-number-of-columns-in-r
    classes <- c("character",rep("NULL",length(classes)-1))
    
    firstColumn <- read.table(dataFilePath,
                              sep=";",
                              header = TRUE,
                              colClasses = classes)
    
    firstColumn$Date <- dmy(firstColumn$Date)
    
    dataFileRows <- which(firstColumn$Date >= ymd(startDate) & 
                              firstColumn$Date <= ymd(endDate))
    
    return(dataFileRows)
}

readHouseholdPowerConsumption <- function(dataFilePath,
                                          startDate,
                                          endDate) {
    #---------------------------------------------------------------------------
    # Reads the household power consumption data corresponding to a specific 
    # date range
    #
    # Args:
    #   dataFilePath: String that specifies the path to
    #   household_power_consumption.txt
    #
    #   startDate: String that specifies the start date in the "Year Month Date" 
    #   format
    #
    #   endDate: String that specifies the end date in the "Year Month Date" 
    #   format
    #
    # Return Value:
    #   powerConsumption: Data frame that contains the household power 
    #   consumption data corresponding to a specific date range
    #---------------------------------------------------------------------------
    dataFileRows <- initializeDataFileRows(dataFilePath,
                                           startDate,
                                           endDate)
    
    tab5rows <- read.table(dataFilePath,
                           sep=";",
                           header = TRUE,
                           nrows = 5,
                           stringsAsFactors=FALSE)
    
    powerConsumption <- read.table(dataFilePath,
                                   sep=";",
                                   stringsAsFactors=FALSE,
                                   header=FALSE,
                                   skip=dataFileRows[1],
                                   nrow=length(dataFileRows))
    
    colnames(powerConsumption) <- colnames(tab5rows)
        
    return(powerConsumption)
}

fileURL <- paste0("https://d396qusza40orc.cloudfront.net",
                  "/exdata%2Fdata%2Fhousehold_power_consumption.zip")

if (!file.exists("./Data")) {
    dir.create("./Data")
  
    # Downloading a *.zip file requires binary file (similar to downloading
    # a JPEG image)
    #
    # http://stackoverflow.com/questions/9655361/download-png-jpg-with-r
    download.file(fileURL,
                  destfile = "./Data/household_power_consumption.zip",
                  mode = 'wb')
    
    # http://www.r-bloggers.com/read-compressed-zip-files-in-r/
    unzip("./Data/household_power_consumption.zip",
          exdir="Data")
}

dataFilePath <- "./Data/household_power_consumption.txt"

powerConsumption <- readHouseholdPowerConsumption(dataFilePath,
                                                  "2007-02-01",
                                                  "2007-02-02")

powerConsumption$Date <- dmy_hms(paste(powerConsumption$Date,
                                       powerConsumption$Time))

# https://www.packtpub.com/article/creating-time-series-charts-r
#
# http://stackoverflow.com/questions/2564258/plot-2-graphs-in-same-plot-in-r
png("./figure/plot4.png",
    width=480,
    height=480)

# http://www.statmethods.net/advgraphs/layout.html
layout(matrix(c(1,2,3,4),nrow=2,ncol=2),
       widths=0.25*rep(1,4),
       heights=0.25*rep(1,4),
       respect=matrix(c(1,1,1,1),nrow=2,ncol=2))

with(powerConsumption,plot(Global_active_power ~ Date,
                           type="l",
                           xlab="",
                           ylab="Global Active Power (kilowatts)"))

with(powerConsumption,plot(Sub_metering_1 ~ Date,
                           type="l",
                           col="black",
                           xlab="",
                           ylab="Energy sub metering"))
with(powerConsumption,lines(Sub_metering_2 ~ Date,
                            type="l",
                            col="red"))
with(powerConsumption,lines(Sub_metering_3 ~ Date,
                            type="l",
                            col="blue"))

# http://stackoverflow.com/questions/10108073/
#   plot-legends-without-border-and-with-white-background
#
# Calculation of the seconds since the epoch (i.e January 1st 1970)
# ----------------------------------------------------------------------
# https://stat.ethz.ch/pipermail/r-help/2009-January/185842.html
#
# Set legend font size:
# --------------------
# http://stackoverflow.com/questions/16905535/
#   r-legend-trouble-how-to-change-the-text-size-in-legend
#
# Set plot layout:
# ---------------
# http://www.statmethods.net/advgraphs/layout.html
n <- which(powerConsumption$Date == ymd_hms("2007-02-01 16:00:00 UTC"))

legend(x=as.numeric(powerConsumption[n,"Date"]),
       y=40.25,
       lty=1,
       col=c("black","red","blue"),
       legend=c("Sub_metering_1",
                "Sub_metering_2",
                "Sub_metering_3"),
       bty="n",
       cex=0.951)

with(powerConsumption,plot(Voltage ~ Date,
                           type="l",
                           xlab="datetime",
                           ylab="Voltage"))

with(powerConsumption,plot(Global_reactive_power ~ Date,
                           type="l",
                           xlab="datetime"))
dev.off()

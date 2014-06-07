# Generates a time series plot of global active power (kilowatts) as a function
# of time from the University of California, Irvine Machine Learning Repository 
# Individual Household Electric Power Consumption Data Set
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
png("./figure/plot2.png",
    width=480,
    height=480)
with(powerConsumption,plot(Global_active_power ~ Date,
                           type="l",
                           ylab="Global Active Power (kilowatts)"))
dev.off()
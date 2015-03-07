# 
# Exploratory Data Analysis
# Project 1 - Plot 1
# March, 2015
#

# Set working directory and execute script.

# Set argument names
plotFile <- "plot1.png"
urlToDownload <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
fileDownloaded <- "exdata-data-household_power_consumption.zip"
dataFile <- "household_power_consumption.txt"
plotDataFile = "household_power_consumption_20070201-20070202.txt"

# Load the required libraries.
library(downloader)

#
# downloadData()
#
# Downloads the required data from the Irvine Machine Learning Repository,
# processes it to create a data frame suitable for plotting the assignment. 
# This data frame is saved to a file.
#
downloadData <- function(plotfile, urlToDownload, zipFile, dataFile, plotDataFile) {
    # Download data and unzip.
    download(urlToDownload, destfile=fileDownloaded)
    dateDownloaded <- date()
    unzip(fileDownloaded)
    
    # Loading the data (Note: Convert "?" to NA)
    print("Loading data...")
    pc <- read.table(dataFile, header=TRUE, sep=";", 
                     stringsAsFactors=FALSE, na.strings="?")
    
    # Create a DateTime column from Date and Time
    pc$DateTime <- as.POSIXct(strptime(paste(pc$Date, pc$Time), "%d/%m/%Y %H:%M:%S"))
    
    # Get subset with just the dates 2/1/2007 - 2/2/2007.
    print("Subsetting data...")
    pc_subset <- subset(pc, subset=(DateTime >= as.POSIXct("2007-02-01") & 
                                        DateTime < as.POSIXct("2007-02-03")))
    
    # Just out of curiosity, how many rows had NAs?
    print(paste("NAs in full dataset:", nrow(pc[!complete.cases(pc[,3:9]),])))
    print(paste("NAs in plotting dataset:", 
                nrow(pc_subset[!complete.cases(pc_subset[,3:9]),])))
    
    # Save pc_subset.
    print("Saving plot data...")
    write.table(pc_subset, plotDataFile, row.names=FALSE, sep=',', quote=FALSE)
}


# If the data prepared for plotting file exists, it can be used without having 
# to re-download and re-process the data.

if (! file.exists(plotDataFile)) {
    downloadData(plotfile, urlToDownload, zipFile, dataFile, plotDataFile)
}

# Read plotting data.
hpc <- read.table(plotDataFile, 
                  header=TRUE, sep=",", stringsAsFactors=FALSE,
                  colClasses=c(rep("character",2), rep("numeric",7), "POSIXct"))

# Create plot.
par(mfrow = c(1,1))
png(plotFile, height=480, width=480)

with(hpc, hist(Global_active_power, col="red", cex.axis=0.75, cex.lab=0.75,
               xlab="Global Active Power (kilowatts)", main="Global Active Power"))

dev.off()


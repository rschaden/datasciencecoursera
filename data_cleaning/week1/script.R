library(XML)
library(xlsx)

if(!dir.exists("data")) {
    dir.create("data")
}

downloadIfNotExists <- function(fileName, fileUrl) {
    if (!file.exists(fileName)) {
        download.file(fileUrl, destfile = fileName)
        dateDownloaded <- date()
        print(paste("Downloaded at", dateDownloaded))
    }
}

question1 <- function() {
    fileName <- "./data/housing_idaho.csv"
    fileUrl <-
        'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv'

    downloadIfNotExists(fileName, fileUrl)
    
    data <- read.csv(fileName)
    
    # Question 1:
    # VAL is the property value, 24 means $1,000,000+
    sum(data$VAL == 24, na.rm = TRUE)
}

question3 <- function() {
    fileName <- "./data/natural_gas_aquisition_program.xlsx"
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
    
    downloadIfNotExists(fileName, fileUrl)
    
    rowIndex <- 18:23
    colIndex <- 7:15
    dat <- read.xlsx(fileName, sheetIndex = 1, colIndex = colIndex, rowIndex = rowIndex)
    sum(dat$Zip*dat$Ext,na.rm=T)
}

question4 <- function() {
    fileName <- "./data/baltimore_restaurants.xml"
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
    
    downloadIfNotExists(fileName, fileUrl)
    
    document <- xmlTreeParse(fileName, useInternal=TRUE)
    length(xpathSApply(document, "//row[zipcode = '21231']", xmlValue))
}

question1()
question3()
question4()
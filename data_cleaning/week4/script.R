library(dplyr)
library(quantmod)
library(lubridate)

if (!dir.exists("data")) {
    dir.create("data")
}

downloadIfNotExists <- function(fileName, fileUrl) {
    if (!file.exists(fileName)) {
        download.file(fileUrl, destfile = fileName)
        dateDownloaded <- date()
        print(paste("Downloaded at", dateDownloaded))
    }
}

mergedGDPEducationData <- function()
{
    GDPfile <- "./data/gdp.csv"
    GDPURL <-
        "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
    
    Educationfile <- "./data/education.csv"
    EducationURL <-
        "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
    
    downloadIfNotExists(GDPfile, GDPURL)
    downloadIfNotExists(Educationfile, EducationURL)
    
    gdp <- read.csv(GDPfile, skip = 4, nrows = 190)
    education <- read.csv(Educationfile)
    
    merge(gdp, education, by.x = "X", by.y = "CountryCode")
}

question1 <- function() {
    fileName <- "./data/american_community_survey.csv"
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
    
    downloadIfNotExists(fileName, fileUrl)
    
    data <- read.csv(fileName)
    
    strsplit(names(data), "wgtp")[[123]] 
}

question2 <- function() {
    fileName <- "./data/gdp.csv"
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
    
    downloadIfNotExists(fileName, fileUrl)
    
    data <- read.csv(fileName, skip = 4, nrows = 190)
    
    mean(as.numeric(gsub(",", "", data$X.4)))
}

question4 <- function() {
    mergedData <- mergedGDPEducationData()
    fiscalYears <- grep("^Fiscal year end: [A-Za-z]+[0-9]*", data$Special.Notes, value = TRUE)
    length(grep("June", fiscalYears))
}

question5 <- function() {
    amzn = getSymbols("AMZN",auto.assign=FALSE)
    sampleTimes = index(amzn)
    
    dates <- as.Date(data, "%Y-%M-%D")
    
    print(paste("In Year 2012:", sum(year(dates) == 2012)))
    
    print(paste("Mondays in 2012:", sum(year(dates) == 2012 & wday(dates, week_start = 1) == 1)))
    
    dates
}


question1()
question2()
question4()
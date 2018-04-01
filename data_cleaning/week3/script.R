library(jpeg)
library(dplyr)

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
    fileUrl <-
        'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv'
    
    downloadIfNotExists(fileName, fileUrl)
    
    data <- read.csv(fileName)
    
    agricultureLogical <- data$ACR == 3 & data$AGS == 6
    
    head(which(agricultureLogical), n = 3)
}

question2 <- function() {
    fileName <- "./data/jeff.jpg"
    fileUrl <-
        "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
    
    downloadIfNotExists(fileName, fileUrl)
    
    data <- readJPEG(fileName, native = TRUE)
    
    quantile(data, probs = c(0.3, 0.8))
}

question3 <- function() {
    mergedData <- mergedGDPEducationData()
    
    print(paste("Number of matches:", nrow(mergedData)))
    
    orderedData <- arrange(mergedData, desc(X.1))
    print(paste("13th country", orderedData[13, "Long.Name"]))
    orderedData
}

question4 <- function() {
    mergedData <- mergedGDPEducationData()
    
    mergedData %>% group_by(Income.Group) %>% summarize(avg.gdp.ranking = mean(X.1))
}


question5 <- function()
{
    mergedData <- mergedGDPEducationData()
    
    mergedData$gdp.quantile <- cut(mergedData$X.1, 5)
    table(mergedData$Income.Group, mergedData$gdp.quantile)
}

question1()
question2()
data <- question3()
question4()
question5()
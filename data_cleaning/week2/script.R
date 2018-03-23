library(jsonlite)
library(sqldf)

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
    fileName <- "./data/jtleek_github_repos.json"
    fileUrl <-
        'https://api.github.com/users/jtleek/repos'
    
    downloadIfNotExists(fileName, fileUrl)
    
    jsonData <- fromJSON(fileName)
    
    jsonData[jsonData$name == "datasharing", ]$created_at
}

question2 <- function() {
    fileName <- "./data/american_community_survey.csv"
    fileUrl <-
        'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv'
    
    downloadIfNotExists(fileName, fileUrl)
    
    acs <- read.csv(fileName)
    
    print(summary(acs[acs$AGEP < 50, ]$pwgtp1))
    print(summary(sqldf("select pwgtp1 from acs where AGEP < 50")))
}

question3 <- function() {
    fileName <- "./data/american_community_survey.csv"
    fileUrl <-
        'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv'
    
    downloadIfNotExists(fileName, fileUrl)
    
    acs <- read.csv(fileName)
    
    print(summary(unique(acs$AGEP)))
    print(summary(sqldf("select distinct AGEP from acs")))
}

question4 <- function() {
    con <- url("http://biostat.jhsph.edu/~jleek/contact.html")
    htmlCode <- readLines(con)
    close(con)
    paste(nchar(htmlCode[10]), nchar(htmlCode[20]), nchar(htmlCode[30]), nchar(htmlCode[100]))
}

question5 <- function() {
    fileName <- "./data/weekly_sst.for"
    fileUrl <-
        'https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for'
    
    downloadIfNotExists(fileName, fileUrl)
    
    # values with a minus indicate columns that should be skipped
    widths = c(-1, 9, -5, 4, 4, -5, 4, 4, -5, 4, 4, -5, 4, 4)
    data <- read.fwf(fileName, widths =  widths, skip = 4)
    sum(data$V4)
}

question1()
question2()
question3()
question4()
question5()
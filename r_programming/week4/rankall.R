rankbystate <- function(data, outcome.column, num) {
    ordered.by.outcome <- data[order(data[, outcome.column], data[, 2]), ]
    
    if(num == "best") {
        num <- 1
    } else if(num == "worst") {
        num <- sum(!is.na(ordered.by.outcome[, outcome.column]))
    }
    
    list(hospital=ordered.by.outcome$Hospital.Name[num], state=ordered.by.outcome$State[1])
}

rankall <- function(outcome, num = "best") {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    valid.outcomes = c("heart attack", "heart failure", "pneumonia")
    outcome.columns = c(11, 17, 23)
    if(!(outcome %in% valid.outcomes)) {
        stop("invalid outcome")
    }
    
    outcome.column <- outcome.columns[match(outcome, valid.outcomes)]
    data[, outcome.column] <- as.numeric(data[, outcome.column])
    
    result <- lapply(split(data, data$State), rankbystate, outcome.column, num)
    do.call(rbind, lapply(result, data.frame, stringsAsFactors=FALSE))
}
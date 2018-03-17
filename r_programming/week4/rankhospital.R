rankhospital <- function(state, outcome, num = "best") {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    filtered.by.state = subset(data, data$State == state)
    
    if(nrow(filtered.by.state) == 0) {
        stop("invalid state")
    }
    
    valid.outcomes = c("heart attack", "heart failure", "pneumonia")
    outcome.columns = c(11, 17, 23)
    if(!(outcome %in% valid.outcomes)) {
        stop("invalid outcome")
    }
    
    outcome.column <- outcome.columns[match(outcome, valid.outcomes)]
    filtered.by.state[, outcome.column] <- as.numeric(filtered.by.state[, outcome.column])
    ordered.by.outcome <- filtered.by.state[order(filtered.by.state[, outcome.column], filtered.by.state[, 2]), ]
    
    if(num == "best") {
        num <- 1
    } else if(num == "worst") {
        num <- sum(!is.na(ordered.by.outcome[, outcome.column]))
    }
    
    ordered.by.outcome$Hospital.Name[num]
}
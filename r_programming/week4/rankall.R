rankall <- function(outcome, num = "best") {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    states = unique(data$State)
    states <- states[order(states)]
    length <- length(states)
    
    result <- data.frame("hospital" = character(length), "state" = character(length), stringsAsFactors=FALSE)

    valid.outcomes = c("heart attack", "heart failure", "pneumonia")
    outcome.columns = c(11, 17, 23)
    if(!(outcome %in% valid.outcomes)) {
        stop("invalid outcome")
    }
    
    outcome.column <- outcome.columns[match(outcome, valid.outcomes)]
    data[, outcome.column] <- as.numeric(data[, outcome.column])
    

    
    for(i in seq_along(states)) {
        state <- states[i]
        filtered.by.state = subset(data, data$State == state)
        ordered.by.outcome <- filtered.by.state[order(filtered.by.state[, outcome.column], filtered.by.state[, 2]), ]
        
        best.hospital <- if(num == "best") {
            num <- 1
        } else if(num == "worst") {
            num <- sum(!is.na(ordered.by.outcome[, outcome.column]))
        }
        
        result[i, ] <- c(ordered.by.outcome$Hospital.Name[num], state)
    }
    
    result
}
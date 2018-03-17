best <- function(state, outcome) {
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
    complete.cases <- na.omit(filtered.by.state)
    
    current.min <- max(complete.cases[outcome.column])
    best.hospital <- ""
    
    for(i in 1:nrow(complete.cases))
    {
        mortality.rate <- complete.cases[i, outcome.column]
        hospital.name <- complete.cases[i, "Hospital.Name"]
        
        if((mortality.rate < current.min) || (mortality.rate == current.min && hospital.name < best.hospital)) { 
            current.min <- mortality.rate
            best.hospital <- hospital.name
        }
    }
    
    best.hospital
}
complete <- function(directory, id=1:332) {
    data <- data.frame()
    files <- list.files(directory)
    
    for(i in id) {
        file = files[i]
        path = file.path(directory, file)
        
        current <- read.csv(path)
        complete.cases <- sum(complete.cases(current))
        
        data <- rbind(data, c(i, complete.cases))
    }
    
    colnames(data) <- c("id", "nobs")
    data
}
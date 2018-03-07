pollutantmean <- function(directory, pollutant, id=1:332) {
    files = list.files(directory)[id]
    
    for(i in seq_along(files)) {
        path = file.path(directory, files[i])
        
        if(i == 1) {
            data <- read.csv(path)
        } else {
            current <- read.csv(path)
            data <- rbind(data, current)
        }
    }

    mean(data[, pollutant], na.rm = TRUE)
}

pollutantmean <- function(directory, pollutant, id=1:332) {
    for(i in seq_along(id)) {
        file_name <- paste(formatC(id[i], width = 3, format = "d", flag = "0"), ".csv", sep = "")
        path <- file.path(directory, file_name)
        
        if(i == 1) {
            data <- read.csv(path)
        } else {
            current <- read.csv(path)
            data <- rbind(data, current)
        }
    }

    mean(data[, pollutant], na.rm = TRUE)
}

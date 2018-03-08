corr <- function(directory, threshold = 0) {
    cc <- complete(directory)
    filtered <- subset(cc, nobs > threshold)
    
    files <- list.files(directory)
    
    filtered.ids <- filtered$id
    result <- vector("numeric", length = length(filtered.ids))

    for(i in seq_along(filtered.ids)) {
        file = files[filtered.ids[i]]
        path = file.path(directory, file)

        current <- read.csv(path)
        correlation <- cor(current$sulfate, current$nitrate, use="complete.obs")

        result[i] <- correlation
    }
    
    result
}
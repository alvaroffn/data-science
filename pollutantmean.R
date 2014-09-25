pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    polmean <- 0
    for(monitor in id){
        monitor <- formatC(monitor, digits = 2, flag = 0)
        filename = paste(directory, "/", monitor, ".csv", sep = "")
        data <- read.csv(filename)
        poltemp <- data[pollutant][!is.na(data[pollutant])]
        polmean <- c(polmean, poltemp)
    }
    round(mean(polmean), digits = 3)
}
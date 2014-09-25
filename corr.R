corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    cors <- c()
    files <- list.files("specdata")
    for(file in files){
        filename = paste(directory, "/", file, sep = "")
        data <- read.csv(filename)
        ccs <- sum(complete.cases(data))
        if(ccs > threshold){
            m<-data[complete.cases(data),]
            ms<-matrix(m["sulfate"][,])
            mn<-matrix(m["nitrate"][,])
            cors <- c(cors,cor(mn,ms)[1,1])
        }
    }
    cors
}
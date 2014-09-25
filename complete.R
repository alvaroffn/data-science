complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    l <- list()
    attributes(l) <- names(c("id","nobs"))
    files <- list.files("specdata")
    ids<-paste(formatC(id,digits=2,flag=0),".csv",sep="")
    for(file in files){
        filename = paste(directory, "/", file, sep = "")
        data <- read.csv(filename)
        l[["id"]] <- c(l[["id"]],data["ID"][1,])
        l[["nobs"]] <- c(l[["nobs"]],sum(complete.cases(data)))
    }
    l2 <- list()
    attributes(l2) <- names(c("id","nobs"))
    for(monitor in id){
        #l2<-append(l2,sapply(lista,"[[",monitor))
        l2[["id"]] <- c(l2[["id"]],sapply(lista["id"],"[[",monitor))
        l2[["nobs"]] <- c(l2[["nobs"]],sapply(lista["nobs"],"[[",monitor))
    }
    data.frame(l2)
}
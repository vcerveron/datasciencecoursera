complete <- function(directory, id=1:332) {
    data <- data.frame(id=numeric(), nobs=numeric())
    for(idn in id) {
        datap <- read.csv(paste0(directory,"/",formatC(idn,width=3,flag="0"),".csv"))
        completos <- complete.cases(datap)
        print(sum(completos))
        data <- rbind(data, data.frame(id=idn, nobs=sum(complete.cases(datap))))
    }
    data
}
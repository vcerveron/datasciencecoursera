pollutantmean <- function(directory, pollutant, id=1:332) {
  data <- NULL
  for(idn in id) {
    datap <- read.csv(paste0(directory,"/",formatC(idn,width=3,flag="0"),".csv"))
    data <- rbind(data, datap)
  }
  mean(data[,pollutant], na.rm=TRUE)
}
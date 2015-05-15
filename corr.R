corr <- function(directory, threshold = 0) {
  v <- numeric(0)
  filenames <- list.files(directory, full.names=TRUE)
  for (f in filenames) {
    datap <- read.csv(f)
    nobs=sum(complete.cases(datap))
    if(nobs>threshold) {
      r <- cor(datap[,"sulfate"], datap[,"nitrate"], use="complete.obs")
      v <- c(v, r)
    }
  }
  v
}
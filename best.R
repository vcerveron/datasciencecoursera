best <- function(state, outcome) {
  ## Read the data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state is valid
  if (!state %in% unique(data[, 7]))
    stop("invalid state")
  
  ## Check that outcome is valid
  if(outcome == 'heart attack')
    col <- 11
  else if(outcome == 'heart failure')
    col <- 17
  else if(outcome == 'pneumonia')
    col <- 23
  else
    stop("invalid outcome")
  
  ## Return hospital name in the state and 30-day death rate for outcome
  statedata = data[data$State == state, c(2, col)]
  ##statedata[,2] <- as.numeric(statedata[,2])
  ## Get hospital name with minimum 30-day death rate
  statedata[which.min(statedata[, 2]), 1]
}
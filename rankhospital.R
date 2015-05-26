rankhospital <- function(state, outcome, num = "best") {
  ## Read the data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state is valid
  if (!state %in% unique(data[, 7]))
    stop("invalid state")
  
  ## Check that outcome is valid
  if(outcome == 'heart attack')
    col = 11
  else if(outcome == 'heart failure')
    col = 17
  else if(outcome == 'pneumonia')
    col = 23
  else
    stop("invalid outcome")
  
  ##data[, col] = as.numeric(data[, col])
  statedata = data[data[, 7] == state, c(2, col)]
  statedata = na.omit(statedata)
  numhospital = nrow(statedata)
  
  switch(num, best = {
    num = 1
  }, worst = {
    num = numhospital
  })
  if (num > numhospital) {
    return(NA)
  }

  ## Return hospital name in that state with the given rank 30-day death rate
  ol = order(statedata[, 2], statedata[, 1])
  statedata[ol, ][num, 1]
}
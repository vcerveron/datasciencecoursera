rankall <- function(outcome, num = "best") {
  ## Read the data
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  states = unique(dat[, 7])
  
  ## Check that outcome is valid
  if(outcome == 'heart attack')
    col <- 11
  else if(outcome == 'heart failure')
    col <- 17
  else if(outcome == 'pneumonia')
    col <- 23
  else
    stop("invalid outcome")
  
  dat[, col] = as.numeric(dat[, col])
  ##select hospital name, state and outcome only
  dat = dat[,c(2,7,col)]
  dat = na.omit(dat)
  
  ##function to get the num ranked hospital in a state 
  rank_in_state <- function(state) {
    df = dat[dat[, 2] == state, ]
    nhospital = nrow(df)
    switch(num, best = {
      num = 1
    }, worst = {
      num = nhospital
    })
    if (num > nhospital) {
      result = NA
    }
    o = order(df[, 3], df[, 1])
    result = df[o, ][num, 1]
    c(result, state)
  }
  
  output = do.call(rbind, lapply(states, rank_in_state))
  output = output[order(output[, 2]), ]
  rownames(output) = output[, 2]
  colnames(output) = c("hospital", "state")
  data.frame(output)
}
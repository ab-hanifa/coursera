## Finding best hospital

best <- function(state, outcome) {
  hospital_dat <- read.csv("outcome-of-care-measures.csv", 
                           colClasses = "character") 
  
  mydat <- hospital_dat[, c(11, 17, 23)] # selecting the necessary cols
  mydat <- data.frame(apply(mydat, 2, as.numeric)) # convert character to numeric
  names(mydat)<- c("heart attack", "heart failure", "pneumonia")
  mydat <- cbind(hospital_dat[, c(2, 7)], mydat) # necesaary columns all togather
  
  if(any(mydat$State == state)) { # If the given state in the data then works
    state_dat <- subset(mydat, State == state)
  }
  else stop("Invalid state") # throughs an error if the given state is not in the state
  
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("Invalid outcome")   # throughs an error if the outcome is other than these
  }
  
  else { 
    m <- min(state_dat[, eval(outcome)], na.rm = TRUE) # calculates the minimum value of the outcome
    min_out <- which(state_dat[, eval(outcome)] == m) # logical vector. true when the row is minimum
    best_hptl <- state_dat[min_out, 1] # select rows which equal to the minimum value
    
    return(sort(best_hptl)[1])
  }
}

best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("MD", 'hert failure')


## Ranking hospital

rankhospital <- function(state, outcome, num) {
  
  hospital_dat <- read.csv("outcome-of-care-measures.csv", 
                           colClasses = "character")
  
  mydat <- hospital_dat[, c(11, 17, 23)]
  mydat <- data.frame(apply(mydat, 2, as.numeric))
  names(mydat)<- c("heart attack", "heart failure", "pneumonia")
  mydat <- cbind(hospital_dat[, c(2, 7)], mydat)
  
  if(any(mydat$State == state)) {
    state_dat <- subset(mydat, State == state)
  }
  else stop("Invalid state")
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    
    stop("Invalid outcome") # Same as before
    
  }
  
  else{
    
    outcm <- state_dat[order(state_dat[, eval(outcome)], state_dat[, 1]), ]
    # orders the oucome variable and Hospital name
  }
  
  if(!is.numeric(num) & num == "best") {
    return(outcm[, 1][1])
    # give the having minimum mortality hospital 
  }
  
  else if(!is.numeric(num) & num == "worst") {
    worst_hopital <- outcm[order(outcm[, eval(outcome_var)],
                                 outcm[, 1], decreasing = TRUE), ]
    # For having worst, we use descending ordering. That's why TRUE
    return(worst_hopital[, 1][1])
  }
  else if(is.numeric(num) & num > nrow(state_dat)) {
    return(NA)
  }
  else return(outcm[, 1][num])
}

rankhospital("TX", "heart failure", 4)  

rankhospital("MD", "heart attack", "best")

rankhospital("TX", "heart failure", 500000)

## Ranking all hospital 


rankall <- function(outcome, num) {
  
  dat_hosp <- read.csv("week4/outcome-of-care-measures.csv",
                       colClasses = "character")
  
  dat_h <- dat_hosp[, c(11, 17, 23)]
  dat_h <- data.frame(apply(dat_h, 2, as.numeric))
  names(dat_h) <- c("heart attack", "heart failure", "pneumonia")
  dat_h <- cbind(dat_hosp[, c(2, 7)], dat_h)
  
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("Invalid outcome")
  }
  
  # tapply is used to get the state specific rank of hospitals for different outcome
  hname_st <- tapply(dat_h, dat_h$State, function(x, num) { 
    x <- x[order(x[, eval(outcome)], x[, 1]), ]
    # order the outcome
    if(!is.numeric(num) & num == "best"){
      return(x[, 1][1])
    }
    else if(!is.numeric(num) & num == "worst") {
      x <- x[order(x[, eval(outcome)], x[, 1], decreasing = TRUE), ]
      return(x[, 1][1])
    }
    else return(x[, 1][num])
    
  }, num) # Here, everything is same as the previous rankhospital() function.
  
  return(data.frame(hospital = unlist(hname_st), state = names(hname_st)))
}

rankall("heart failure", "worst")


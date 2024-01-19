## Finding best hospital

best <- function(state, outcome) {
  hospital_dat <- read.csv("/media/abuhanifa/StudyFolder/coursera/week4/outcome-of-care-measures.csv",colClasses = "character")
  
  mydat <- hospital_dat[, c(11, 17, 23)]
  mydat <- data.frame(apply(mydat, 2, as.numeric))
  names(mydat)<- c("heart attack", "heart failure", "pneumonia")
  mydat <- cbind(hospital_dat[, c(2, 7)], mydat)
  
  if(any(mydat$State == state)) {
    state_dat <- subset(mydat, State == state)
  }
  else stop("Invalid state")
  
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("Invalid outcome")
  }
  
  else { 
    m <- min(state_dat[, eval(outcome)], na.rm = TRUE)
    min_out <- which(state_dat[, eval(outcome)] == m)
    best_hptl <- state_dat[min_out, 1]
    
    return(sort(best_hptl)[1])
  }
}

best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("MD", 'hert failure')

## Ranking hospital


rankhospital <- function(state, outcome_var, num) {
  
  hospital_dat <- read.csv("/media/abuhanifa/StudyFolder/coursera/week4/outcome-of-care-measures.csv",colClasses = "character")
  
  mydat <- hospital_dat[, c(11, 17, 23)]
  mydat <- data.frame(apply(mydat, 2, as.numeric))
  names(mydat)<- c("heart attack", "heart failure", "pneumonia")
  mydat <- cbind(hospital_dat[, c(2, 7)], mydat)
  
  if(any(mydat$State == state)) {
    state_dat <- subset(mydat, State == state)
  }
  else stop("Invalid state")
  
  if(!outcome_var %in% c("heart attack", "heart failure", "pneumonia")) {
    
    stop("Invalid outcome")
    
  }
  
  else{
    
    outcm <- state_dat[order(state_dat[, eval(outcome_var)], state_dat[, 1]), ]
  }
  
  if(!is.numeric(num) & num == "best") {
    return(outcm[, 1][1])
  }
  
  else if(!is.numeric(num) & num == "worst") {
    worst_hopital <- outcm[order(outcm[, eval(outcome_var)],
                                 outcm[, 1], decreasing = TRUE), ]
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
  
  dat_hosp <- read.csv("/media/abuhanifa/StudyFolder/coursera/week4/outcome-of-care-measures.csv",colClasses = "character")
  
  dat_h <- dat_hosp[, c(11, 17, 23)]
  dat_h <- data.frame(apply(dat_h, 2, as.numeric))
  names(dat_h) <- c("heart attack", "heart failure", "pneumonia")
  dat_h <- cbind(dat_hosp[, c(2, 7)], dat_h)
  
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("Invalid outcome")
  }
  
  hname_st <- tapply(dat_h, dat_h$State, function(x, num) { 
    x <- x[order(x[, eval(outcome)], x[, 1]), ]
    if(!is.numeric(num) & num == "best"){
      return(x[, 1][1])
    }
    else if(!is.numeric(num) & num == "worst") {
      x <- x[order(x[, eval(outcome)], x[, 1], decreasing = TRUE), ]
      return(x[, 1][1])
    }
    else return(x[, 1][num])
    
  }, num)
  
  return(data.frame(hospital = unlist(hname_st), state = names(hname_st)))
}

rankall("heart failure", "worst")


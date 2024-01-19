outcome <- read.csv("/media/abuhanifa/StudyFolder/coursera/week4/outcome-of-care-measures.csv",colClasses = "character")
head(outcome)

names(outcome)

outcome <- outcome[, c(2, 7, 11, 17, 23)]

names(outcome)[3:5] <- c("Heart.Attack", "Heart.Failure", "Pneumonia") 

outcome$Heart.Attack <- as.numeric(outcome$Heart.Attack)

outcome$Heart.Failure <- as.numeric(outcome$Heart.Failure)

outcome$Pneumonia <- as.numeric(outcome$Pneumonia)

names(outcome)

st_sub <- subset(outcome, State == "TX") 

#out_c <- na.omit(st_sub)

out_c <- out_c[order(out_c$Heart.Failure, out_c$Hospital.Name), ]
out_c <- st_sub[!is.na(st_sub$Heart.Failure), ]
out_c[, 1][4]

out_c$Rank <- data.table::frank(out_c$Heart.Failure, ties.method = "dense") 

sort(out_c[out_c$Rank == 4, 1])


rankhospital <- function(state, outcome, num) {
  
  hospital_dat <- read.csv("/media/abuhanifa/StudyFolder/coursera/week4/outcome-of-care-measures.csv",colClasses = "character")
  
  mydat <- hospital_dat[, c(11, 17, 23)]
  mydat <- data.frame(apply(mydat, 2, as.numeric))
  names(mydat)<- c("heart attack", "heart failure", "pneumonia")
  mydat <- cbind(hospital_dat[, c(2, 7)], mydat)
  
  if(any(mydat$State == state)) {
    state_dat <- subset(mydat, State == state)
  }
  else stop("Invalid state")
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    
    stop("Invalid outcome")
    
  }
  
  else{
  
    outcm <- state_dat[!is.na(state_dat[, eval(outcome)]), ]
    outcm <- outcm[order(outcm[, eval(outcome)], outcm[, 1]), ]
  }
  
    if(!is.numeric(num) & num == "best") {
      return(outcm[, 1][1])
    }
    
    else if(!is.numeric(num) & num == "worst") {
      worst_hopital <- outcm[order(outcm[, eval(outcome)],
                                      outcm[, 1], decreasing = TRUE), ]
      return(worst_hopital[, 1][1])
    }
  else if(is.numeric(num) & num > nrow(state_dat)) {
    return(NA)
  }
    else return(outcm[, 1][num])
  }

rankhospital("TX", "heart failure", 4)  

rankhospital("MD", "heart attack", "worst")
rankhospital("NC", "heart attack", "worst")
rankhospital("TX", "pneumonia", 10)

rankhospital("NY", "heart attack", 7)

rankhospital("WA", "heart attack", 7)

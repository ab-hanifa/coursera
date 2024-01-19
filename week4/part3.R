hospital_dat <- read.csv("/media/abuhanifa/StudyFolder/coursera/week4/outcome-of-care-measures.csv",colClasses = "character")

mydat <- hospital_dat[, c(11, 17, 23)]
mydat <- data.frame(apply(mydat, 2, as.numeric))
names(mydat)<- c("heart attack", "heart failure", "pneumonia")
mydat <- cbind(hospital_dat[, c(2, 7)], mydat)
mydat

#split_dat <- split(mydat, mydat$State)
#split_dat

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

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)

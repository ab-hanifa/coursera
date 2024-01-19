outcome <- read.csv("/media/abuhanifa/StudyFolder/coursera/week4/outcome-of-care-measures.csv",colClasses = "character")
head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])



table(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)

names(outcome)

table(outcome$State)

rev_out <- outcome[, c(2, 7, 11, 17, 23)]

table(outcome$Hospital.Name)

subset(rev_out, 
       Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm = TRUE) & State == "TX")

library(tidyverse)

rev_out |> 
  rename(hattack = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, 
         heartf = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
         pnenumonia = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) |> 
  filter(State == "TX") |> 
  filter(hattack == min(hattack, na.rm = TRUE))


d <- subset(rev_out, State == "TX")

d1 <- subset(d, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm = TRUE))
d1$Hospital.Name

## 
best <- function(state, outcome) {
  
  data <- read.csv("/media/abuhanifa/StudyFolder/coursera/week4/outcome-of-care-measures.csv",colClasses = "character")
  
  if(any(data$State == state)) {
  state_dat <- subset(data, State == state)
  }
  else stop("Invalid state")
  
  if(outcome == "heart attack") {
    
    heart_attack <- subset(state_dat, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm = TRUE))
    return(sort(heart_attack$Hospital.Name)[1])
  }
  
  else if(outcome == "heart failure") {
    state_dat$heart_failure <- sapply(state_dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, as.numeric)
    heart_fail <- subset(state_dat, heart_failure == min(heart_failure, na.rm = TRUE))
    return(sort(heart_fail$Hospital.Name)[1])
  }
  
  else if(outcome == "pneumonia") {
    state_dat$pneumonia <- sapply(state_dat$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, as.numeric)
    mort.pneumonia <- subset(state_dat, pneumonia == min(pneumonia, na.rm = TRUE))
    return(sort(mort.pneumonia$Hospital.Name)[1])
  }
  
  else stop("Invalid outcome")
}
best("SC", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("NY", "pneumonia")
best("BB", "heart attack")
best("MD", 'hert failure')

## 

outcome <- read.csv("/media/abuhanifa/StudyFolder/coursera/week4/outcome-of-care-measures.csv",colClasses = "character")
head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])


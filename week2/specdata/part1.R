pollutantmean <- function(pullutant, id) {
  list_f <- list.files(path = ".", pattern = "*.csv")
  
  df <- lapply(id, function(x) read.csv(list_f[x], header = TRUE))
  
  combined_dat <- do.call(rbind, df)
  
  m <- mean(combined_dat[[pullutant]], na.rm = TRUE) 
  # we can use  combined_dat[, eval(pullutant)]

  return(m)
}

pollutantmean("nitrate", 1:332)
pollutantmean("sulfate", 34)
pollutantmean("nitrate", 70:72)
##

## part2

complete <- function(id){
  list_f <- list.files(path = ".", pattern = "*.csv")
  
  df <- lapply(id, function(x) read.csv(list_f[x],
                                                        header = TRUE))
  
  complt <- cbind(ID = id, 
             nobs = sapply(1:length(id), 
                           function(x) sum(complete.cases(df[[x]]))))
  return(complt)
}

## Function evaluation
min(complete(1:332)[, 2])
complete(54)

cc <- complete(c(6, 10, 20, 34, 100, 200, 310))
print(cc)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete(332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

## 

## Third part

corr <- function(thresold = 0){
  list_f <- list.files(path = ".", pattern = "*.csv")
  
  df <- lapply(1:332, function(x) read.csv(list_f[x],
                                           header = TRUE))
  cor_data <- sapply(1:332, function(x) {cor(df[[x]]$sulfate,
                                             df[[x]]$nitrate,
                                             use = "na.or.complete")})
  data_cor <- data.frame(complete(1:332), cor_data)
  
  unlist(data_cor[data_cor$nobs > thresold, 2])
  
}
## Function evaluation

cr <- corr()                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)


cr <- corr(2000)                
n <- length(cr)                
cr <- corr(1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
corr(456)

cr <- corr(129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr(2000)                
n <- length(cr)                
cr <- corr(1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

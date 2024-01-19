list_f <- list.files(path = ".", pattern = "*.csv")
list_f

df <- lapply(1, function(x) read.csv(list_f[x], header = TRUE))
View(df)

do.call(rbind, df)

## From internet

##
pollutantmean <- function(pollutant, id = 1:332){
  # locate the files:
  files <- list.files(path = ".")
  
  # read the files indicated by id:
  dfs <- list()
  for(i in id){
    dfs[[i]]<- read.csv(files[i], header = TRUE)
  }
  
  # find the mean:
  big_df <- do.call(rbind, dfs)
  mean <- mean(big_df[, eval(pollutant)], na.rm = TRUE) # The eval() function does the trick for colmun indexing
  return(mean)
}

## library(tidyverse)

## Using tidyverse
list.files(path = ".")

df <- list.files(path = ".", pattern = "*.csv")
df |> 
  map_df(~ read_csv(.))

### part2

list_f <- list.files(path = ".", pattern = "*.csv")

df <- lapply(c(2, 4, 8, 10, 12), function(x) read.csv(list_f[x], header = TRUE))

m <- cbind(id = c(2, 4, 8, 10, 12), 
           nobs = sapply(1:length(c(2, 4, 8, 10, 12)), 
                         function(x) sum(!is.na(df[[x]][, 2]) &
                                                  !is.na(df[[x]][, 3]))))
  

m

cbind(id = c(2, 4, 8, 10, 12), 
      nobs = sapply(1:length(c(2, 4, 8, 10, 12)), 
                    function(x) sum(complete.cases(df[[x]]))))

### Part - 03 # Rough

list_f <- list.files(path = ".", pattern = "*.csv")

df <- lapply(1:332, function(x) read.csv(list_f[x],
                                         header = TRUE))
View(df)

complt <- data.frame(cbind(ID = 1:332, 
                nobs = sapply(1:332, 
                              function(x) sum(complete.cases(df[[x]])))))

sub_dat <- subset(complt, nobs > 930)$ID
if(length(sub_dat) > 0){
  sapply(c(sub_dat), 
         function(x) cor(df[[x]]$sulfate, df[[x]]$nitrate, use = "complete.obs"))
}


subset(complt, nobs > 930)$ID
length(complt[complt$nobs > 2000, ]$ID)

sum(complete.cases(df[[289]]))

d <- lapply(1:332, 
       function(x) cor(df[[x]]$sulfate, df[[x]]$nitrate, use = "na.or.complete"))

d
unlist(d)

e <- sapply(1:332, 
            function(x) {
              if(sum(complete.cases(df[[x]])) <= 930) return(0)
            }
)

sum(unlist(e))

## Third final function

corr <- function(thresold){
  list_f <- list.files(path = ".", pattern = "*.csv")
  
  df <- lapply(1:332, function(x) read.csv(list_f[x],
                                           header = TRUE))
  cor_data <- data.frame(t(sapply(1:332, 
              function(x) {
                correlation <- cor(df[[x]]$sulfate,
                              df[[x]]$nitrate, 
                              use = "na.or.complete")
                complete_case <- sum(complete.cases(df[[x]]))
                return(data.frame(Cc = complete_case, Cor = correlation))
                })))
  
  unlist(cor_data[cor_data$Cc > thresold, 2])
  
}

corr(thresold = 1500)

## 

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

corr(thresold = 950)

pollutantmean <- function(directory, pullutant, id = 1:332) {
  file_path <- paste0("palce the directory where your specdata is", directory)
  list_f <- list.files(path = file_path, pattern = "*.csv")
  
  df <- lapply(id, function(x) read.csv(list_f[x], header = TRUE))
  
  combined_dat <- do.call(rbind, df)
  
  m <- mean(combined_dat[[pullutant]], na.rm = TRUE) 
  # we can use  combined_dat[, eval(pullutant)]
  
  return(m)
}

pollutantmean("specdata", "nitrate", 32)


## part2

complete <- function(directory, id){
  
  file_path <- paste0("palce the directory where your specdata is", directory)
  
  list_f <- list.files(path = file_path, pattern = "*.csv")
  
  df <- lapply(id, function(x) read.csv(list_f[x],
                                        header = TRUE))
  
  complt <- cbind(ID = id, 
                  nobs = sapply(1:length(id), 
                                function(x) sum(complete.cases(df[[x]]))))
  return(complt)
}


## Third part

corr <- function(directory, thresold = 0){
  file_path <- paste0("palce the directory where your specdata is", directory)
  
  list_f <- list.files(path = file_path, pattern = "*.csv")
  
  df <- lapply(1:332, function(x) read.csv(list_f[x],
                                           header = TRUE))
  cor_data <- sapply(1:332, function(x) {cor(df[[x]]$sulfate,
                                             df[[x]]$nitrate,
                                             use = "na.or.complete")})
  data_cor <- data.frame(complete(1:332), cor_data)
  
  unlist(data_cor[data_cor$nobs > thresold, 2])
  
}

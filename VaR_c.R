VaR_c <- function(roll) {#roll:滚动估计结果。不用输入置信水平，已设置为1%与5%
  a <- list()
  alpha <- c(0.01,0.05)
  names <- c("1%","5%")
  for(i in 1:2){
    a[[names[i]]] <- sapply(roll, quantile,prob=alpha[i])
  }
  return(a)
}

ES_c <- function(roll){
  a <- list()
  alpha <- c(0.01,0.05)
  names <- c("1%","5%")
  for(i in 1:2){
    a[[names[i]]] <- sapply(roll,GAS:: ES,prob=alpha[i])
  }
  return(a)
}
#GAS::ES(norm_roll$`000001.ss`)
#ES_c(norm_roll)

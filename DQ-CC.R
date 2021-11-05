DQ.UC.CC <- function(y.ots,VaR,models,alpha){
  UC.pval <-  CC.pval <- DQ.pval <- vector("double", length(models))
  for (j in 1:length(models)) {
    test <- GAS::BacktestVaR(data  = y.ots,
                             VaR   = VaR[,j],
                             alpha = alpha)
    
    CC.pval[j] <- test$LRcc[2]
    DQ.pval[j] <- test$DQ$pvalue
    UC.pval[j] <- test$LRuc[2]
  }
  names(UC.pval) <- names(CC.pval)<- names(DQ.pval)  <-c("norm","t","skst","ald","aparch-t")
return(data.frame(UC.pval,CC.pval,DQ.pval))
}
sh.VaR
models=modelnames
UC.pval <-  CC.pval <- DQ.pval <- vector("double", length(models))
for (j in 1:length(models)) {
  test <- GAS::BacktestVaR(data  = y.ots,
                           VaR   = VaR[,j],
                           alpha = alpha)
  
  CC.pval[j] <- test$LRcc[2]
  DQ.pval[j] <- test$DQ$pvalue
  UC.pval[j] <- test$LRuc[2]
}
names(UC.pval) <- names(CC.pval)<- names(DQ.pval)  <-c("norm","t","skst","ald","aparch-t")

#test <- GAS::BacktestVaR(data  = data.ret[1201:3600,1],
#                         VaR   = VaR[,3],
#                         alpha = 0.05)
#test$LRuc
#DQ.UC.CC(data.ret[2001:3000,1],sh.VaR,alpha = 0.01,models = modelnames)

DQ.UC.CC.garch <- function(VaR,alpha){
  UC.pval <-  CC.pval <- DQ.pval <- vector("double",3)
  ifelse(alpha==0.01,j <- 1,j <- 2)
  for(i in 1:3){
  test <- GAS::BacktestVaR(data  =  VaR[3,i][[1]],
                           VaR   =  VaR[j,i][[1]],
                           alpha = alpha)
  CC.pval[i] <- test$LRcc[2]
  DQ.pval[i] <- test$DQ$pvalue
  UC.pval[i] <- test$LRuc[2]
  }
  names(UC.pval) <- names(CC.pval)<- names(DQ.pval)  <- c("000001.ss","399001.sz","399005.sz")
  return(data.frame(UC.pval,CC.pval,DQ.pval))
}







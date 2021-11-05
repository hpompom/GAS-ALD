#回测
modelnames <- c("norm","t","skst","ald","aparch-t")
datanames <- c("000001.ss","399001.sz","399005.sz")
  back.5 <- list()
  UC.pval <-  CC.pval <- DQ.pval <- vector("double", length(modelnames))
  for(j in 1:3){
    for(i in 1:7){
      test[[i]] <- BacktestVaR(VaR[1,i][[1]][,1],data[2001:3000,1],alpha=0.01)
      CC.pval[i] <- test[[i]]$LRcc[2]
      DQ.pval[i] <- test[[i]]$DQ$pvalue
      UC.pval[i] <- test[[i]]$LRuc[2]
    }
    back.5[[datanames[j]]] <- data.frame(UC.pval,CC.pval,DQ.pval)
  }

back.5$norm$DQ
VaR[1,1][[1]][,1]

#1%
sh.VaR <- sz.VaR <- zxb.VaR <- data.frame(1:1500)
for (i  in 1:4) {
  sh.VaR[,i] <- VaR[1,i][[1]][,1]
  sz.VaR[,i] <- VaR[1,i][[1]][,2]
  zxb.VaR[,i] <- VaR[1,i][[1]][,3]
}
colnames(zxb.VaR) <- colnames(sz.VaR) <- colnames(sh.VaR)<- c("norm","t","skst","ald")
#加入aparch
#---norm---
#sh.VaR$aparch.n <- garch.n.VaR[1,1][[1]]
#sz.VaR$aparch.n <- garch.n.VaR[1,2][[1]]
#zxb.VaR$aparch.n <- garch.n.VaR[1,3][[1]]
#---t--
sh.VaR$aparch.t <- garch.t.VaR[1,1][[1]]
sz.VaR$aparch.t <- garch.t.VaR[1,2][[1]]
zxb.VaR$aparch.t <- garch.t.VaR[1,3][[1]]


DQ.UC.CC(data.ret[1501:3000,1],sh.VaR,alpha = 0.01,models = modelnames)#ald
DQ.UC.CC(data.ret[1501:3000,2],sz.VaR,alpha = 0.01,models = modelnames)#ald
DQ.UC.CC(data.ret[1501:3000,3],zxb.VaR,alpha = 0.01,models = modelnames)#ald

round(DQ.UC.CC(data.ret[1501:3000,1],sh.VaR,alpha = 0.01,models = modelnames),3)
round(DQ.UC.CC(data.ret[1501:3000,2],sz.VaR,alpha = 0.01,models = modelnames),3)
round(DQ.UC.CC(data.ret[1501:3000,3],zxb.VaR,alpha = 0.01,models = modelnames),3)
#stargazer(DQ.UC.CC(data.ret[2001:3000,1],sh.VaR,alpha = 0.01,models = modelnames))
#-aparch-
DQ.UC.CC.garch(garch.t.VaR,0.01)
DQ.UC.CC.garch(garch.n.VaR,0.01)

plot(data.ret[1501:3000,1],ylim=c(-0.2,0.2))
lines(sh.VaR[,5],col="blue")

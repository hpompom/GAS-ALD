#-loss function-#
es <- ES_c(ald_roll)
var <- VaR_c(ald_roll)
tt1 <- FZLoss(data = data.ret[1501:3000,1],VaR = var$`1%`[,1],ES = es$`1%`[,1],alpha=0.01)
mean(FZLoss(data = data.ret[,3],VaR = fix.ald.var[,3],ES = fix.ald.es[,3],alpha=0.01))

#---ql-----
sh.ql <- apply(sh.VaR, 2, LossVaR,realized=data.ret[1501:3000,1],tau=0.01)
sz.ql <- apply(sz.VaR, 2, LossVaR,realized=data.ret[1501:3000,2],tau=0.01)
zxb.ql <- apply(zxb.VaR, 2, LossVaR,realized=data.ret[1501:3000,3],tau=0.01)

cusum <- function(x1,x2){
  n <- length(x1)
  result <- c()
  for(i in 1:n){
    result <- c(result,sum(x1[1:i]-x2[1:i]))
  }
  return(result)
}
cusum(sh.ql[,1],sh.ql[,2])
apply(sh.ql, 2, cusum,x2=sh.ql[,4])

sh.diff.ql <- apply(sh.ql, 2, cusum,x2=sh.ql[,4])
sh.diff.ql <- as.data.frame(sh.diff.ql)*10e3
sh.diff.ql$data <- data$交易日期[1501:3000]
write.table(sh.diff.ql,file = "sh.qldiff数据.csv",sep=",") 

sz.diff.ql <- apply(sz.ql, 2, cusum,x2=sz.ql[,4])
sz.diff.ql <- as.data.frame(sz.diff.ql)*10e3
sh.diff.ql$data <- data$交易日期[1501:3000]
write.table(sz.diff.ql,file = "sz.qldiff数据.csv",sep=",") 

zxb.diff.ql <- apply(zxb.ql, 2, cusum,x2=zxb.ql[,4])
zxb.diff.ql <- as.data.frame(zxb.diff.ql)*10e3
zxb.diff.ql$data <- data$交易日期[1501:3000]
write.table(zxb.diff.ql,file = "zxb.qldiff数据.csv",sep=",") 


apply(sh.ql, 2, mean)*10e3
round(apply(sh.ql, 2, mean)*10e3,3)
round(apply(sz.ql, 2, mean)*10e3,3)
round(apply(zxb.ql, 2, mean)*10e3,3)
ql.loss <- data.frame(sh=round(apply(sh.ql, 2, mean)*10e3,3),sz=round(apply(sz.ql, 2, mean)*10e3,3),zxb=round(apply(zxb.ql, 2, mean)*10e3,3))
write.table(ql.loss,file = "ql数据.csv",sep=",") 

#----fzl------
FZLoss(data=data.ret[1501:3000,1],)

sh.fzl <- data.frame(1:1500)
for(i in 1:7){
  sh.fzl[,i] <- FZLoss(data=data.ret[1501:3000,1],VaR = sh.VaR[,i],ES=sh.ES[,i],alpha=0.01)
}

sz.fzl <- data.frame(1:1500)
for(i in 1:7){
  sz.fzl[,i] <- FZLoss(data=data.ret[1501:3000,2],VaR = sz.VaR[,i],ES=sz.ES[,i],alpha=0.01)
}

zxb.fzl <- data.frame(1:1500)
for(i in 1:7){
  zxb.fzl[,i] <- FZLoss(data=data.ret[1501:3000,3],VaR = zxb.VaR[,i],ES=zxb.ES[,i],alpha=0.01)
}

colnames(sh.fzl) <- rownames(ql.loss)
colnames(sz.fzl) <- rownames(ql.loss)
colnames(zxb.fzl) <- rownames(ql.loss)

sh.diff.fzl <- apply(sh.fzl, 2, cusum,x2=sh.fzl[,4])
sh.diff.fzl <- as.data.frame(sh.diff.fzl)
#sh.diff.fzl <- as.data.frame(sh.diff.ql)*10e3
sh.diff.fzl$data <- data$交易日期[1501:3000]
write.table(sh.diff.fzl,file = "sh.fzldiff数据.csv",sep=",") 

sz.diff.fzl <- apply(sz.fzl, 2, cusum,x2=sz.fzl[,4])
sz.diff.fzl <- as.data.frame(sz.diff.fzl)
sz.diff.fzl$data <- data$交易日期[1501:3000]
write.table(sz.diff.fzl,file = "sz.fzldiff数据.csv",sep=",") 

zxb.diff.fzl <- apply(zxb.fzl, 2, cusum,x2=zxb.fzl[,4])
zxb.diff.fzl <- as.data.frame(zxb.diff.fzl)
zxb.diff.fzl$data <- data$交易日期[1501:3000]
write.table(zxb.diff.fzl,file = "zxb.fzldiff数据.csv",sep=",") 


sh.fzl.m <- round(apply(sh.fzl, 2, mean),3)
sz.fzl.m <- round(apply(sz.fzl, 2, mean),3)
zxb.fzl.m <- round(apply(zxb.fzl, 2, mean),3)
fzl <- data.frame(sh=sh.fzl.m,sz=sz.fzl.m,zxb=zxb.fzl.m)

write.table(fzl,file = "fzl数据.csv",sep=",") 


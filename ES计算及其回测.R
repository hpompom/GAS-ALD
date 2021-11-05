#---------ES计算及其回测--------
#---aparch的计算-----
#计算skst的分位点
ES.c.aparch <- function(test,quantil){
  es <- c()
  for(i in 1:1500){
  es <- c(es,test$Mu[i]+test$Sigma[i]*quantil[i])
  }
  return(es)
}
#-上证-
test.sh$Sigma
test.sh <- as.data.frame(garch_roll.t$`000001.ss`)
quantil.sh <- c()
for(i in 1:1500){
  f = function(x) qdist("sstd", p=x, mu = 0, sigma = 1, 
                        skew  = test.sh$Skew[i], shape=test.sh$Shape[i] )
  quantil.sh <- c( quantil.sh,integrate(f, 0, 0.01)$value/0.01)
}
ES.aparch.sh <- ES.c.aparch(test.sh,quantil.sh)
#-深圳-
quantil.sz <- c()
test.sz <- as.data.frame(garch_roll.t$`399001.sz`)
for(i in 1:1500){
  f = function(x) qdist("sstd", p=x, mu = 0, sigma = 1, 
                        skew  = test.sz$Skew[i], shape=test.sz$Shape[i] )
  quantil.sz <- c(quantil.sz,integrate(f, 0, 0.01)$value/0.01)
}
ES.aparch.sz <- ES.c.aparch(test.sz,quantil.sz)
#-中小板-
quantil.zxb <- c()
test.zxb <- as.data.frame(garch_roll.t$`399005.sz`)
for(i in 1:1500){
  f = function(x) qdist("sstd", p=x, mu = 0, sigma = 1, 
                        skew  = test.zxb$Skew[i], shape=test.zxb$Shape[i] )
  quantil.zxb <- c( quantil.zxb,integrate(f, 0, 0.01)$value/0.01)
}
ES.aparch.zxb <- ES.c.aparch(test.zxb,quantil.zxb)
#-------GAS的ES计算------
#-----Var与ES整合------
ES <- sapply(roll, ES_c)
data.frame(ES.aparch.sh,ES.aparch.sz,ES.aparch.zxb )
colnames(ES) <- c("norm","t","skst","ald")
sh.ES <- sz.ES <- zxb.ES <- data.frame(1:1500)
for (i  in 1:4) {
  sh.ES[,i] <- ES[1,i][[1]][,1]
  sz.ES[,i] <- ES[1,i][[1]][,2]
  zxb.ES[,i] <- ES[1,i][[1]][,3]
}

sh.ES$aparch.t <- ES.aparch.sh
sz.ES$aparch.t <- ES.aparch.sz
zxb.ES$aparch.t <- ES.aparch.zxb

colnames(sh.ES) <- colnames(sz.ES) <- colnames(zxb.ES) <- c("norm","t","skst","ald","aparch-skst")
#-----------es导出-------
sh.ES$fix.ald <- fix.ald.es$sh
sz.ES$fix.ald <- fix.ald.es$sz
zxb.ES$fix.ald <- fix.ald.es$zxb

sh.ES$his <- his.es$sh
sz.ES$his <- his.es$sz
zxb.ES$his <- his.es$zxb

write.table(sh.ES,file = "上证指数ES数据.csv",sep=",") 
write.table(sz.ES,file = "深圳指数ES数据.csv",sep=",") 
write.table(zxb.ES,file = "中小板指数ES数据.csv",sep=",") 

#------var导出-------
sh.VaR$fix.ald <- fix.ald.var$sh
sz.VaR$fix.ald <- fix.ald.var$sz
zxb.VaR$fix.ald <- fix.ald.var$zxb

sh.VaR$his <- his.var$sh
sz.VaR$his <- his.var$sz
zxb.VaR$his <- his.var$zxb

write.table(sh.VaR,file = "上证指数VaR数据.csv",sep=",") 
write.table(sz.VaR,file = "深圳指数VaR数据.csv",sep=",") 
write.table(zxb.VaR,file = "中小板指数VaR数据.csv",sep=",") 


for( i in 1:3){
  print(ESTest(alpha=0.01,actual = data.ret[1501:3000,1],ES=fix.ald.es[,i],VaR=fix.ald.var[,i])$p.value)
}
ESTest(alpha=0.01,actual = data.ret[1501:3000,1],ES=fix.ald.es$sh,VaR=fix.ald.var$sh)$p.value

for(i in 1:7){
  print((ESTest(alpha=0.01,
               actual = data.ret[1501:3000,1],
               ES=sh.ES[,i],
               VaR=sh.VaR[,i],boot=TRUE,n.boot = 10000)$p.value))
}

for(i in 1:7){
  print(round(ESTest(alpha=0.01,
               actual = data.ret[1501:3000,2],
               ES=sz.ES[,i],
               VaR=sz.VaR[,i],boot=TRUE,n.boot = 10000)$p.value,3))
}

for(i in 1:7){
  print(round(ESTest(alpha=0.01,
               actual = data.ret[1501:3000,3],
               ES=zxb.ES[,i],
               VaR=zxb.VaR[,i],boot=TRUE,n.boot = 10000)$p.value,3))
}


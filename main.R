library(GAS)
library(MCS)#最优模型集合
library(zoo)
library(stargazer)#latex转换
library(tidyverse)
library(parallel)#并行
library(rugarch)#有回测ES
library(readxl)
library(pastecs)#描述性统计
library(FinTS)#ARCH效应检验
library(factorstochvol)
source("VaR_c.r")
set.seed(1234)
#----数据预处理----
data <- read_excel("~/Desktop/GAS/数据/上证-深圳-中小板.xls")
data <- data.frame(data)
data.zoo <- zoo(data[,-1],data[,1])
data.ret <- apply(data.zoo, 2, logret)
data.ret <- data.ret
colnames(data.ret) <- datanames
#----描述性统计----
#收益率可视化
par(mfrow = c(2, 3))
plot(zoo::zoo(data[,2], order.by = data[,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "Date (year)",cex.lab = 1.5,cex = 1.5)
plot(zoo::zoo(data[,3], order.by = data[,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "Date (year)",cex.lab = 1.5,cex = 1.5)
plot(zoo::zoo(data[,4], order.by = data[,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "Date (year)",cex.lab = 1.5,cex = 1.5)
plot(zoo::zoo(data.ret[,1], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "Date (year)",cex.lab = 1.5,cex = 1.5)
title(main = "000001.ss log-returns (%)", cex.main = 1.5)
plot(zoo::zoo(data.ret[,2],order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "Date (year)",cex.lab = 1.5,cex = 1.5)
title(main = "399001.sz log-returns (%)", cex.main = 1.5)
plot(zoo::zoo(data.ret[,3], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "Date (year)",cex.lab = 1.5,cex = 1.5)
title(main = "399005.sz log-returns (%)", cex.main = 1.5)
par(mfrow = c(1, 1))
#--描述性统计
stat.desc(rnorm(5000),norm = TRUE)
t(stat.desc(data.ret,norm = TRUE)[c(4,5,9,12,15,17,19,20),])#基本统计量
arch.lm.2.s <- apply(data.ret, 2, ArchTest,lags=2)%>%#arch-lm滞后2阶返回统计量
  lapply(., function(a){a$statistic})%>%
  data.frame()
arch.lm.2.p <- apply(data.ret, 2, ArchTest,lags=2)%>%#arch-lm滞后2阶返回p值
  lapply(., function(a){a$p.value})%>%
  data.frame()
data.frame(t(stat.desc(data.ret,norm = TRUE)[c(4,5,9,12,15,17,19,20),]),t(arch.lm.2.s),t(arch.lm.2.p))
write.table(data.frame(t(stat.desc(data.ret,norm = TRUE)[c(4,5,9,12,15,17,19,20),]),t(arch.lm.2.s),t(arch.lm.2.p))
            ,file = "描述性统计.csv",sep=",") 
#数据驱动型模型计算VaR与ES#
#---创建空间---
GASSpec_N   <- UniGASSpec(Dist = "norm", GASPar = list(scale = TRUE))
GASSpec_SN <- UniGASSpec(Dist = "snorm", GASPar = list(scale = TRUE,skewness=TRUE))
GASSpec_ST   <- UniGASSpec(Dist = "std", GASPar = list(scale = TRUE))
GASSpec_SKST <- UniGASSpec(Dist = "sstd", GASPar = list(scale = TRUE,skewness=TRUE))
GASSpec_ALD <- UniGASSpec(Dist = "ald", GASPar = list(location=TRUE,scale = TRUE,skewness = TRUE) )#Asymmetric Laplace Distribution
#GASSpec_AST1 <- UniGASSpec(Dist = "ast1", GASPar = list(scale = TRUE,skewness=TRUE,shape=TRUE,shape2=TRUE))#Asymmetric Student-t with one tail decay parameter
#GASSpec_AST <- UniGASSpec(Dist = "ast", GASPar = list(scale = TRUE,skewness=TRUE,shape=TRUE))#Asymmetric Student-t with two tail decay parameters
#----拟合-----
fit.sh <- UniGASFit(GASSpec = GASSpec_ALD,data = data.ret[,1])
summary(fit.sh)
plot(fit.sh)
ks.test(GAS::pit(fit.sh),runif(1000))
GAS::residuals(fit.sh)[1]
ks.test(GAS::pit(fit.sh),runif(1000))#k-s
fit.sz <- UniGASFit(GASSpec = GASSpec_ALD,data = data.ret[,2])
ks.test(GAS::pit(fit.sz),runif(1000))#k-s
fit.zxb <- UniGASFit(GASSpec = GASSpec_ALD,data = data.ret[,3])
ks.test(GAS::pit(fit.zxb),runif(1000))#k-s
data.frame(fit.sh@Estimates$optimiser$pars,fit.sz@Estimates$optimiser$pars,fit.zxb@Estimates$optimiser$pars)
round(data.frame(fit.sh@Estimates$optimiser$pars,fit.sz@Estimates$optimiser$pars,fit.zxb@Estimates$optimiser$pars)
,3)
write.table(round(data.frame(fit.sh@Estimates$optimiser$pars,fit.sz@Estimates$optimiser$pars,fit.zxb@Estimates$optimiser$pars)
                  ,3),file = "参数.csv",sep=",") 
summary(fit.sh)
#----结果可视化---
par(mfrow = c(3, 1))
#expression(paste(mu==0, ", ", lambda==1, ", ", kappa==0.5),
#           paste(mu==0, ", ", lambda==1, ", ", kappa==1),
#          paste(mu==0, ", ", lambda==1, ", ", kappa==5)),
#----参数结果----
#plot(zoo::zoo(data.ret[,1], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
#     ylab = "",xlab = "",cex.lab = 1.5,cex = 1.5)
plot(zoo::zoo(getFilteredParameters(fit.sh)[,1], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab=expression(mu),xlab = "",cex.lab = 1.5,cex = 1.5)
plot(zoo::zoo(getFilteredParameters(fit.sh)[,2], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = expression(sigma),xlab = "",cex.lab = 1.5,cex = 1.5)
#title("sigma")
plot(zoo::zoo(getFilteredParameters(fit.sh)[,3], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = expression(kappa),xlab = "",cex.lab = 1.5,cex = 1.5)
abline(h=0,col="blue",pch = 15,cex.lab = 1.5,cex = 1.5)
#title("skew")
getMoments(fit.sh)[,4]
plot(zoo::zoo(getMoments(fit.sh)[,4], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "",cex.lab = 1.5,cex = 1.5)
title("kur")
#---深圳----
#plot(zoo::zoo(data.ret[,2], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
#     ylab = "",xlab = "",cex.lab = 1.5,cex = 1.5)
plot(zoo::zoo(getFilteredParameters(fit.sz)[,1], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = expression(mu),xlab = "",cex.lab = 1.5,cex = 1.5)
plot(zoo::zoo(getFilteredParameters(fit.sz)[,2], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = expression(sigma),xlab = "",cex.lab = 1.5,cex = 1.5)
plot(zoo::zoo(getFilteredParameters(fit.sz)[,3], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = expression(kappa),xlab = "",cex.lab = 1.5,cex = 1.5)


#---中小板---
#plot(zoo::zoo(data.ret[,3], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
#     ylab = "",xlab = "",cex.lab = 1.5,cex = 1.5)
plot(zoo::zoo(getFilteredParameters(fit.zxb)[,1], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = expression(mu),xlab = "",cex.lab = 1.5,cex = 1.5)

plot(zoo::zoo(getFilteredParameters(fit.zxb)[,2], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = expression(sigma),xlab = "",cex.lab = 1.5,cex = 1.5)

plot(zoo::zoo(getFilteredParameters(fit.zxb)[,3], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab =expression(kappa),xlab = "",cex.lab = 1.5,cex = 1.5)
abline(h=0,col="blue",pch = 15,cex.lab = 1.5,cex = 1.5)
title("skew")
plot(zoo::zoo(getFilteredParameters(fit.zxb)[,4], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "Date (year)",cex.lab = 1.5,cex = 1.5)

#----矩结果----
par(mfrow = c(4, 1))
#---上证---
plot(zoo::zoo(getMoments(fit.sh)[,1], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "",cex.lab = 1.5,cex = 1.5)
lines(zoo::zoo(getMoments(fit.sh)[,1], order.by = data[-1,1]),col="red")
title("000001.ss")
#plot(zoo::zoo(getMoments(fit.sh)[,1], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
#     ylab = "",xlab = "Date (year)",cex.lab = 1.5,cex = 1.5)
#title("mean")
plot(zoo::zoo(getMoments(fit.sh)[,2], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "",cex.lab = 1.5,cex = 1.5)
title("sigma")
plot(zoo::zoo(getMoments(fit.sh)[,3], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "",cex.lab = 1.5,cex = 1.5)
abline(h=0,col="blue",pch = 15,cex.lab = 1.5,cex = 1.5)
title("skew")
getMoments(fit.sh)[,4]
plot(zoo::zoo(getMoments(fit.sh)[,4], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "",cex.lab = 1.5,cex = 1.5)
title("kur")
#---深圳----
plot(zoo::zoo(getMoments(fit.sz)[,1], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "",cex.lab = 1.5,cex = 1.5)
lines(zoo::zoo(getMoments(fit.sz)[,1], order.by = data[-1,1]),col="red")

plot(zoo::zoo(getMoments(fit.sz)[,2], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "",cex.lab = 1.5,cex = 1.5)
plot(zoo::zoo(getMoments(fit.sz)[,3], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "",cex.lab = 1.5,cex = 1.5)
abline(h=0,col="blue",pch = 15,cex.lab = 1.5,cex = 1.5)
plot(zoo::zoo(getMoments(fit.sz)[,4], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "Date (year)",cex.lab = 1.5,cex = 1.5)

#---中小板---
plot(zoo::zoo(getMoments(fit.zxb)[,1], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "",cex.lab = 1.5,cex = 1.5)
lines(zoo::zoo(getMoments(fit.zxb)[,1], order.by = data[-1,1]),col="red")

plot(zoo::zoo(getMoments(fit.zxb)[,2], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "",cex.lab = 1.5,cex = 1.5)

plot(zoo::zoo(getMoments(fit.zxb)[,3], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "",cex.lab = 1.5,cex = 1.5)
abline(h=0,col="blue",pch = 15,cex.lab = 1.5,cex = 1.5)
title("skew")
plot(zoo::zoo(getMoments(fit.zxb)[,4], order.by = data[-1,1]), las = 1, type = 'l', pch = 10, col = 'black',
     ylab = "",xlab = "Date (year)",cex.lab = 1.5,cex = 1.5)
moments::kurtosis(data.ret[,3])
#-----------滚动估计---------

cluster <- makeCluster(4)#开启并行
#置信水平
alpha_5=0.05
alpha_1=0.01
#
name <- c("000001.ss","399001.sz","399005.sz")
H <- 1500#向前长度
#norm
norm_roll <- list()
for(i in 1:3){
  norm_roll[[name[i]]] <- UniGASRoll(data.ret[,i], 
                                    GASSpec_N, 
                                    RefitEvery = 5,
                                    cluster = cluster, 
                                    RefitWindow="recursive",
                                    ForecastLength = H)
}
#t
t_roll <- list()
for(i in 1:3){
  t_roll[[name[i]]] <- UniGASRoll(data.ret[,i], 
                                      GASSpec_ST, 
                                      RefitEvery = 5,
                                      cluster = cluster, 
                                      RefitWindow="recursive",
                                      ForecastLength = H)
}
#skew-t
skst_roll <- list()
for(i in 1:3){
  skst_roll[[name[i]]] <- UniGASRoll(data.ret[,i], 
                                  GASSpec_SKST, 
                                  RefitEvery = 5,
                                  cluster = cluster, 
                                  RefitWindow="recursive",
                                  ForecastLength = H)
}
#非对称拉普拉斯
ald_roll <-list() 
for(i in 1:3){
ald_roll[[name[i]]] <- UniGASRoll(data.ret[,i], 
                                  GASSpec_ALD, 
                                  RefitEvery = 5,
                                  cluster = cluster, 
                                  RefitWindow="recursive",
                                  ForecastLength = H)
}
#historical
#var
VaR(data.ret[1:1510,1],p=0.99)
sh.his.var <- c();sz.his.var <- c();zxb.his.var <- c()

a <- c()
for(i in 1:300){
  a <- c(a,1500+5*i)
}
for(i in 1:300){
   sh.his.var <- c(sh.his.var,rep(VaR(data.ret[1:a[i],1],p=0.99),5))
   sz.his.var <- c(sz.his.var,rep(VaR(data.ret[1:a[i],2],p=0.99),5))
   zxb.his.var <- c(zxb.his.var,rep(VaR(data.ret[1:a[i],3],p=0.99),5))
}

his.var <- data.frame(sh=sh.his.var,sz=sz.his.var,zxb=zxb.his.var)
#es
sh.his.es <- c();sz.his.es <- c();zxb.his.es <- c()
for(i in 1:300){
  sh.his.es <- c(sh.his.es,rep(ES(data.ret[1:a[i],1],p=0.99),5))
  sz.his.es <- c(sz.his.es,rep(ES(data.ret[1:a[i],2],p=0.99),5))
  zxb.his.es <- c(zxb.his.es,rep(ES(data.ret[1:a[i],3],p=0.99),5))
}

#fix-par-ald
sh.fix.ald.var <- c();sz.fix.ald.var <- c();zxb.fix.ald.var <- c()
for(i in 1:300){
  sh.par <- mleALD(data.ret[1:a[i],1], initial = NA)$par
  sz.par<- mleALD(data.ret[1:a[i],2], initial = NA)$par
  zxb.par <- mleALD(data.ret[1:a[i],3], initial = NA)$par
  sh.fix.ald.var <- c(sh.fix.ald.var,rep(qALD(0.01,mu = sh.par[1],sigma=sh.par[2],p=sh.par[3]),5))
  sz.fix.ald.var <- c(sz.fix.ald.var,rep(qALD(0.01,mu = sz.par[1],sigma=sz.par[2],p=sz.par[3]),5))
  zxb.fix.ald.var <- c(zxb.fix.ald.var,rep(qALD(0.01,mu = zxb.par[1],sigma=zxb.par[2],p=zxb.par[3]),5))
  
}
fix.ald.var <- data.frame(sh=sh.fix.ald.var,sz=sz.fix.ald.var,zxb=zxb.fix.ald.var)
sh.fix.ald.es <- c();sz.fix.ald.es <- c();zxb.fix.ald.es <- c()
for(i in 1:300){
  sh.par <- mleALD(data.ret[1:a[i],1], initial = NA)$par
  sz.par<- mleALD(data.ret[1:a[i],2], initial = NA)$par
  zxb.par <- mleALD(data.ret[1:a[i],3], initial = NA)$par
  g.sh <- function(x) qALD(prob=x, mu = sh.par[1],sigma = sh.par[2],p = sh.par[3])
  g.sz <- function(x) qALD(prob=x, mu = sz.par[1],sigma = sz.par[2],p = sz.par[3])
  g.zxb <- function(x) qALD(prob=x, mu = zxb.par[1],sigma = zxb.par[2],p = zxb.par[3])
  sh.fix.ald.es <- c(sh.fix.ald.es,rep(integrate(g.sh, 0, 0.01)$value/0.01,5))
  sz.fix.ald.es <- c(sz.fix.ald.es,rep(integrate(g.sz, 0, 0.01)$value/0.01,5))
  zxb.fix.ald.es <- c(zxb.fix.ald.es,rep(integrate(g.zxb, 0, 0.01)$value/0.01,5))
}
fix.ald.es <- data.frame(sh=sh.fix.ald.es,sz=sz.fix.ald.es,zxb=zxb.fix.ald.es)
                   
#-----基准组：APGARCH-----
#使用t和norm
cluster
garchspec.t <- ugarchspec(variance.model = list(model = "apARCH"),distribution.model = "sstd")
#garchspec.n <- ugarchspec(variance.model = list(model = "ARCH"),distribution.model = "norm")
garch_roll.n <- list()
garch_roll.t <- list()
for(i in 1:3 ){
  #garch_roll.n[[name[i]]] <- ugarchroll(spec = garchspec.n,data = data.ret[,i],RefitEvery = 5,forecast.length = H,cluster = cluster,refit.window="recursive")
  garch_roll.t[[name[i]]] <- ugarchroll(spec = garchspec.t,data = data.ret[,i],RefitEvery = 5,forecast.length = H,cluster = cluster,refit.window="recursive")
  
}
stopCluster(cluster)#关闭并行
#------计算VaR-------
roll <- list(norm_roll,
             t_roll,skst_roll,
             ald_roll)
VaR <- sapply(roll, VaR_c)
colnames(VaR) <- c("norm","t","skst","ald")
#garch.n.VaR <- sapply(garch_roll.n, as.data.frame,which="VaR")
garch.t.VaR <- sapply(garch_roll.t, as.data.frame,which="VaR")

ES <- sapply(roll, ES_c)
back_ald <- BacktestVaR(data.ret[2001:3000,1],VaR_ald,alpha = 0.05)
back_sstd <- BacktestVaR(data.ret[2001:3000,1],VaR_sstd,alpha = 0.05)
back_std <- BacktestVaR(data.ret[2001:3000,1],VaR_std,alpha = 0.05)
#back_ast <- BacktestVaR(data.ret[2001:3000,1],VaR_ast,alpha = 0.05)
#back_ast1 <- BacktestVaR(data.ret[2001:3000,1],VaR_ast1,alpha = 0.05)

#----计算VaR----
garch.n.VaR <- sapply(garch_roll.n, as.data.frame,which="VaR")
garch.t.VaR <- sapply(garch_roll.t, as.data.frame,which="VaR")

#1%
UC.pval <-  CC.pval <- DQ.pval <- vector("double", 1)
a <- BacktestVaR(garch.t.VaR[3,1][[1]],garch.t.VaR[1,1][[1]],0.01)
  test <- GAS::BacktestVaR(data  = y.ots,
                           VaR   = VaR[,j],
                           alpha = alpha)
  
  CC.pval[j] <- test$LRcc[2]
  DQ.pval[j] <- test$DQ$pvalue
  UC.pval[j] <- test$LRuc[2]


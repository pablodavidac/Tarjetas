########################Estacionalidad Cap 4 Commandeur
setwd("d:/Users_info/ALBANPD/My Documents/R")
data.1 <- log(ts(read.table("Canceladas.txt"),start = c(2014,1),frequency = 12))
data.1<-diff(data.1)
#data.1 <- log(read.table("UKdriversKSI.txt",skip=1))
 #colnames(data.1) <- "logKSI"
 #data.1 <- ts(data.1, start = c(1969),frequency=12)
 data.2 <- read.table("UKinflation.txt",skip=1)
 colnames(data.2) <- "IR"
 data.2 <- ts(data.2 , start = 1950,frequency = 4)
 
par(mfrow=c(1,1))
plot(data.1, col = "darkgrey", xlab="",ylab = "log KSI",pch=3,cex=0.5,
      cex.lab=0.8,cex.axis=0.7)
abline(v=2014.5:2017, lty= "dotted",col="sienna")
 
 
 #############################Deterministic level and seasonal

temp <- cbind(data.1,t)
#temp <- cbind(data.1,tQ4)
fit <- lm(temp[,1] ~ as.factor(temp[,2]))
(coefs <- round(as.numeric(coef(fit)),8))
#[1] 7.42826254 -0.12741266 -0.08907726 -0.16675216 -0.07626917 -0.11420957
#[7] -0.06578303 -0.05553695 -0.01852728 0.06152429 0.16598222 0.22020236
(obs.error.var <- round(summary(fit)$sigma^2,8))
 par(mfrow=c(1,1))
 fit.val <- ts(fitted(fit),start = c(2014,2),frequency = 12)
 temp1 <- cbind(data.1, fit.val)
 plot(temp1 , plot.type="single" , col =c("darkgrey","blue"),lty=c(1,2),
       xlab="")
 legend("topright",leg = c("log UK drivers KSI"," deterministic level and seasonal"),
         cex = 0.7, lty = c(1, 2),col = c("darkgrey","blue"),
         pch=c(3,NA),bty = "y", horiz = T,
)
 
#############residuos
 par(mfrow=c(1,1))
 plot(temp1[,1] , plot.type="single" , col =c("darkgrey","blue"),lty=c(1,2),
        xlab="")
  abline(h=0, col="sienna")
  legend("topright",leg = c("log UK drivers KSI"," deterministic level "),
          cex = 0.7, lty = c(1, 2),col = c("darkgrey","blue"),
          pch=c(3,NA),bty = "y", horiz = T,
 )
  
  
  
  par(mfrow=c(1,1))
  df <- model.matrix(fit)
  seas <- ts(df[,2:12]%*%coefs[2:12],start = c(2014,2), frequency = 12)
  plot(seas, xlab="",ylab = "",col = "darkgrey")
  abline(h=0, col = "sienna", lty = 3)
  
   par(mfrow=c(1,1))
   res <- ts(resid(fit),start = 1960, frequency = 12)
   plot(res, xlab="",ylab = "",col = "darkgrey")
   abline(h=0, col = "sienna", lty = 3)
 hist(res,breaks = 10)
 
 shapiro.test(res)
 Box.test(res, lag = 15, type = "Ljung")
 
 ##########################Stochastic
 library("dlm", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
  fn <- function(params){
   mod <- dlmModPoly(order = 1 ) +
     dlmModSeas(frequency =4)
   V(mod) <- exp(params[1])
   diag(W(mod))[1:2] <- exp(params[2:3])
   return(mod)
 }
  fit <- dlmMLE(data.1, tQ4,fn)
  mod <- fn(fit$par)
  (obs.error.var <- V(mod))
 #[,1]
 #[1,] 0.003513922
  (seas.var <- (diag(W(mod))[2]))
 #[1] 1.891993e-10
  (level.var <- (diag(W(mod))[1]))
 #[1] 0.0009456294
  filtered <- dlmFilter(data.1,mod)
 smoothed <- dlmSmooth(filtered)
  sm <- dropFirst(smoothed$s)
  mu <- c(sm[,1])
  nu <- c(sm[,2])
  res <- c(residuals(filtered,sd=F))
  
  par(mfrow=c(1,1))
  temp <- ts(cbind(data.1,mu)[-c(1:12),],start = c(2014,2),
               frequency = 12)
  plot.ts(temp , plot.type="single" , col =c("darkgrey","blue"),
            lty=c(1,2), xlab="")
  legend("topright",
           leg = c("log UK drivers KSI"," stochastic level"),
           cex = 0.7, lty = c(1, 2),col = c("darkgrey","blue"),
           pch=c(3,NA),bty = "y", horiz = T)
  
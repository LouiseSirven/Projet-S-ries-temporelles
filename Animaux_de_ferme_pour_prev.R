require(zoo)
require(tseries)

#espace de travail
path <- "C:/Users/22sir/Projet-Series-temporelles"
setwd(path) 
getwd() 
list.files()

#importation du fichier
datafile <- "Animaux_ferme.csv"
data <- read.csv(datafile,sep=";")

#Mise en forme des dates
data$Date<-as.Date(paste(data$Date,1,sep="-"), format = "%Y-%m-%d")
data$annee=as.numeric(format(data$Date, format = "%Y"))
data$mois=as.numeric(format(data$Date, format = "%m"))
dates_char = as.character(data$Date)
dates_char[1];dates_char[length(dates_char)] #affiche la première et la dernière date
dates <- as.yearmon(seq(from=2001,to=2022+1/12,by=1/12))

#Tri de la série par date croissante et retrait des dernières valeurs pour la prévision
data<-data[order(data$Date,decreasing=FALSE),] 
xm <- zoo(data$Indice, order.by = dates)
T <- length(xm)-1

#représentation graphique
dev.off()
par(mar=c(4,4,1,1))#ajustement des marges
plot(xm, ylab="IPI", xlab='Date',xaxt="n")
axis(side=1,at=seq(2002,2022,4)) #affiche l'axe des abcisses par 12 de 0 à 196
acf(xm)

#stationnarisation de la série
acf(xm) #autocorrélogramme série brute
dxm.source <- diff(xm,1)#série sans la tendance
dxm <- dxm.source[1:(T-4)] #on supprime les 4 dernières valeurs
##dXt.ts <- diff(Xt.ts,1)
plot(dxm, xlab='Date')
decompose(xm,type="additive")
plot(decompose(xm,type="additive"),xaxt="n")
axis(side=1,at=seq(2002,2022,4))
plot(decompose(dxm,type="additive"),xaxt="n")
axis(side=1,at=seq(2002,2022,4))
#dev.off()
par(mfrow=c(2,1),mar=c(1,1,1,1)) #définit la disposition des graphiques en deux lignes et une colonne
acf(dxm,lag.max=20);pacf(dxm) #trace les autocorrélogrammes total et partiel série dif
#Cela nous donne p<=3 et q<=2.



#tests de stationnarité
pp.test(xm)
kpss.test(dxm, lshort = TRUE)

#Test des modèles envisagés.
arima312 <- arima(dxm,c(3,0,2)) #régresse l'ARIMA(3,0,2)

Box.test(arima312$residuals, lag=1, type="Ljung-Box") #effectue le test d'autocorrélation de Ljung-Box à l'ordre 1 sur les résidus de la régression de ARIMA(3,0,2)

# Le test de Ljung-Box ne rejette pas l'absence d'autocorrélation des résidus à l'ordre 1.
Qtest <- function(series, k) {
  t(apply(matrix(1:k), 1, FUN=function(l) {
    pval <- Box.test(series, lag=l, type="Ljung-Box")$p.value
    return(c("lag"=l,"pval"=pval))
  }))
}
Qtest(arima312$residuals,24)

signif <- function(estim){ #fonction de test des significations individuelles des coefficients
  coef <- estim$coef
  se <- sqrt(diag(estim$var.coef))
  t <- coef/se
  pval <- (1-pnorm(abs(t)))*2
  return(rbind(coef,se,pval))
}

p=1;q=0
estim <- arima(dxm,c(p,0,q)); signif(estim); Qtest(estim$residuals,24)
# L'AR(1) est significatif mais non valide

p=2;q=0
estim <- arima(dxm,c(p,0,q)); signif(estim); Qtest(estim$residuals,30)
# L'AR(2) est significatif mais non valide.

p=3;q=0
estim <- arima(dxm,c(p,0,q)); signif(estim); Qtest(estim$residuals,24)
# L'AR(3) est valide
ar3 <- estim

p=0;q=1
estim <- arima(dxm,c(p,0,q)); signif(estim); Qtest(estim$residuals,24)
# Le MA(1) est significatif mais non valide

p=0;q=2
estim <- arima(dxm,c(p,0,q)); signif(estim); Qtest(estim$residuals,24)
# Le MA(2) est valide
ma2 <- estim

p=1;q=1
estim <- arima(dxm,c(p,0,q)); signif(estim); Qtest(estim$residuals,24)
# L'ARMA(1,1) est valide 
arma11 <- estim

p=1;q=2
estim <- arima(dxm,c(p,0,q)); signif(estim); Qtest(estim$residuals,24)
# L'ARMA(1,2) est valide mais non significatif

p=2;q=1
estim <- arima(dxm,c(p,0,q)); signif(estim); Qtest(estim$residuals,24)
# L'ARMA(2,1) est non significatif mais valide (Q(24))

p=2;q=2
estim <- arima(dxm,c(p,0,q)); signif(estim); Qtest(estim$residuals,24)
# L'ARMA(2,2) n'est pas significatif mais est valide

p=3;q=1
estim <- arima(dxm,c(p,0,q)); signif(estim); Qtest(estim$residuals,24)
# L'ARMA(3,1) est valide
arma31 <- estim

p=3;q=2
estim <- arima(dxm,c(p,0,q)); signif(estim); Qtest(estim$residuals,24)
# L'ARMA(3,2) est valide mais non significatif

# On garde donc les modèles MA(2), AR(3), ARMA(3,1) et ARMA(1,1). Parmi ces modèles valides, on peut choisir celui qui minimise les critères d'information
estim <- arma31; AIC(estim); BIC(estim)
estim <- arma11; AIC(estim); BIC(estim)
estim <- ar3; AIC(estim); BIC(estim)
estim <- ma2; AIC(estim); BIC(estim)

# On choisit ARMA(3,1) et MA(2).

#Blancheur des résidus
#MA(2)
ma2 <- arima(dxm,order=c(0,0,2))
par(mfrow=c(1,1))
acf(ma2$residuals,50, main="")
par(mfrow=c(1,1),mar=c(2,2,2,2))
plot(ma2$residuals)
#ARMA31
arma31 <- arima(dxm,order=c(3,0,1))
par(mfrow=c(1,1))
acf(arma31$residuals,50, main="")
par(mfrow=c(1,1),mar=c(2,2,2,2))
plot(arma31$residuals)

#QQplot, normalité des résidus
#MA2
qqnorm(ma2$residuals, pch = 1, frame = FALSE)#plot QQtest
qqline(ma2$residuals, col = "steelblue", lwd = 2)#QQPlot théorique
shapiro.test(ma2$residuals) #Résidus ne suivent pas loi normale
jarque.bera.test(ma2$residuals) #Résidus ne suivent pas loi normale

#ARMA31
qqnorm(arma31$residuals, pch = 1, frame = FALSE)#plot QQtest
qqline(arma31$residuals, col = "steelblue", lwd = 2)#QQPlot théorique
shapiro.test(arma31$residuals) #Résidus ne suivent pas loi normale
jarque.bera.test(arma31$residuals) #Résidus ne suivent pas loi normale

##Prédictions
#pdates <- as.yearmon(seq(from=2001+1/12,to=2022+1/12,by=1/12))
pdates <- as.yearmon(seq(from=2021+10/12,to=2022+1/12,by=1/12))
arma31p <- zoo(predict(arma31, 4)$pred, order.by = pdates)
arma11p <- zoo(predict(arma11, 4)$pred, order.by = pdates)
ar3p <- zoo(predict(ar3, 4)$pred, order.by = pdates)
ma2p <- zoo(predict(ma2, 4)$pred, order.by = pdates)
#plot(ma2p)
obs <- dxm.source[(T-3):T]
plot(obs)
pred <- cbind("obs"=obs, "ma2"=ma2p, "ar3"=ar3p, "arma31"=arma31p, "arma11"=arma11p)
par(mfrow=c(1,1),mar=c(0,0,2,2)) 
plot(pred) #C'est l'AR3 qui paraît le mieux.
par(mfrow=c(1,1),mar=c(1,1,1,1)) 
#plot(ma2) #inverse des racines

##Bonus vérification avec le module forecast
#test avec librairie forecast
library(forecast)
install.packages('forecast', dependencies = TRUE)
auto <- auto.arima(dxm,stepwise=FALSE,approx=FALSE, max.P = 4, max.Q =2)
fcast <- forecast(auto)
print(auto)
plot(fcast)

#test avec librairie forecast
auto <- auto.arima(xm,stepwise=FALSE,approx=FALSE, max.P = 4, max.Q =2)
fcast <- forecast(auto)
print(auto)
plot(fcast)

pred <- predict(ma2, 4)
############################################
############################################


#### Q7 ####
ma2p <- zoo(predict(ma2,4)$pred)
pred <- cbind(obs, "arma31"=arma31p, "ma2"=arma12p)

sqrt(sum((ar3p-obs)^2)/4)
sqrt(sum((ma2p-obs)^2)/4)
sd(obs)





pmax=3; qmax=2

## fonction pour estimer un arima et en v´erifier l'ajustement et la validit´e
modelchoice <- function(p,q,data=dxm, k=24){
  estim <- try(arima(data, c(p,1,q),optim.control=list(maxit=20000)))
  if (class(estim)=="try-error") return(c("p"=p,"q"=q,"arsignif"=NA,"masignif"=NA,"resnocorr"=NA, "ok"=NA))
  arsignif <- if (p==0) NA else signif(estim)[3,p]<=0.05
  masignif <- if (q==0) NA else signif(estim)[3,p+q]<=0.05
  resnocorr <- sum(Qtest(estim$residuals,24)[,2]<=0.05,na.rm=T)==0
  checks <- c(arsignif,masignif,resnocorr)
  ok <- as.numeric(sum(checks,na.rm=T)==(3-sum(is.na(checks))))
  return(c("p"=p,"q"=q,"arsignif"=arsignif,"masignif"=masignif,"resnocorr"=resnocorr,"ok"=ok))
}

armodels <- t(apply(matrix(1:3), 1, function(l) modelchoice(l,0)))
mamodels <- t(apply(matrix(1:2), 1, function(l) modelchoice(0,l)))

## fonction pour estimer et vérifier tous les arima(p,q) avec p<=pmax et q<=max
armamodelchoice <- function(pmax,qmax){
  pqs <- rbind(c(1,1),
               kronecker(matrix(1:(min(pmax,qmax)-1)),matrix(1,nrow=3,ncol=2))+kronecker(matrix(1,nrow=min(pmax,qmax)-1),matrix(c(0,1,1,1,0,1),ncol=2)),
               cbind(min(pmax,qmax):pmax,min(pmax,qmax):qmax)[-1,]
  )
  t(apply(matrix(1:dim(pqs)[1]),1,function(row) modelchoice(pqs[row,1],pqs[row,2])))
}
armamodels <- armamodelchoice(pmax,qmax) #



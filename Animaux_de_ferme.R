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
dates_char[1];dates_char[length(dates_char)] #affiche la premi`ere et la derni`ere date
dates <- as.yearmon(seq(from=2001,to=2022+1/12,by=1/12))

#Tri de la série par date croissante
data<-data[order(data$Date,decreasing=FALSE),] 
xm <- zoo(data$Indice, order.by = dates)
##T <- length(xm)
##Xt.ts<-ts(data$Indice, start=c(2001,1), end=c(2022,2), frequency = 12)

#représentation graphique
dev.off()
par(mar=c(4,4,1,1))#ajustement des marges
plot(xm, ylab="IPI", xlab='Date')

#stationnarisation de la série
acf(xm) #autocorrélogramme série brute
dxm <- diff(xm,1)#série sans la tendance
##dXt.ts <- diff(Xt.ts,1)
plot(dxm, xlab='Date')
dev.off()
par(mfrow=c(2,1),mar=c(1,1,1,1)) #définit la disposition des graphiques en deux lignes et une colonne
acf(dxm);pacf(dxm) #trace les autocorrélogrammes total et partiel

#tests de stationnarité
pp.test(dxm)

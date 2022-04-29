require(zoo)
require(tseries)

path <- "C:/Users/22sir/Desktop/ST/ProjetST"
setwd(path) #définit l'espace de travail (working directory ou "wd")
getwd() #affiche le wd
list.files() #liste les ?l?ments du wd

datafile <- "valeurs_mensuelles.csv"

data <- read.csv(datafile,sep=";") #importe un fichier .csv dans un objet de classe data.frame
xm.source <- zoo(data[[2]]) #convertit le premier ?l?ment de data en s?rie temporelle de type "zoo"
T <- length(xm.source)
xm <- xm.source[1:(T-4)] #supprime les 4 derni?res valeurs

plot(xm) #repr?sente graphiquement xm

plot(xm, xaxt="n") #repr?sente xm sans l'axe des abcisses
axis(side=1,at=seq(0,196,12)) #affiche l'axe des abcisses par 12 de 0 ? 196
acf(xm) #trace l'autocorr?logramme total
#serie_decomp<-decompose(xm,type="additive")
desaison <- xm-lag(xm,-1)
plot(desaison)
acf(desaison)
pacf(desaison)
pp.test(desaison)

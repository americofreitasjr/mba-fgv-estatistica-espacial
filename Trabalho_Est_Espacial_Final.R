library(maps)
library(mapdata)
library(maptools)
library(rgdal)
library(RgoogleMaps)
library(readr)
library(spatstat)
library(dplyr)

## Lendo o shapefile com os bairros de Houston
houston_shp = readOGR("Houston_City_Limit/Houston_City_Limit.shp")
houston=spTransform(houston_shp, CRS("+proj=longlat +datum=WGS84"))

class(houston)
library(readxl)
library(readr)
houston_limit<-read.csv2("Base Houston.csv", sep = ",", dec = "." )
houston_limit
#------------variavies por delito------------------------------------
hrobbery<-read.csv2("robbery.csv",sep = ";", dec = ".")
hagg<- read.csv2("aggravated assault.csv", dec = ".")
hauto<- read.csv2("auto theft.csv", dec = ".")
hburglary<- read.csv2("burglary.csv", dec = ".")
hrape<- read.csv2("rape.csv", dec = ".")
hmurder<-read.csv2("murder.csv", dec = ".")
write.csv2(houston_limit, "hous.csv")
ls()
#--------------------------------------------------------------------

#Definindo o shapefile como uma janela onde os pontos serao plotados 
HCL <- as.owin(houston)
#Plotando o shapefile
plot(HCL)
#Criando o padrao de pontos a ser plotado
Houston_ppp = ppp(houston_limit$lon, houston_limit$lat,window=HCL )

#---------------criando pontos por delitos-------------------------------
hagg_ppp = ppp(hagg$lon, hagg$lat,window=HCL )
hrobbery_ppp = ppp(hrobbery$lon, hrobbery$lat,window=HCL )
hmurder_ppp = ppp(hmurder$lon,hmurder$lat,window=HCL )
hauto_ppp = ppp(hauto$lon, hauto$lat,window=HCL )
hrape_ppp = ppp(hrape$lon, hrape$lat,window=HCL )
hburglary_ppp = ppp(hburglary$lon, hburglary$lat,window=HCL )

#--------------------------------------------------------------------

##ppp - criar um objeto com classe ppp representando o padrao de pontos
#Argumentos:
#x - longitude
#y - latitude
#window - um objeto owin. 

#Plotando as localizacoes dos delitos
par(mar=c(0.5,0.5,1.5,0.5))

plot(Houston_ppp, pch=21, cex=0.9, bg="blue", main="Ocorrencias de crimes em Houston")
                                   

#------------------PRIMEIRA ORDEM-------------------------------------------------------------------
#PLOTANDO GRAFICO EM  EFEITOS EM PRIMEIRA ORDEM
Houston.g = density.ppp(x = Houston_ppp, sigma=0.01, kernel="gaussian")
Houston.q = density.ppp(x = Houston_ppp, sigma=0.01, kernel="quartic")
#Plotando os dados e as funcoes intensidades estimadas pelas diversas funcoes kernel
par(mfrow=c(2,2))

plot(density.ppp(Houston_ppp, sigma=0.01, kernel="gaussian"), main="Sigma=0.01", cex.main=0.5)#gaussian
plot(density.ppp(Houston_ppp, sigma=0.03, kernel="quartic"), main="Sigma=0.03", cex.main=0.5)#quartic
plot(density.ppp(Houston_ppp, sigma=0.03, kernel="gaussian"), main="Sigma=0.03", cex.main=0.5)#quartic
##density.ppp - calcula a funcao de intensidade de acordo com o kernel escolhido
#Argumentos:
#x - objeto da classe ppp
#sigma - é o valor do raio (tau na expressao dos slides)
#kernel - o kernel que deseja-se usar

#----------------PLOTANDO GRAFICO EM  EFEITOS EM PRIMEIRA ORDEM POR DELITO

par(mfrow=c(3,2))
plot(density.ppp(hagg_ppp , sigma=0.02, kernel="gaussian"), main="Delito:Aggravated assault", cex.main=0.5)#quartic
plot(density.ppp(hrobbery_ppp , sigma=0.02, kernel="gaussian"), main="Delito:Robbery", cex.main=0.5)#quartic
plot(density.ppp(hmurder_ppp , sigma=0.02, kernel="gaussian"), main="Delito:Murder", cex.main=0.5)#quartic
plot(density.ppp(hauto_ppp , sigma=0.02, kernel="gaussian"), main="Delito:Auto thief", cex.main=0.5)#quartic
plot(density.ppp(hrape_ppp  , sigma=0.02, kernel="gaussian"), main="Delito:Rape", cex.main=0.5)#quartic
plot(density.ppp(hburglary_ppp , sigma=0.02, kernel="gaussian"), main="Delito:burglary", cex.main=0.5)#quartic

#--------------------plotando graficos por crimes-------------------------------
library(ggplot2)
library(ggmap)
#Plotando o grafico com recursos do Google Maps
#Criando o grafico com a densidade e o layout do Google Maps
HT <- get_map("Houston, Texas", zoom=8,maptype='hybrid')
plot(HT)
google = plot(HT) + stat_density2d(aes(x=lat,y=lon, fill = ..level..),alpha = .8, h=.025, n = 400,geom = "polygon", data = houston_limit) 
plot(google)
google+ scale_fill_gradient(low = "black", high= "red") + facet_wrap(~ offense)

#Funcao que estima o raio por meio de validacao cruzada (custosa computacionalmente)
raio.est = bw.diggle(Houston_ppp)
raio.est
plot(raio.est)

#-------------------------------------SEGUNDA ORDEM----------------------------------
#Sorteando uma amostra de tamanho 100 para estudar o efeito de segunda ordem por conta do custo computacional
aHouston = sample_n(houston_limit,100)

ahagg = sample_n(hagg ,100)
ahrobbery = sample_n(hrobbery,100)
ahmurder = sample_n(hmurder,100)
ahauto= sample_n(hauto,100)
ahrape= sample_n(hrape,100)
ahburglary= sample_n(hburglary,100) 
aHouston_ppp<-ppp(aHouston$lon, aHouston$lat, window=HCL)
ahagg_ppp<-ppp(ahagg$lon, ahagg$lat, window=HCL)
ahrobbery_ppp<-ppp(ahrobbery$lon,ahrobbery$lat, window=HCL)
ahmurder_ppp<-ppp(ahmurder$lon, ahmurder$lat, window=HCL)
ahauto_ppp <-ppp(ahauto$lon, ahauto$lat, window=HCL)
ahrape_ppp <-ppp(ahrape$lon, ahrape$lat, window=HCL)
ahburglary_ppp <-ppp(ahburglary$lon,ahburglary$lat, window=HCL)


aHouston_ppp<-ppp(aHouston$lon, aHouston$lat, window=HCL)
plot(ahagg_ppp, pch=21, cex=0.9, bg="blue", main="Aggravated assault")
plot(ahrobbery_ppp, pch=21, cex=0.9, bg="blue", main="Robbery")
plot(ahmurder_ppp, pch=21, cex=0.9, bg="blue", main="Murder")
plot(ahrape_ppp, pch=21, cex=0.9, bg="blue", main="Rape")
plot(ahburglary_ppp, pch=21, cex=0.9, bg="blue", main="Burglary")
plot(ahauto_ppp, pch=21, cex=0.9, bg="blue", main="Auto theft")

#Estimando a funcao G
HC.G = Gest(aHouston_ppp)
Hagg.G=Gest(ahagg_ppp)
Hrob.G=Gest(ahrobbery_ppp)
Hmurd.G=Gest(ahmurder_ppp)
Haut.G =Gest(ahauto_ppp)
Hrape.G= Gest(ahrape_ppp)
Hburg.G=Gest(hburglary_ppp )
#Gest - estima a funcao de distribuicao G de um padrao de pontos
#Argumento
#X - um objeto da classe ppp

#Estimando a funcao K
HC.K = Kest(aHouston_ppp)
Hagg.K=Kest(ahagg_ppp)
Hrob.K=Kest(ahrobbery_ppp)
Hmurd.K=Kest(ahmurder_ppp)
Haut.K =Kest(ahauto_ppp)
Hrape.K=Kest(ahrape_ppp)
Hburg.K=Kest(hburglary_ppp )

#Kest - estima a funcao K de Ripley de um padrao de pontos
#Argumento
#X - um objeto da classe ppp

#Estimando a funcao F
HC.F = Fest(aHouston_ppp)
KHagg.F=Fest(ahagg_ppp)
Hrob.F=Fest(ahrobbery_ppp)
Hmurd.F=Fest(ahmurder_ppp)
Haut.F =Fest(ahauto_ppp)
Hrape.F=Fest(ahrape_ppp)
Hburg.F=Fest(hburglary_ppp )
#-----------Plotando as funcoes G, K e F de Houston-------------------------
par(mfrow = c(2,2))
par(mar=c(2.5,2.5,1.5,.5))

plot(HC.G, main="Funcao G de Houston")
plot(HC.K, main="Funcao K de Houston")
plot(HC.F, main="Funcao F de Houston")

#-------------Plotando as funcoes G, K e F dos delitos----------------------
par(mfrow = c(2,2))
par(mar=c(2.5,2.5,1.5,.5))
plot(KHagg.G, main="Funcao G de Aggravated assault")
plot(KHagg.F, main="Funcao F de Aggravated assault")
plot(KHagg.K, main="Funcao K de Aggravated assault")
par(mfrow = c(2,2))
par(mar=c(2.5,2.5,1.5,.5))
plot(Hrob.F, main="Funcao F de Robbery")
plot(Hrob.G, main="Funcao G de Robbery")
plot(Hrob.k, main="Funcao k de Robbery")
par(mfrow = c(2,2))
par(mar=c(2.5,2.5,1.5,.5))
plot(Hmurd.F, main="Funcao F de Murder")
plot(Hmurd.G, main="Funcao G de Murder")
plot(Hmurd.K, main="Funcao K de Murder")
par(mfrow = c(2,2))
par(mar=c(2.5,2.5,1.5,.5))
plot(Haut.F, main="Funcao F de Auto thief")
plot(Haut.K, main="Funcao K de Auto thief")
plot(Haut.G, main="Funcao G de Auto thief")
par(mfrow = c(2,2))
par(mar=c(2.5,2.5,1.5,.5))
plot(Hrape.F, main="Funcao F de Rape")
plot(Hrape.G, main="Funcao G de Rape")
plot(Hrape.K, main="Funcao K de Rape")
par(mfrow = c(2,2))
par(mar=c(2.5,2.5,1.5,.5))
plot(Hburg.F, main="Funcao F de Burglary")
plot(Hburg.G, main="Funcao G de Burglary")
plot(Hburg.K, main="Funcao K de Burglary")
#-----------------------------------------------------------------------------------------
#Realizando o teste de Clark-Evans para verificar agregacao espacial
clarkevans.test(aHouston_ppp)
clarkevans.test(ahagg_ppp)
clarkevans.test(ahrobbery_ppp)
clarkevans.test(ahmurder_ppp)
clarkevans.test(ahauto_ppp)
clarkevans.test(ahrape_ppp)
clarkevans.test(hburglary_ppp )

#------------------------------------------------------------------------------------------------
#Realizando o teste de Hopkins-Skellam de Completa aleatoriedade espacial para verificar agregacao espacial
hopskel.test(aHouston_ppp, alternative="clustered")
hopskel.test(ahagg_ppp, alternative="clustered")
hopskel.test(ahrobbery_ppp, alternative="clustered")
hopskel.test(ahmurder_ppp, alternative="clustered")
hopskel.test(ahauto_ppp, alternative="clustered")
hopskel.test(ahrape_ppp, alternative="clustered")
hopskel.test(hburglary_ppp , alternative="clustered")


#Funcoes para estimar os envelopes das funcoes F, G e K
#Kest=envelope(aNYppp,Kest,nsim=10) #alto custo computacional
Gest=envelope(aHouston_ppp,fun = Gest,nsim=20)
Fest=envelope(aHouston_ppp,fun = Fest, nsim=10)

Gest=envelope(ahagg_ppp,fun = Gest,nsim=20)
Fest=envelope(ahagg_ppp,fun = Fest, nsim=10)

Gest=envelope(ahmurder_ppp,fun = Gest,nsim=20)
Fest=envelope(ahmurder_ppp,fun = Fest, nsim=10)

Gest=envelope(ahrobbery_ppp,fun = Gest,nsim=20)
Fest=envelope(ahrobbery_ppp,fun = Fest, nsim=10)

Gest=envelope(hburglary_ppp,fun = Gest,nsim=20)
Fest=envelope(hburglary_ppp,fun = Fest, nsim=10)

Gest=envelope(ahrape_ppp,fun = Gest,nsim=20)
Fest=envelope(ahrape_ppp,fun = Fest, nsim=10)

Gest=envelope(ahauto_ppp,fun = Gest,nsim=20)
Fest=envelope(ahauto_ppp,fun = Fest, nsim=10)



#Plotando as funcoes e seus respectivos envelopes
par(mfrow=c(2,2))
plot(aHouston_ppp, pch=21, cex=0.9, bg="blue")

plot(ahrobbery_ppp, pch=21, cex=0.9, bg="blue")
plot(ahmurder_ppp, pch=21, cex=0.9, bg="blue")
plot(ahauto_ppp, pch=21, cex=0.9, bg="blue")
plot(ahrape_ppp, pch=21, cex=0.9, bg="blue")
plot(hburglary_ppp, pch=21, cex=0.9, bg="blue")
plot(ahagg_ppp, pch=21, cex=0.9, bg="blue")
#plot(aKest)
plot(Gest)
plot(Fest)
par(mfrow=c(1,1))



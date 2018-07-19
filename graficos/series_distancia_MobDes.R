library("TSdist")
library("dtw")
library("ggplot2")
library("plyr")
library("reshape2")
library("lattice")
library("scales")
library(dplyr, warn.conflicts = FALSE)
# documentacao TSdist
# https://journal.r-project.org/archive/2016/RJ-2016-058/RJ-2016-058.pdf
# https://cran.r-project.org/web/packages/TSdist/TSdist.pdf


# Create variable 'log'  that contains the paths of the scripts and workspaces (.RData)
source("./paths.R")

# matrix with ts 
dataserie <- list()

for (it in 1:length(logs$sistema)){
  rm(list = ls()[!ls() %in% c("logs","it","dataserie")])
  #source(as.character(logs$caminho[it]) )
  load (as.character(logs$ws_series_agrupadas[it]))
  
  name<-(paste("M-",as.character(logs$sistema[it]), sep = ""))
  if(as.character(logs$sistema[it])=="Cocos2d-x"){
    name<-("M-Cocos")}
  
  # get mobile
  x<-saida_device_bind%>% 
    filter(prob==0.00)%>%
    filter(tipo=="mobile")
  # get desktop
  y<-saida_device_bind%>% 
    filter(prob==0.00)%>%
    filter(tipo=="desktop")
  # get desktop mobile
  z<-saida_device_bind%>% 
    filter(prob==0.00)%>%
    filter(tipo=="desktop mobile")
  aux<-data.frame(iteracao=c(1:max(x$iteracao,y$iteracao,z$iteracao)))#igualar tamanho do vetor
  x<-right_join(x,aux,by="iteracao",rm.na=TRUE)
  rm(aux)
  x<-x%>% mutate(porc= if_else(is.na(porc),0,porc) ) %>% select(iteracao,porc)%>% spread(iteracao, porc) 
  
  dataserie[[name]]<-ts(x)
  rm(x,y,z,name)
}

for (it in 1:length(logs$sistema)){
  rm(list = ls()[!ls() %in% c("logs","it","dataserie")])
  #source(as.character(logs$caminho[it]) )
  load (as.character(logs$ws_series_agrupadas[it]))
  
  name<-(paste("D-",as.character(logs$sistema[it]), sep = ""))
  if(as.character(logs$sistema[it])=="Cocos2d-x"){
    name<-("D-Cocos")}
  
  # get mobile
  x<-saida_device_bind%>% 
    filter(prob==0.00)%>%
    filter(tipo=="mobile")
  # get desktop
  y<-saida_device_bind%>% 
    filter(prob==0.00)%>%
    filter(tipo=="desktop")
  # get desktop mobile
  z<-saida_device_bind%>% 
    filter(prob==0.00)%>%
    filter(tipo=="desktop mobile")
  aux<-data.frame(iteracao=c(1:max(x$iteracao,y$iteracao,z$iteracao)))#igualar tamanho do vetor
  y<-right_join(y,aux,by="iteracao",rm.na=TRUE)
  rm(aux)
  y<-y%>% mutate(porc= if_else(is.na(porc),0,porc) ) %>% select(iteracao,porc)%>% spread(iteracao, porc) 
  
  dataserie[[name]]<-ts(y)
  rm(x,y,z,name)
}


for (it in 1:length(logs$sistema)){
  rm(list = ls()[!ls() %in% c("logs","it","dataserie")])
  #source(as.character(logs$caminho[it]) )
  load (as.character(logs$ws_series_agrupadas[it]))
  
  name<-(paste("DM-",as.character(logs$sistema[it]), sep = ""))
  if(as.character(logs$sistema[it])=="Cocos2d-x"){
    name<-("DM-Cocos")}
  
  # get mobile
  x<-saida_device_bind%>% 
    filter(prob==0.00)%>%
    filter(tipo=="mobile")
  # get desktop
  y<-saida_device_bind%>% 
    filter(prob==0.00)%>%
    filter(tipo=="desktop")
  # get desktop mobile
  z<-saida_device_bind%>% 
    filter(prob==0.00)%>%
    filter(tipo=="desktop mobile")
  aux<-data.frame(iteracao=c(1:max(x$iteracao,y$iteracao,z$iteracao)))#igualar tamanho do vetor
  z<-right_join(z,aux,by="iteracao",rm.na=TRUE)
  rm(aux)
  z<-z%>% mutate(porc= if_else(is.na(porc),0,porc) ) %>% select(iteracao,porc)%>% spread(iteracao, porc) 
  
  dataserie[[name]]<-ts(z)
  rm(x,y,z,name)
}

#calcular distancia de  duas series 
# Computes the distance measure based on the cross-correlation between a pair of numeric time series.
#CCorDistance(x$porc,y$porc)
# Computes two different distance measure based on Pearson’s correlation between a pair of numeric time series of the same length.
#CorDistance(x$porc,y$porc)
# Computes the Dissim distance between a pair of numeric series.
#DissimDistance(x$porc,y$porc) #nao funciona
# Computes the Dynamic Time Warping distance between a pair of numeric time series.
# do not nedd same size
#DTWDistance(y_godot$porc,x_godot$porc)
#DTWDistance(x_allegro$porc,y_allegro$porc)


matrizresult<-TSDatabaseDistances(dataserie, distance = "dtw")

#portugues
MDR = as.data.frame(melt(as.matrix(matrizresult)))  ### Insira a matriz obtida em matrizresult
colnames(MDR) <- c("Datastream", "Algorithm", "Valor")

MDR <- ddply(MDR, .(Datastream), transform, rescale = rescale(Valor))
p <- ggplot(MDR, aes(Datastream, Algorithm)) +
  geom_tile(aes(fill = Valor), colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue")
p + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),text = element_text(size=15))
ggsave("./graficos/pt/mapa_MobDes.pdf", p+ theme(axis.title.x=element_blank(), axis.title.y=element_blank()), width=12, height=8)

#ingles
MDR = as.data.frame(melt(as.matrix(matrizresult)))  ### Insira a matriz obtida em matrizresult
colnames(MDR) <- c("Datastream", "Algorithm", "Value")

MDR <- ddply(MDR, .(Datastream), transform, rescale = rescale(Value))
p <- ggplot(MDR, aes(Datastream, Algorithm)) +
  geom_tile(aes(fill = Value), colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue")
p + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),text = element_text(size=15))
ggsave("./graficos/en/mapa_MobDes.pdf", p+ theme(axis.title.x=element_blank(), axis.title.y=element_blank()), width=12, height=8)





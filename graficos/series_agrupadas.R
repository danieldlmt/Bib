
library(dplyr)
# Create variable 'log'  that contains the paths of the scripts and workspaces (.RData)
source("./paths.R")
limiar<- 0.0

for (it in 1:length(logs$sistema)){
  

  rm(list = ls()[!ls() %in% c("logs","it","limiar")])
  #source(as.character(logs$caminho[it]))
  load (as.character(logs$ws_series_agrupadas[it]))

#######################
  # Generalist vs Specialist
##########################
  aux<-data.frame(iteracao=c(1:max(saida_plat_bind$iteracao)))#igualar tamanho do vetor
  gen <-saida_plat_bind%>% filter(prob==limiar)%>%filter(tipo=="gen")  
  gen<-right_join(gen,aux,by="iteracao",rm.na=TRUE)
  gen<-gen%>% mutate(porc= if_else(is.na(porc),0,porc) ) 
  esp <-saida_plat_bind%>% filter(prob==limiar)%>%filter(tipo=="esp") 
  esp<-right_join(esp,aux,by="iteracao",rm.na=TRUE)
  esp<-esp%>% mutate(porc= if_else(is.na(porc),0,porc) ) 
  #pt
  name <- paste ( "./graficos/pt/serie_gen_",tolower(as.character(logs$sistema[it])),".pdf",sep = "" )
  pdf ( name , width = 8 , height = 6 ) 
    par(mar=c(5.1, 5, 4.1, 2.1)) 
    plot(gen$iteracao, gen$porc, col="black", type="l", xlab="Tempo (semanas)", ylim=c(0,100),ylab="Porcentagem",cex.names = 1.7,cex.axis = 1.7,cex.lab=1.7) 
    z<-lm(porc~iteracao,data=gen)
    abline(z,col="orange",lty="longdash")
  dev.off()
  #en
    name <- paste ( "./graficos/en/serie_gen_",tolower(as.character(logs$sistema[it])),".pdf",sep = "" )
    pdf ( name , width = 8 , height = 6 ) 
    par(mar=c(5.1, 5, 4.1, 2.1)) 
    plot(gen$iteracao, gen$porc, col="black", type="l", xlab="Time (weeks)", ylim=c(0,100),ylab="Percentage",cex.names = 1.7,cex.axis = 1.7,cex.lab=1.7) 
    z<-lm(porc~iteracao,data=gen)
    abline(z,col="orange",lty="longdash")
  dev.off()

  #pt
  name <- paste ( "./graficos/pt/serie_esp_",tolower(as.character(logs$sistema[it])),".pdf",sep = "" )
  pdf ( name , width = 8 , height = 6 )    
    par(mar=c(5.1, 5, 4.1, 2.1)) 
    plot(esp$iteracao, esp$porc, col="blue", type="l", xlab="Tempo (semanas)", ylim=c(0,100),ylab="Porcentagem",cex.names = 1.7,cex.axis = 1.7,cex.lab=1.7) 
    z<-lm(porc~iteracao,data=esp)
    abline(z,col="orange",lty="longdash")
  dev.off()
  #en
  name <- paste ( "./graficos/en/serie_esp_",tolower(as.character(logs$sistema[it])),".pdf",sep = "" )
  pdf ( name , width = 8 , height = 6 )    
    par(mar=c(5.1, 5, 4.1, 2.1)) 
    plot(esp$iteracao, esp$porc, col="blue", type="l", xlab="Time (weeks)", ylim=c(0,100),ylab="Percentage",cex.names = 1.7,cex.axis = 1.7,cex.lab=1.7) 
    z<-lm(porc~iteracao,data=esp)
    abline(z,col="orange",lty="longdash")
  dev.off()
    
  #######################
  # Mobile vs Desktop
  ##########################
  aux<-data.frame(iteracao=c(1:max(saida_device_bind$iteracao)))#igualar tamanho do vetor
  mobiledesktop <- saida_device_bind%>% filter(prob==limiar)  %>%filter(tipo=="desktop mobile")  
  mobiledesktop<-right_join(mobiledesktop,aux,by="iteracao",rm.na=TRUE)
  mobiledesktop<-mobiledesktop%>% mutate(porc= if_else(is.na(porc),0,porc) ) 
  mobile<- saida_device_bind%>% filter(prob==limiar)  %>%filter(tipo=="mobile")
  mobile<-right_join(mobile,aux,by="iteracao",rm.na=TRUE)
  mobile<-mobile%>% mutate(porc= if_else(is.na(porc),0,porc) ) 
  desktop <- saida_device_bind%>% filter(prob==limiar)  %>%filter(tipo=="desktop") 
  desktop<-right_join(desktop,aux,by="iteracao",rm.na=TRUE)
  desktop<-desktop%>% mutate(porc= if_else(is.na(porc),0,porc) )  
    
    #pt
    name <- paste ( "./graficos/pt/serie_mobdes_",tolower(as.character(logs$sistema[it])),".pdf",sep = "" )
    pdf ( name , width = 8 , height = 6 )     
      par(mar=c(5.1, 5, 4.1, 2.1)) 
      plot(mobiledesktop$iteracao, mobiledesktop$porc, col="black", type="l", xlab="Tempo (semanas)",ylim=c(0,100), ylab="Porcentagem",cex.names = 1.7,cex.axis = 1.7,cex.lab=1.7)
      z<-lm(porc~iteracao,data=mobiledesktop)
      abline(z,col="orange",lty="longdash")
    dev.off() 
    
    #en
    name <- paste ( "./graficos/en/serie_mobdes_",tolower(as.character(logs$sistema[it])),".pdf",sep = "" )
    pdf ( name , width = 8 , height = 6 )     
      par(mar=c(5.1, 5, 4.1, 2.1)) 
      plot(mobiledesktop$iteracao, mobiledesktop$porc, col="black", type="l", xlab="Time (weeks)",ylim=c(0,100), ylab="Percentage",cex.names = 1.7,cex.axis = 1.7,cex.lab=1.7)
      z<-lm(porc~iteracao,data=mobiledesktop)
      abline(z,col="orange",lty="longdash")
    dev.off() 
    
    #pt
    name <- paste ( "./graficos/pt/serie_mob_",tolower(as.character(logs$sistema[it])),".pdf",sep = "" )
    pdf ( name , width = 8 , height = 6 )  
      par(mar=c(5.1, 5, 4.1, 2.1)) 
      plot(mobile$iteracao, mobile$porc, col="darkgreen", type="l", xlab="Tempo (semanas)",ylim=c(0,100), ylab="Porcentagem",cex.names = 1.7,cex.axis = 1.7,cex.lab=1.7)
      z<-lm(porc~iteracao,data=mobile)
      abline(z,col="orange",lty="longdash")
    dev.off() 
    #en
    name <- paste ( "./graficos/en/serie_mob_",tolower(as.character(logs$sistema[it])),".pdf",sep = "" )
    pdf ( name , width = 8 , height = 6 )  
      par(mar=c(5.1, 5, 4.1, 2.1)) 
      plot(mobile$iteracao, mobile$porc, col="darkgreen", type="l", xlab="Time (weeks)",ylim=c(0,100), ylab="Percentage",cex.names = 1.7,cex.axis = 1.7,cex.lab=1.7)
      z<-lm(porc~iteracao,data=mobile)
      abline(z,col="orange",lty="longdash")
    dev.off() 
    
    name <- paste ( "./graficos/pt/serie_des_",tolower(as.character(logs$sistema[it])),".pdf",sep = "" )
    pdf ( name , width = 8 , height = 6 )  
    par(mar=c(5.1, 5, 4.1, 2.1)) 
      plot(desktop$iteracao, desktop$porc, col="red", type="l", xlab="Tempo (semanas)",ylim=c(0,100), ylab="Porcentagem",cex.names = 1.7,cex.axis = 1.7,cex.lab=1.7)
      z<-lm(porc~iteracao,data=desktop)
      abline(z,col="orange",lty="longdash")
    dev.off() 
    #en
    name <- paste ( "./graficos/en/serie_des_",tolower(as.character(logs$sistema[it])),".pdf",sep = "" )
    pdf ( name , width = 8 , height = 6 )  
      par(mar=c(5.1, 5, 4.1, 2.1)) 
      plot(desktop$iteracao, desktop$porc, col="red", type="l", xlab="Time (weeks)",ylim=c(0,100), ylab="Percentage",cex.names = 1.7,cex.axis = 1.7,cex.lab=1.7)
      z<-lm(porc~iteracao,data=desktop)
      abline(z,col="orange",lty="longdash")
    dev.off() 
    
}
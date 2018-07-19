library(ggplot2)
library(dplyr)
library(tidyr)

# Create variable 'log'  that contains the paths of the scripts and workspaces (.RData)
source("./paths.R")

z<-data.frame(row.names=c("Especialista","Generalista"))
z1<-data.frame()
z2<-data.frame()
w<-data.frame(row.names=c("Desktop","Mobile","Desktop e Mobile"))
w1<-data.frame()
w2<-data.frame()
w3<-data.frame()

ge<-data.frame()
dm<-data.frame()
for (it in 1:length(logs$sistema)){
      rm(list = ls()[!ls() %in% c("logs","it","z","w","z1","z2","w1","w2","w3","ge","dm")])
      #source(as.character(logs$caminho[it]) )
      load (as.character(logs$ws_analise[it]))

      
    
    x<-saida_plat%>% 
      filter(prob==0.00)
    
    ndev<-max(saida_plat$ndev)  
    gen<-nrow((filter(x, tipo == "gen")))*100/ndev   
    esp<-nrow((filter(x, tipo == "esp")))*100/ndev    
    x<-data.frame(porc=c(gen,esp),tipo=c("gen","esp"),sistema= c(as.character(logs$sistema[it]),as.character(logs$sistema[it])))
   # x<-x%>% mutate(sistema=as.character(logs$sistema[it]))
    ge<-rbind(ge,x)
    x<-x%>%select(porc)
    z<-cbind(z,x)
    z1<- rbind(z1,gen)
    z2<- rbind(z2,esp)

    
    y<-saida_device%>% 
      filter(prob==0.00)
    mobdes<-nrow((filter(y, tipo == "desktop mobile")))*100/ndev      
    mob<-nrow((filter(y, tipo == "mobile")))*100/ndev   
    des<-nrow((filter(y, tipo == "desktop")))*100/ndev    
    y<-data.frame(porc=c(des,mob,mobdes))
    
    y<-data.frame(porc=c(mobdes,mob,des),tipo=c("mobdes","mob","des"),sistema=c(as.character(logs$sistema[it]),as.character(logs$sistema[it]),as.character(logs$sistema[it])))
   # y<-y%>% mutate(sistema=as.character(logs$sistema[it]))
    dm<-rbind(dm,y)
    y<-y%>%select(porc)    
    
    w<-cbind(w,y)
    w1<- rbind(w1,mobdes)
    w2<- rbind(w2,mob)
    w3<- rbind(w3,des)


}

colnames(z1) <- "Generalista"
colnames(z2) <- "Especialista"
colnames(w1) <- "DesktopMobile"
colnames(w2) <- "Mobile"
colnames(w3) <- "Desktop"

Sistemas <- as.character(logs$sistema)

###################
##################
#Generalista e especialista
###################
###################
      dados1<-data.frame(Sistemas,z1,z2)
      #pt
 name <- paste ( "./graficos/pt/dist_genesp.pdf",sep = "" )
 pdf ( name , width = 8 , height = 6 )
     ggplot(ge) +
       aes(x = sistema, y = porc, fill = tipo) +
       geom_col()+
       ylim(0,100)+
       theme(text = element_text(size=16))+
       labs(fill = "Tipo", x=element_blank(), y="Porcentagem")+
       scale_fill_brewer(palette="Set1",labels  = c("Especialista","Generalista"))
  dev.off()
  
  name <- paste ( "./graficos/en/dist_genesp.pdf",sep = "" )
  pdf ( name , width = 8 , height = 6 )
    ggplot(ge) +
      aes(x = sistema, y = porc, fill = tipo) +
      geom_col()+
      ylim(0,100)+
      theme(text = element_text(size=16))+
      labs(fill = "Type", x=element_blank(), y="Percentage")+
      scale_fill_brewer(palette="Set1",labels  = c("Specialist","Generalist"))
  dev.off()

  
  
  
  #pt
  name <- paste ( "./graficos/pt/dist_mobdes.pdf",sep = "" )
  pdf ( name , width = 8 , height = 6 )
    ggplot(dm) +
      aes(x = sistema, y = porc, fill = tipo) +
      geom_col()+
      ylim(0,100)+
      theme(text = element_text(size=16))+
      labs(fill = "Tipo", x=element_blank(), y="Porcentagem")+
      scale_fill_brewer(palette="Set1",labels  = c("Desktop","Mobile", "Desktop e Mobile"))
  dev.off()

  #en
  name <- paste ( "./graficos/en/dist_mobdes.pdf",sep = "" )  
  pdf ( name , width = 8 , height = 6 )
    ggplot(dm) +
      aes(x = sistema, y = porc, fill = tipo) +
      geom_col()+
      ylim(0,100)+
      theme(text = element_text(size=16))+
      labs(fill = "Type", x=element_blank(), y="Percentage")+
      scale_fill_brewer(palette="Set1",labels  = c("Desktop","Mobile", "Desktop and Mobile"))
  dev.off()
 
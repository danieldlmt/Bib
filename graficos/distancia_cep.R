library("TSdist")
library("dtw")
library("ggplot2")
library("plyr")
library("reshape2")
library("lattice")
library("scales")
library("tidyr")
library(dplyr, warn.conflicts = FALSE)


# Create variable 'log'  that contains the paths of the scripts and workspaces (.RData)
source("./paths.R")
dist_cep<-data.frame()
nplat<-data.frame()
# Loop that walks through 'logs' to get .RData of each library
for (it in 1:length(logs$sistema)){

  load (as.character(logs$ws_analise[it]))

  
  x<-os_summary%>%select(n_commit)
  mat<-dist(x)
  
  MDR = as.data.frame(melt(as.matrix(mat)))  ### Insira a matriz obtida em matrizresult
  colnames(MDR) <- c("Datastream", "Algorithm", "Valor")
  
  #pt
  MDR <- ddply(MDR, .(Datastream), transform, rescale = rescale(Valor))
  p <- ggplot(MDR, aes(Datastream, Algorithm)) +
    geom_tile(aes(fill = Valor), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue")+
    labs(x=element_blank(), y=element_blank())+ 
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(),text = element_text(size=15))
  name <- paste ( "./graficos/pt/mapa_cep_",tolower( as.character( logs$sistema[it] ) ),".pdf",sep = "" )
  ggsave(name, p+ theme(axis.title.x=element_blank(), axis.title.y=element_blank()), width=5, height=3)
  
  #en
  colnames(MDR) <- c("Datastream", "Algorithm", "Value")
  p <- ggplot(MDR, aes(Datastream, Algorithm)) +
    geom_tile(aes(fill = Value), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue")+
    labs(x=element_blank(), y=element_blank())+ 
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(),text = element_text(size=15))
  name <- paste ( "./graficos/en/mapa_cep_",tolower( as.character( logs$sistema[it] ) ),".pdf",sep = "" )
  ggsave(name, p+ theme(axis.title.x=element_blank(), axis.title.y=element_blank()), width=5, height=3)
  
  
  
  x<-data.frame (porc = os_summary$n_commit*100/sum(os_summary$n_commit),
                 tipo = rownames(os_summary),
                 sistema = c(as.character(logs$sistema[it]),as.character(logs$sistema[it]),as.character(logs$sistema[it]),as.character(logs$sistema[it]),as.character(logs$sistema[it])))
  dist_cep<-rbind(dist_cep,x)
  
  
  rm(list = ls()[!ls() %in% c("logs","it","data","dist_cep")])                    
}



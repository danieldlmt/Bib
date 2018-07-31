library(dplyr)
library(ggplot2)
library(venn)
library(stringr)
#
###
######
#########
# Extract the plots as pdfs
#########
######
###
#

# Create variable 'log'  that contains the paths of the scripts and workspaces (.RData)
source("./paths.R")

# Loop that walks through 'logs' to get .RData of each library
for (it in 1:length(logs$sistema)){
  load (as.character(logs$ws_analise[it]))
  
  #Com quantas plataformas cada desenvolvedor trabalha?
  # pt
  name <- paste ( "./graficos/pt/dev_nplat_",tolower( as.character( logs$sistema[it] ) ),".pdf",sep = "" )

  aux<-authors3 %>% filter(n_platform!=0)
  ggplot(data=aux, aes(aux$n_platform)) + 
    geom_histogram(binwidth=.5)+
    labs(x="Número de plataformas", y="Número de desenvolvedores")+
    theme(text = element_text(size=16))+
    scale_x_continuous(breaks = c(1:5))
  ggsave(name,width=8, height=6)
  # en
  name <- paste ( "./graficos/en/dev_nplat_",tolower( as.character( logs$sistema[it] ) ),".pdf",sep = "" )

  ggplot(data=aux, aes(aux$n_platform)) + 
    geom_histogram(binwidth=.5)+
    labs(x="umber of platforms", y="Number of developers")+
    theme(text = element_text(size=16))+
    scale_x_continuous(breaks = c(1:5))
  ggsave(name,width=8, height=6)
  
  rm(list = ls()[!ls() %in% c("logs","it","data","dist_cipcep")])                    
}
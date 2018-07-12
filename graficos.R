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
   source("~/bib/paths.R")

# Loop that walks through 'logs' to get .RData of each library
for (it in 1:length(logs$sistema)){
  
  ################################################################################################################## 
  # QP1 - Como é a distribuição de tarefas de manutenção entre o código independente e específico de plataforma?\n") 
  ##################################################################################################################
  load (as.character(logs$ws_analise[it]))
              # Portugues
                name <- paste ( "~/bib/graficos/pt/qp1_",tolower( as.character( logs$sistema[it] ) ),".pdf",sep = "" )
                pdf ( name , width = 8 , height = 6 )
                par(mar=c(5.1, 5, 4.1, 2.1)) 
                barplot(os_summary3$n_commit, names.arg =c("Independente","Específico","Ambos") , xlab = "Tipo", col=c("blue","orange","darkgreen"), ylab = "Número de modificações", cex.names = 1.7,cex.axis = 1.7,cex.lab=1.7)
                legend(0.7, os_summary3$n_commit[1]/2, "          ",yjust = 0.5,       xjust = 0.5, bg="white")
                legend(1.9, os_summary3$n_commit[2]/2, "          ",yjust = 0.5,       xjust = 0.5, bg="white")
                legend(3.1, os_summary3$n_commit[3]/2 , "          ",yjust = 0.5,       xjust = 0.5, bg="white")
                text(0.7,os_summary3$n_commit[1]/2,os_summary3$n_commit[1],cex=1.7, bg="white")
                text(1.9,os_summary3$n_commit[2]/2,os_summary3$n_commit[2],cex=1.7, bg="white")
                text(3.1,os_summary3$n_commit[3]/2,os_summary3$n_commit[3],cex=1.7, bg="white")  
                dev.off()
              
              #Ingles
                name <- paste ( "~/bib/graficos/en/qp1_",tolower( as.character( logs$sistema[it] ) ),".pdf",sep = "" )
                pdf ( name )
                pdf ( name , width = 8 , height = 6 )
                barplot(os_summary3$n_commit, names.arg =c("Independent","Specific","Both") , xlab = "Type", col=c("blue","orange","darkgreen"), ylab = "Number of commits", cex.names = 1.7,cex.axis = 1.7,cex.lab=1.7)
                legend(0.7, os_summary3$n_commit[1]/2, "          ",yjust = 0.5,       xjust = 0.5, bg="white")
                legend(1.9, os_summary3$n_commit[2]/2, "          ",yjust = 0.5,       xjust = 0.5, bg="white")
                legend(3.1, os_summary3$n_commit[3]/2 , "          ",yjust = 0.5,       xjust = 0.5, bg="white")
                text(0.7,os_summary3$n_commit[1]/2,os_summary3$n_commit[1],cex=1.7, bg="white")
                text(1.9,os_summary3$n_commit[2]/2,os_summary3$n_commit[2],cex=1.7, bg="white")
                text(3.1,os_summary3$n_commit[3]/2,os_summary3$n_commit[3],cex=1.7, bg="white")  
                dev.off()
              
  
              # QP1.1 Qual plataforma é modificada com mais frequência?.\n")
                      # Portugues
                        name <- paste ( "~/bib/graficos/pt/qp1_1_",tolower( as.character( logs$sistema[it] ) ),".pdf",sep = "" )
                        pdf ( name , width = 8 , height = 6 )
                        par(mar=c(5.1, 5, 4.1, 2.1)) 
                        barplot(os_summary$n_commit, names.arg = row.names(os_summary) ,col=rainbow(5), xlab = "Plataforma", ylab = "Número de modificações",cex.names = 1.7,cex.axis = 1.7,cex.lab=1.7)
                        legend(0.7,os_summary$n_commit[1]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5) 
                        legend(1.92,os_summary$n_commit[2]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5) 
                        legend(3.1,os_summary$n_commit[3]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5) 
                        legend(4.3,os_summary$n_commit[4]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5) 
                        legend(5.5,os_summary$n_commit[5]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5) 
                        text(0.7,os_summary$n_commit[1]/2,os_summary$n_commit[1],cex=1.7, bg="white")
                        text(1.92,os_summary$n_commit[2]/2,os_summary$n_commit[2],cex=1.7, bg="white")
                        text(3.1,os_summary$n_commit[3]/2,os_summary$n_commit[3],cex=1.7, bg="white")
                        text(4.3,os_summary$n_commit[4]/2,os_summary$n_commit[4],cex=1.7, bg="white")
                        text(5.5,os_summary$n_commit[5]/2,os_summary$n_commit[5],cex=1.7, bg="white")
                        dev.off()
                        
                      # Ingles
                        name <- paste ( "~/bib/graficos/en/qp1_1_",tolower( as.character( logs$sistema[it] ) ),".pdf",sep = "" )
                        pdf ( name , width = 8 , height = 6 )
                        par(mar=c(5.1, 5, 4.1, 2.1)) 
                        barplot(os_summary$n_commit, names.arg = row.names(os_summary) ,col=rainbow(5), xlab = "Platform", ylab = "Number of commits",cex.names = 1.7,cex.axis = 1.7,cex.lab=1.7)
                        
                        legend(0.7,os_summary$n_commit[1]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5) 
                        legend(1.92,os_summary$n_commit[2]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5) 
                        legend(3.1,os_summary$n_commit[3]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5,) 
                        legend(4.3,os_summary$n_commit[4]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5,) 
                        legend(5.5,os_summary$n_commit[5]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5 ) 
                        text(0.7,os_summary$n_commit[1]/2,os_summary$n_commit[1],cex=1.7, bg="white")
                        text(1.92,os_summary$n_commit[2]/2,os_summary$n_commit[2],cex=1.7, bg="white")
                        text(3.1,os_summary$n_commit[3]/2,os_summary$n_commit[3],cex=1.7, bg="white")
                        text(4.3,os_summary$n_commit[4]/2,os_summary$n_commit[4],cex=1.7, bg="white")
                        text(5.5,os_summary$n_commit[5]/2,os_summary$n_commit[5],cex=1.7, bg="white")
                        dev.off()
 rm(list = ls()[!ls() %in% c("logs","it","data")])                    
 ################################################################################################################## 
 # QP2 - A equipe de desenvolvimento de bibliotecas multiplataforma é formada majoritariamente 
 #       por desenvolvedores generalistas ou especialistas em plataformas?
 ################################################################################################################## 
                      
                      
           # QP2.1 - A equipe de desenvolvimento de bibliotecas multiplataforma tende  a tornar-se 
           #         mais generalista ou especialita em plataformas ao lonto do tempo?\n")    
                      
                      
 ################################################################################################################## 
 # QP3 - Os desenvolvedores de bibliotecas multiplataforma são especialistas em plataformas de um determinado 
 #       tipo de dispositivo ou trabalham com plataformas de mais de um tipo de dispositivo?\n")
 ##################################################################################################################   
  
  
  
  
  
         # QP3.1 - Os desenvolvedores de bibliotecas multiplataforma tendem a especializar-se 
         #         em dispositivos desktop ou mobile ao longo do tempo?\n")
  
  
  
  
  
  
  
  
  
  
  
  
}
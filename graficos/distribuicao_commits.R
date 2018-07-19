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
dist_cipcep<-data.frame()
nplat<-data.frame()
# Loop that walks through 'logs' to get .RData of each library
for (it in 1:length(logs$sistema)){
  
  ################################################################################################################## 
  # QP1 - Como é a distribuição de tarefas de manutenção entre o código independente e específico de plataforma?\n") 
  ##################################################################################################################
  load (as.character(logs$ws_analise[it]))
  
  x<-data.frame (porc = os_summary3$n_commit*100/sum(os_summary3$n_commit),
                 tipo = rownames(os_summary3),
                 sistema = c(as.character(logs$sistema[it]),as.character(logs$sistema[it]),as.character(logs$sistema[it])))
  dist_cipcep<-rbind(dist_cipcep,x)
  

  
  
              # Portugues
                name <- paste ( "./graficos/pt/dist_cipcep_",tolower( as.character( logs$sistema[it] ) ),".pdf",sep = "" )
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
                name <- paste ( "./graficos/en/dist_cipcep_",tolower( as.character( logs$sistema[it] ) ),".pdf",sep = "" )
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
                        name <- paste ( "./graficos/pt/dist_cep_",tolower( as.character( logs$sistema[it] ) ),".pdf",sep = "" )
          pdf ( name , width = 8 , height = 6 )
                        par(mar=c(5.1, 5, 4.1, 2.1)) 
                        barplot(os_summary$n_commit, 
                                names.arg = row.names(os_summary),
                                col=rainbow(5), 
                                xlab = "Plataforma", 
                                ylab = "Número de modificações",
                                cex.names = 1.7,cex.axis = 1.7,cex.lab=1.7)
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
                        name <- paste ( "./graficos/en/dist_cep_",tolower( as.character( logs$sistema[it] ) ),".pdf",sep = "" )
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
                        
          
                        
                                      
          ##### QP2.1 - Dos desenvolvedores   que trabalham com uma plataforma, como é a distribuição das plataformas?\n")
                  
                  #pt
                   name <- paste ( "./graficos/pt/dist_devcep_",tolower( as.character( logs$sistema[it] ) ),".pdf",sep = "" )
   pdf ( name , width = 8 , height = 6 )
                   par(mar=c(5.1, 5, 4.1, 2.1))                       
                        barplot(devesp,names.arg = row.names(os_summary) , ylim = c(0,n_dev),col=rainbow(5), cex.names = 1.7,cex.axis = 1.7,cex.lab=1.7, xlab = "Plataforma", ylab = "Número de desenvolvedores")
                        #allegro
                        if ( devesp[1]>5 ){
                          legend(0.7,devesp[1]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5) 
                          text(0.7,devesp[1]/2,devesp[1],cex=1.7)}
                        if ( devesp[2]>5 ){
                        legend(1.92,devesp[2]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5) 
                          text(1.92,devesp[2]/2,devesp[2],cex=1.7)}
                        if ( devesp[3]>5 ){
                        legend(3.1,devesp[3]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5) 
                          text(3.1,devesp[3]/2,devesp[3],cex=1.7)}
                        if ( devesp[4]>5 ){
                         legend(4.3,devesp[4]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5) 
                          text(4.3,devesp[4]/2,devesp[4],cex=1.7)}
                        if ( devesp[5]>5 ){
                        legend(5.5,devesp[5]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5) 
                          text(5.5,devesp[5]/2,devesp[5],cex=1.7) }
   dev.off()
                 
                 #en
                 name <- paste ( "./graficos/en/dist_devcep_",tolower( as.character( logs$sistema[it] ) ),".pdf",sep = "" )
   pdf ( name , width = 8 , height = 6 )
                 par(mar=c(5.1, 5, 4.1, 2.1))                       
                 barplot(devesp,names.arg = row.names(os_summary) , ylim = c(0,n_dev),col=rainbow(5), cex.names = 1.7,cex.axis = 1.7,cex.lab=1.7, xlab = "Platform", ylab = "Number of developers")
                 #allegro
                 if ( devesp[1]>5 ){
                   legend(0.7,devesp[1]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5) 
                   text(0.7,devesp[1]/2,devesp[1],cex=1.7)}
                 if ( devesp[2]>5 ){
                   legend(1.92,devesp[2]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5) 
                   text(1.92,devesp[2]/2,devesp[2],cex=1.7)}
                 if ( devesp[3]>5 ){
                   legend(3.1,devesp[3]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5) 
                   text(3.1,devesp[3]/2,devesp[3],cex=1.7)}
                 if ( devesp[4]>5 ){
                   legend(4.3,devesp[4]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5) 
                   text(4.3,devesp[4]/2,devesp[4],cex=1.7)}
                 if ( devesp[5]>5 ){
                   legend(5.5,devesp[5]/2,"      ",bg="white",yjust = 0.5,       xjust = 0.5) 
                   text(5.5,devesp[5]/2,devesp[5],cex=1.7) }
    dev.off()
    
 rm(list = ls()[!ls() %in% c("logs","it","data","dist_cipcep")])                    
}
#pt
name <- paste ( "./graficos/pt/dist_cipcep.pdf",sep = "" )
pdf ( name , width = 8 , height = 6 )
ggplot(dist_cipcep) +
  aes(x = sistema, y = porc, fill = tipo) +
  geom_col()+
  ylim(0,100)+
  theme(text = element_text(size=16))+
  labs(fill = "Tipo", x=element_blank(), y="Porcentagem")+
  scale_fill_brewer(palette="Dark2",labels  = c("CIP e CEP","CIP","CEP"))
dev.off()
#en
name <- paste ( "./graficos/en/dist_cipcep.pdf",sep = "" )
pdf ( name , width = 8 , height = 6 )
ggplot(dist_cipcep) +
  aes(x = sistema, y = porc, fill = tipo) +
  geom_col()+
  ylim(0,100)+
  theme(text = element_text(size=16))+
  labs(fill = "Type", x=element_blank(), y="Percentage")+
  scale_fill_brewer(palette="Dark2",labels  = c("PIC and PSC","PIC","PSC"))
dev.off()




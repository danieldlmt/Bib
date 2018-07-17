library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(tidyr)
library(tseries)

# Extract the time series acumulating the number of developers in each iteration
# It means, it extract the original time series

################################
# Carregamento do log de commits
# Escolha apenas um log
# 
###############################


# Create variable 'log'  that contains the paths of the scripts and workspaces (.RData)
source("./paths.R")

for (it in 1:length(logs$sistema)){
  rm(list = ls()[!ls() %in% c("logs","it")])
  #source(as.character(logs$caminho[it]) )
  #load ("./workspace/allegro_data.RData")
  load(as.character(logs$data[it]) )
  
  #variaveis bind
  saida_plat_bind <- data.frame()
  saida_device_bind <- data.frame()
  
  
  
  ########################################
  ########################################
  ########################################
  #inicio da analise temporal
  #######################################
  #######################################
  #######################################
  
  platform3 <- data %>%
    select(rev, platform,author, date) %>%
    group_by(rev) %>%
    arrange(platform)%>%
    summarise( n_plat = n_distinct(platform), platform = paste(unique(platform), collapse=", "), author=unique(author), date= unique(date)) %>%
    arrange(n_plat,platform, rev)
  
  #upgrading module3 table
  platform3 <- platform3 %>% arrange (rev)
  #modules3 [,"n_files"]  <- revs %>% arrange(rev) %>% select (n_files)
  file_list <- data %>%
    select(rev, path) %>%
    group_by(rev) %>%
    arrange(path)%>%
    summarise( n_files = n_distinct(path), path = paste(unique(path), collapse=", "))
  platform3  [,"n_files"] <-  file_list %>% arrange(rev) %>% select (n_files)
  platform3  [,"path"] <-  file_list %>% arrange(rev) %>% select (path)
  platform3 <- platform3 %>% arrange(n_plat,platform, rev)
  
  
  date_ini<- min(as.POSIXct(platform3$date))
  date_left<- min(as.POSIXct(platform3$date))
  date_right<- max(as.POSIXct(platform3$date))
  nperiods <- difftime(date_right,date_left, units = "weeks")/4
  date_right<- seq.POSIXt( date_left, length=2, by='4 weeks')[2] 

    dev_all_time<-data.frame(tipo=NA,n=NA, porcentage=NA,iteracao=NA, date_left=NA, date_right=NA)
  dev_gen_time<-data.frame(n_platform=NA,n=NA, porcentage=NA,iteracao=NA, date_left=NA, date_right=NA)
  
  
  #geral
  devicetype_time <- data.frame(developer=NA, dispositivo="")
  devicetype_time <- devicetype_time %>% mutate(porcentage= (developer/sum(developer))*100, tipo = "todos", iteracao = NA, date_left = as.POSIXct( date_left), date_right = as.POSIXct( date_right ))
  
  #rules_dev_time <- data.frame(lhs=NA,o=NA,rhs=NA, support=NA, confidence=NA, lift=NA, iteracao=NA, date_left = as.POSIXct( date_left), date_right = as.POSIXct( date_right )) 
  
  #rules_commit_time <- data.frame(lhs=NA,o=NA,rhs=NA, support=NA, confidence=NA, lift=NA, iteracao=NA, date_left = as.POSIXct( date_left), date_right = as.POSIXct( date_right )) 
  
  qtd_dev <- data_frame(iteracao=rep(0,as.integer(nperiods)),n_dev=rep(0,as.integer(nperiods)))
  
  for (j in 1:as.integer(nperiods)){
   
    ###################################################
    ##################################################
    ######################################################
    #####################################################
    ###########################################################3
    ##########################################################
    
    # Remove outliers from 'commits' and 'added lines' to better calculate thresholds
    # limiar_commit - vector for commits
    # limiar_line - vector for added lines
    source("./tresholds.R" )
    # calculate the total number of developers in the window  
    data_aux <- data%>%
      filter(as.POSIXct(date)>=date_left  & as.POSIXct(date)<date_right )  
    n_dev<-n_distinct(data_aux$author)
    
    
    #Classificao de devs de acordo com os limiares
    # nova variavel para classificacao do desenvolvedor entre generalista e especialista em plataformas
    platform_new <- data_aux %>%
      select( platform, author,n_line_add,n_line_del,rev,path,date)%>%
      group_by(author,platform) %>%
      summarise(n_line_add=sum(n_line_add),
                n_line_del=sum(n_line_del), 
                commits=n_distinct(rev),
                files=n_distinct(path), 
                first=min(as.POSIXct(date)),
                last=max(as.POSIXct(date)) ) %>%
      mutate( ind = if_else(platform == "Independente",1,0))    
    
    #Classify the devs     
    source("./devclassificacao.R")
    rm(platform_new)
    
    saida_plat<-saida_plat%>%
      group_by(tipo,prob)%>%
      summarise(iteracao= max(iteracao),n=n(), ndev=max(ndev))%>%
      mutate(porc=n*100/ndev)%>%
      select(iteracao,prob,tipo, n, porc, ndev)
    #select(iteracao,author,prob,ind,esp,gen,mobile,desktop,indd,tipo,ndev)
    #%>%
    # group_by(prob,tipo)%>%     
    #summarise(n = n())
    saida_plat_bind<-bind_rows(saida_plat_bind,saida_plat)
    rm(saida_plat)
    
    saida_device <- platform_new_bind%>%
      mutate(iteracao = j,
             tipo = if_else(desktop==1&mobile==0,"desktop",
                            if_else(desktop==0&mobile==1,"mobile",
                                    if_else(desktop==1&mobile==1,"desktop mobile","nada") )))
    
    
    
    saida_device<-saida_device%>%
      group_by(tipo,prob)%>%
      summarise(iteracao= max(iteracao),n=n(), ndev=max(ndev))%>%
      mutate(porc=n*100/ndev)%>%
      select(iteracao,prob,tipo, n, porc, ndev)      
    
    
    saida_device_bind<-bind_rows(saida_device_bind,saida_device)
    rm(platform_new_bind, saida_device, data_aux)
    #%>%
    # group_by(prob,tipo)%>%     
    #summarise(n = n())
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #date_left <- date_right
    date_right <- seq.POSIXt( date_right, length = 2, by = '4 weeks' ) [ 2 ] 
    
  }
  
  
  
  
  save.image(as.character(logs$ws_series_acumuladas[it]))
}

##
########
##################
############################
###################################
######################## Plots
#######################################
##############################
#####################
#############
######
#

# Generalista vs especialista
#plot(dev_all_time2$iteracao, dev_all_time2$porcentage, col="darkslateblue", type="b",pch=20, xlab="Tempo (semanas)", ylim=c(0,100),ylab="% de generalistas")  

#mobile vs desktop
#plot(devicetype_time2$iteracao, devicetype_time2$porcentage, col="darkslateblue", type="b",pch=20, xlab="Tempo (semanas)",ylim=c(0,100), ylab="Porcentagem")



















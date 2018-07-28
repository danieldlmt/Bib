library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(tidyr)
library(tseries)

#Para testar a serie temporal com limiares
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
  load (as.character(logs$data[it]))
 
  #load ("./workspace/allegro_data.RData")
  
  # Filtro de desenvolvedores ativos 
  # periodo de contribuição minimo de 24 semanas
  source("./devsativos.R")
  dev_ativo<-dev_ativo %>% select(author)
  data<- right_join(data, dev_ativo,by="author")
  
  ########################################
  ########################################
  ########################################
  #inicio da analise temporal com janela deslizante
  #######################################
  #######################################
  #######################################
  
  platform3 <- data %>%
    filter(platform!="Independente") %>% #filter devs that work with independent code
    select(rev, platform,author, date,n_line_add,n_line_del,path) %>%
    group_by(rev) %>%
    arrange(platform)%>%
    summarise( n_plat = n_distinct(platform), platform = paste(unique(platform), collapse=", "), author=unique(author), date= unique(date),n_line_add=max(n_line_add),n_line_del=max(n_line_del)) %>%
    arrange(n_plat,platform, rev)
  
  #upgrading module3 table
  platform3 <- platform3 %>% 
    arrange (rev)
  #modules3 [,"n_files"]  <- revs %>% arrange(rev) %>% select (n_files)
  file_list <- data %>%
    filter(platform!="Independente") %>% #filter devs that work with independent code
    select(rev, path) %>%
    group_by(rev) %>%
    arrange(path)%>%
    summarise( n_files = n_distinct(path), path = paste(unique(path), collapse=", "))
  platform3  [,"n_files"] <-  file_list %>% arrange(rev) %>% select (n_files)
  platform3  [,"path"] <-  file_list %>% arrange(rev) %>% select (path)
  platform3 <- platform3 %>% arrange(n_plat,platform, rev)
  rm(file_list)
  
  date_ini<- min(as.POSIXct(platform3$date))
  date_left<- min(as.POSIXct(platform3$date))
  date_right<- max(as.POSIXct(platform3$date))
  nperiods <- difftime(date_right,date_left, units = "weeks")/4
  date_right<- seq.POSIXt( date_left, length=2, by='24 weeks')[2] 
  
  ###########################################################3
  ##########################################################
  
  #variaveis bind
  saida_plat_bind <- data.frame()
  saida_device_bind <- data.frame()
  

  ####################3
  ###################3
  # Janela 24 semanas
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
    #limiar_line<-xlimiar_line
    # calculate the total number of developers in the window  
    data_aux <- data%>%
      filter(platform!="Independente") %>% #filter devs that work with independent code
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

    ########################
    #########################
    # atualiza variaveis de tempo
    #########################
    #date_left <- date_right
    date_right <- seq.POSIXt( date_right, length = 2, by = '4 weeks' ) [ 2 ] 
    
    # Janela de tempo deslizante
    if (as.integer(difftime(date_right,date_left,units = "weeks"))>24)
      date_left<- seq.POSIXt( date_right, length = 2, by = '-24 weeks' ) [ 2 ]    
    ##############################
    ##########################3
    ###########################
    
  }#fim for

  save.image(as.character(logs$ws_series_agrupadas[it]))
}

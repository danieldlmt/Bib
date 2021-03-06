# Libraries
library(dplyr, warn.conflicts = FALSE)
library(stringr)


########################################
########################################
########################################
#inicio da analise 
#######################################
#######################################
#######################################

# Create variable 'log'  that contains the paths of the scripts and workspaces (.RData)
  source("./paths.R")

# Loop that walks through 'logs' and save the results of each system in 'logs$ws_analise'
  for (it in 1:length(logs$sistema)){
    rm(list = ls()[!ls() %in% c("logs","it")])
    #source(as.character(logs$caminho[it]) )
    load (as.character(logs$data[it]))
    
    # Filtro de desenvolvedores ativos 
      # periodo de contribuição minimo de 24 semanas
          source("./devsativos.R")
          dev_ativo<-dev_ativo %>% select(author)
          data<- right_join(data, dev_ativo,by="author")
    
  
          
  commits <- as.data.frame.matrix(xtabs(~ rev + platform, data=data[c('platform', 'rev')]))
  commits <- commits %>% select (Android, Linux, Windows, iPhone, macOS, Independente)  %>%  filter (  !(rowSums(commits) == 0) )
  rownames(commits)<-NULL
  commits<- ifelse(commits>0, 1, 0)
  ######################################
  # numero de plataformas modificadas em cada commit
  platform <- data %>%
    select(rev, platform, author,n_line_add,n_line_del)%>% 
    group_by(rev) %>%
    summarise(n_plat = n_distinct(platform), platform = paste(unique(platform), collapse=", "), author=unique(author), n_line_add=max(n_line_add),n_line_del=max(n_line_del)) %>%
    arrange(rev)

  # lista de arquivos modificados em cada commit
  file_list <- data %>%
    select(rev, path) %>%
    group_by(rev) %>%
    arrange(path)%>%
    summarise( n_files = n_distinct(path), path = paste(unique(path), collapse=", "))
  
  # upgrade em platform, + numero de arquivos modificados em cada commit
  platform <- as.data.frame (merge(platform,file_list,by="rev"))
  
  
  # quantidade de commits agrupados de acordo com o numero de plataformas
  # por exemplo, 100 commits modificaram 2 plataformas, 40 commits modificaram 3 plataformas...
  platform2 <- platform %>%
    mutate(n_plat = if_else(str_detect(platform, "Independente"), as.numeric(n_plat-1) ,as.numeric(n_plat) )) %>%
    group_by(n_plat) %>%
    summarise(n_rev= n())%>%
    mutate (porcentage = round(n_rev/sum(n_rev) * 100, 1) ) %>%
    arrange(n_plat)
  
  # verificacao de modificacao do diretorio independente nos commits que modificaram mais de um diretorio 
  platform4 <- platform %>% filter(!platform=="Independente")
  porcentage_independente <- sum( str_detect(platform4$platform,"Independente") ) * 100 / nrow(platform4) 
  
  n_independente <- sum(platform$n_plat==1 & platform$platform=="Independente" )
  n_especifico_total <- sum(platform$n_plat==1 & platform$platform!="Independente")
  n_esp_ind_total <- sum(platform$n_plat>1 & is.element("Independente", platform$platform))
  
  # numero de vezes que cada arquivo foi modificado
  files <- data %>% group_by(path) %>% summarise( qtd = n() ) %>%  arrange(qtd)
  
  # numero de arquivos modificados em cada commit
  revs <- data %>% group_by(rev) %>% summarise( n_files = n() ) %>%  arrange(rev)
  
  # quantidade de modificacoes que cada plataforma sofreu
  platform5 <- data %>% select (platform) %>% group_by(platform) %>% summarise(n=n())
  
  #numero de modificacoes que cada desenvolvedor fez em cada plataforma
  independente <- platform %>% filter(str_detect(platform, "Independente")) %>% select(author) %>% group_by(author)%>% summarise(independente=n())
  android <- platform %>% filter(str_detect(platform, "Android")) %>% select(author) %>% group_by(author)%>% summarise(android=n())
  linux <- platform %>% filter(str_detect(platform, "Linux"))%>% select(author) %>% group_by(author)%>% summarise(linux=n())
  win <- platform %>% filter(str_detect(platform, "Windows"))%>% select(author) %>% group_by(author)%>% summarise(win=n())
  iphone <- platform %>% filter(str_detect(platform, "iPhone"))%>% select(author) %>% group_by(author)%>% summarise(iphone=n())
  macosx <- platform %>% filter(str_detect(platform, "macOS"))   %>% select(author) %>% group_by(author)%>% summarise(macosx=n())
  
  max.len <- max (nrow(independente),nrow(android),nrow(linux),nrow(android),nrow(win),nrow(iphone),nrow(macosx))
  
  # lista de desenvolvedores que programam para cada plataforma
  authors<- data.frame( 
    independente=c(as.character(independente$author), rep(NA, max.len - length(independente$author))),
    android=c(as.character(android$author), rep(NA, max.len - length(android$author))),
    linux=c(as.character(linux$author), rep(NA, max.len - length(linux$author))),
    win=c(as.character(win$author), rep(NA, max.len - length(win$author))), 
    iphone=c(as.character(iphone$author), rep(NA, max.len - length(iphone$author))), 
    macosx=c(as.character(macosx$author), rep(NA, max.len - length(macosx$author))) )
  
  # numero de plataformas suportadas por cada desenvolvedor
  authors3 <- data %>% select(author) %>% distinct(author) %>% mutate(id=as.numeric(author))
  authors3 <-merge(authors3,independente,by="author", all.x = TRUE)
  authors3 <-merge(authors3,android,by="author", all.x = TRUE)
  authors3 <-merge(authors3,linux,by="author", all.x = TRUE)
  authors3 <-merge(authors3,win,by="author", all.x = TRUE)
  authors3 <-merge(authors3,iphone,by="author", all.x = TRUE)
  authors3 <-merge(authors3,macosx,by="author", all.x = TRUE)
  authors3 <- authors3 %>% rowwise() %>%  mutate(n_platform=5-(is.na(android)+is.na(linux)+is.na(win)+is.na(iphone)+is.na(macosx)), n_commit= sum(independente,android,linux,win,iphone,macosx,NA, na.rm = TRUE)  )
  ## Filtrar autores que nao trabalharam com plataformas e src
  authors3 <- authors3 %>% filter( !(is.na(independente) & n_platform == 0))
  ##adiciona coluna desenvolvedor especialista e generalista ()
  authors3 <- authors3 %>% mutate( tipo = if_else( n_platform > 1 ,  "gen" , if_else( n_platform == 1 , "esp", NA_character_)) )
  authors3_aux<- authors3 %>% select(author ,tipo)
  

  # numero total de modificacoes realizadas em cada plataforma
  os_summary <- data.frame(n_commit = c(sum(android$android),sum(linux$linux),sum(win$win),sum(iphone$iphone),sum(macosx$macosx)), n_dev = c(nrow(android),nrow(linux),nrow(win),nrow(iphone),nrow(macosx)), row.names= c('Android', 'Linux', 'Windows', 'iPhone', 'macOS'))
  os_summary2 <- as.data.frame( t(os_summary))    
  os_summary3 <- as.data.frame(t(data.frame(independente=as.integer(n_independente),especifico=as.integer(n_especifico_total),ambos=as.integer(n_esp_ind_total),row.names = "n_commit")))
  
  # distribuicao de modificacoes entre as plataformas
  platform3 <- platform %>% filter(n_plat==1)
  platform3 <-platform3 %>% select(platform) %>% group_by(platform) %>% summarise( n = n() ) %>% filter(!platform=="Independente")
  #platform3 <-platform3 %>% mutate( porcentage = n*100/sum(n))
  rownames(platform3)<-platform3$platform
  platform3<-merge(os_summary, platform3, by="row.names")
  platform3<- data.frame(platform=platform3$platform ,mod_total=platform3$n_commit,mod_uma=platform3$n,row.names = platform3$platform)
  platform3<- platform3 %>% mutate(moduma_modtotal=mod_uma*100/mod_total , modtotal_summodtotal=mod_total*100/sum(mod_total))
  
  
  # graph - relacao entre numero de plataformas e arquivos modificados em um commit   
  graf1 <- platform %>% select (n_plat,n_files) %>% group_by(n_plat) %>% summarise(n_files= sum(n_files)/n())
  
  
  ########################
  ########################
  # inicio QP1
  ########################
  ########################
  
  # distribuição de desenvolvedores especialistas e generalistas
  devall <- authors3%>% 
    select(tipo) %>%  
    group_by(tipo) %>% 
    summarise(n_dev=n())%>%
    mutate(porcentage = round(n_dev/sum(n_dev) * 100, 1)) 
  colnames(devall) <- c("tipo", "n", "porc_dev")
  

  
    
  
  devgen <- authors3%>% 
    filter (tipo=="gen") %>%
    select(n_platform) %>% 
    group_by(n_platform)%>%
    summarise(n=n())%>%
    mutate(porcentage = round(n/sum(n) * 100, 1)) 
  colnames(devgen) <- c("n_platform", "n_dev_gen", "porc_gen")
  
  devesp<- authors3%>% 
    filter (tipo=="esp") %>%
    select(android,linux,win,iphone,macosx) 
  if(length(devesp$android)){
     devesp <- ifelse(devesp>0, 1, 0)
  }
  devesp <- colSums(devesp,na.rm=T)
  
  
  authors3_plats<-authors3 %>%select(n_platform)%>% filter(n_platform>0) %>%group_by(n_platform) %>%summarise(n = n())
  # resposta qp1
  #   aux <-  merge(devall, devcore, by = "n_platform", all=T)  
  #   pp1 <- merge(aux, devperi, by = "n_platform", all=T) 
  #   remove(aux)
  ########################
  ########################
  # fim QP1
  ########################
  ########################
  
  ########################
  ########################
  # inicio QP2
  ########################
  ########################
  
  

  
  # [todos] numero de modificacoes por plataforma de cada desenvolvedor  
  authors41 <- authors3 %>% select(android,linux,win, iphone,macosx, n_commit ) 
  authors41[colnames(authors41)][is.na(authors41[colnames(authors41)])] <- 0
  
  # [peri] numero de modificacoes por plataforma de cada desenvolvedor  
  authors42 <- authors3 %>% 
    arrange(desc(n_commit)) %>%
    head(ceiling(nrow(authors3)*0.2)) %>% 
    select(android,linux,win, iphone,macosx, n_commit ) 
  authors42[colnames(authors42)][is.na(authors42[colnames(authors42)])] <- 0
  
  # [core] numero de modificacoes por plataforma de cada desenvolvedor  
  authors43 <- authors3 %>% 
    arrange(desc(n_commit)) %>%
    head(-ceiling(nrow(authors3)*0.2)) %>% 
    select(android,linux,win, iphone,macosx, n_commit ) 
  authors43[colnames(authors43)][is.na(authors43[colnames(authors43)])] <- 0
  
  # [todos] numero de modificacoes por dispositivo de cada desenvolvedor  
  authors6<- authors41 %>% mutate(Desktop=linux+win+macosx, Mobile=android+iphone)%>% select (Desktop,Mobile)
  # [peri] numero de modificacoes por dispositivo de cada desenvolvedor  
  authors62<- authors42 %>% mutate(Desktop=linux+win+macosx, Mobile=android+iphone)%>% select (Desktop,Mobile)
  # [core] numero de modificacoes por dispositivo de cada desenvolvedor  
  authors63<- authors43 %>% mutate(Desktop=linux+win+macosx, Mobile=android+iphone)%>% select (Desktop,Mobile)
  
  # [todos] numero de dispositivos que os desenvolvedores trabalham
  devicetype <- data.frame(dispositivo = c("None", "Desktop", "Mobile", "Both"), n_todos=c(
    filter(authors6, Desktop==0 &Mobile==0) %>% nrow(),
    filter(authors6, Desktop>0 &Mobile==0) %>% nrow(),
    filter(authors6, Desktop==0 &Mobile>0) %>% nrow(),
    filter(authors6, Desktop>0 &Mobile>0) %>% nrow() ) )
  devicetype <- devicetype %>% mutate(porc_todos = (n_todos/sum(n_todos))*100)
  
  # resultado qp2
  # aux <- merge(devicetype,devicetype3, by = "dispositivo", all = T)
  # devicetype2, by= "dispositivo", all = T) 
  # remove(aux)
  
  ########################
  ########################
  # fim QP2
  ########################
  ########################
  
  
  
  
  
  #
  ####
  #######
  ##########
  ############
  # final script (rest is going to become obsolete)
    # Generalist and Specialist developer
    # Division by device type
  #############
  ###########
  ########
  #####
  ##
  #

  # Remove outliers from 'commits' and 'added lines' to better calculate thresholds
    # limiar_commit - vector for commits
    # limiar_line - vector for added lines
      source("./tresholds.R" )

  # calculate the total number of developers in the window  
  data_aux<- data%>% filter(platform!="Independente") #filter devs that work with independent code
  n_dev<-n_distinct(data_aux$author)
  j<-1
  
   
    # Transformation in 'data' to make the analysis
    platform_new <- data_aux %>%
      select( platform, author,n_line_add,n_line_del,rev,path,date)%>%
      group_by(author,platform) %>%
      summarise(n_line_add=sum(n_line_add)/n_distinct(path), # mean line_add/file
                n_line_del=sum(n_line_del)/n_distinct(path), # mean line_dell/file
                commits=n_distinct(rev),
                files=n_distinct(path), 
                first=min(as.POSIXct(date)),
                last=max(as.POSIXct(date)) ) %>%
      mutate( ind = if_else(platform == "Independente",1,0))
 
 #Classify the devs     
 source("./devclassificacao.R")
  
  # x1 e x2 sao as variaveis de saida  
  saida_plat <- platform_new_bind%>%
    mutate(tipo = if_else(esp==1&gen==0,"esp",
                          if_else(esp==0&gen==1,"gen",
                                    if_else(esp==1&gen==1,"esp gen","nada") )))
  
  #%>%
  # group_by(prob,tipo)%>%     
  #summarise(n = n())
  
  saida_device <- platform_new_bind%>%
    mutate(tipo = if_else(desktop==1&mobile==0,"desktop",
                          if_else(desktop==0&mobile==1,"mobile",
                                  if_else(desktop==1&mobile==1,"desktop mobile","nada") )))
  #%>%
  # group_by(prob,tipo)%>%     
  #summarise(n = n())
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # save workspace .RData in 'logs$ws_analise'
  save.image(as.character(logs$ws_analise[it]))
}



########################################
########################################
########################################
#fim da analise 
#######################################
#######################################
#######################################
#remove(devall, devperi, devcore, authors41,authors42,authors43,authors6,authors62, authors63,devicetype, devicetype2, devicetype3, file_list, files,linux, independente, iphone, macosx,android,win, authors, max.len, revs, commits_trans, commits, authors5_trans, authors5, )

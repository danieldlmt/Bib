library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(tidyr)
library(tseries)
################################
# Carregamento do log de commits
# Escolha apenas um log
# 
###############################


# Create variable 'log'  that contains the paths of the scripts and workspaces (.RData)
  source("./paths.R")

for (it in 1:length(logs$sistema)){
  rm(list = ls()[!ls() %in% c("logs","it")])
  source(as.character(logs$caminho[it]) )
  #load (as.character(logs$data[it]))
  
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
  select(rev, platform,author, date,n_line_add,n_line_del,path) %>%
  group_by(rev) %>%
  arrange(platform)%>%
  summarise( n_plat = n_distinct(platform), platform = paste(unique(platform), collapse=", "), author=unique(author), date= unique(date),n_line_add=max(n_line_add),n_line_del=max(n_line_del)) %>%
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

###########################################################3
##########################################################

######################################
# numero de plataformas modificadas em cada commit
platform <- platform3 %>%
  select(rev, platform, author,n_line_add,n_line_del)%>%
  group_by(rev) %>%
  summarise(n_plat = n_distinct(platform), platform = paste(unique(platform), collapse=", "), author=unique(author), n_line_add=max(n_line_add),n_line_del=max(n_line_del)) %>%
  arrange(rev)

# lista de arquivos modificados em cada commit
file_list <- platform3 %>%
  select(rev, path) %>%
  group_by(rev) %>%
  arrange(path)%>%
  summarise( n_files = n_distinct(path), path = paste(unique(path), collapse=", "))

# upgrade em platform, + numero de arquivos modificados em cada commit
platform <- merge(platform,file_list,by="rev")
#numero de modificacoes que cada desenvolvedor fez em cada plataforma
independente <- platform %>% filter(str_detect(platform, "Independente")) %>% select(author) %>% group_by(author)%>% summarise(independente=n())
android <- platform %>% filter(str_detect(platform, "Android")) %>% select(author) %>% group_by(author)%>% summarise(android=n())
linux <- platform %>% filter(str_detect(platform, "Linux"))%>% select(author) %>% group_by(author)%>% summarise(linux=n())
win <- platform %>% filter(str_detect(platform, "Windows"))%>% select(author) %>% group_by(author)%>% summarise(win=n())
iphone <- platform %>% filter(str_detect(platform, "iPhone"))%>% select(author) %>% group_by(author)%>% summarise(iphone=n())
macosx <- platform %>% filter(str_detect(platform, "macOS"))   %>% select(author) %>% group_by(author)%>% summarise(macosx=n())
# numero de plataformas suportadas por cada desenvolvedor
authors3 <- platform3 %>% select(author) %>% distinct(author) %>% mutate(id=as.numeric(author))
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


#variaveis bind
saida_plat_bind <- data.frame(iteracao=NA,author=NA,prob=NA, ind=NA,esp=NA, gen=NA, mobile=NA, desktop=NA, indd=NA, tipo=NA,ndev=NA)
saida_device_bind <- data.frame(iteracao=NA,author=NA,prob=NA, ind=NA,esp=NA, gen=NA, mobile=NA, desktop=NA, indd=NA, tipo=NA,ndev=NA)

dev_all_time24<-data.frame(tipo=NA,n=NA, porcentage=NA,iteracao=NA, date_left=NA, date_right=NA)
dev_gen_time24<-data.frame(n_platform=NA,n=NA, porcentage=NA,iteracao=NA, date_left=NA, date_right=NA)


devicetype_time24 <- data.frame(developer=NA, dispositivo="")
devicetype_time24 <- devicetype_time24 %>% mutate(porcentage= (developer/sum(developer))*100, tipo = "todos", iteracao = NA, date_left = as.POSIXct( date_left), date_right = as.POSIXct( date_right ))

janela<-c(-12,-24,-36, -48) 

#############################3
# Quantidade de desenvolvedores nas janelas de tempo
###########################
qtd_dev24 <- data_frame(iteracao=rep(0,as.integer(nperiods)),n_dev=rep(0,as.integer(nperiods)))


    ####################3
    ###################3
    # Janela 24 semanas
    for (j in 1:as.integer(nperiods)){
      
      

      
      
      platform3_aux <- platform3%>%
        filter(as.POSIXct(date)>=date_left  & as.POSIXct(date)<date_right )  
      
      if (as.integer( length(platform3_aux$rev))){
        ###################################
        # qtd de devs
        ###################################
        qtd_dev24$iteracao[j] <- j
        qtd_dev24$n_dev[j]  <- n_distinct(platform3_aux$author)
     
        
        #################################
        #inicio qp1
        ##############################3333
        #numero de modificacoes que cada desenvolvedor fez em cada plataforma
        independente3 <- platform3_aux %>% filter(str_detect(platform, "Independente"))  %>% select(author) %>% group_by(author)%>% summarise(independente=n())
        android3 <- platform3_aux %>% filter(str_detect(platform, "Android")) %>% select(author) %>% group_by(author)%>% summarise(android=n())
        linux3 <- platform3_aux %>% filter(str_detect(platform, "Linux")) %>% select(author) %>% group_by(author)%>% summarise(linux=n())
        win3 <- platform3_aux %>% filter(str_detect(platform, "Windows")) %>% select(author) %>% group_by(author)%>% summarise(win=n())
        iphone3 <- platform3_aux %>% filter(str_detect(platform, "iPhone")) %>% select(author) %>% group_by(author)%>% summarise(iphone=n())
        macosx3 <- platform3_aux %>% filter(str_detect(platform, "macOS"))  %>% select(author) %>% group_by(author)%>% summarise(macosx=n())
        # numero de plataformas suportadas por cada desenvolvedor
        authors3 <- data %>% select(author) %>% distinct(author) %>% mutate(id=as.numeric(author))
        authors3 <-merge(authors3,independente3,by="author", all.x = TRUE)
        authors3 <-merge(authors3,android3,by="author", all.x = TRUE)
        authors3 <-merge(authors3,linux3,by="author", all.x = TRUE)
        authors3 <-merge(authors3,win3,by="author", all.x = TRUE)
        authors3 <-merge(authors3,iphone3,by="author", all.x = TRUE)
        authors3 <-merge(authors3,macosx3,by="author", all.x = TRUE)
        
        authors3 <- authors3 %>% rowwise() %>%  mutate(n_platform=5-(is.na(android)+is.na(linux)+is.na(win)+is.na(iphone)+is.na(macosx)), n_commit= sum(independente,android,linux,win,iphone,macosx,NA, na.rm = TRUE)  )
        
        
        
        ## Filtrar autores que nao trabalharam com plataformas e src
        authors3 <- authors3 %>% filter( !(is.na(independente) & n_platform == 0))
        #authors3 <- authors3 %>% filter( n_platform > 0 )
        
        ##adiciona coluna desenvolvedor especialista e generalista ()
        authors3 <- authors3 %>% mutate( tipo = if_else( n_platform > 1 ,  "gen" , if_else( n_platform == 1 , "esp", NA_character_)) )
        
        #porcentagem do numero de plataforma os desenvolvedores trabalham
        
        
        # distribuição de desenvolvedores especialistas e generalistas
        devall_aux <- authors3%>% 
          select(tipo) %>%  
          group_by(tipo) %>% 
          summarise(n=n())%>% 
          mutate(porcentage = round(n/sum(n) * 100, 1), iteracao = j, date_left = as.POSIXct( date_left), date_right = as.POSIXct( date_right )) 
        
        dev_gen_aux <- authors3%>% 
          filter (tipo=="gen") %>%
          select(n_platform) %>% 
          group_by(n_platform)%>%
          summarise(n=n())%>%
          mutate(porcentage = round(n/sum(n) * 100, 1), iteracao = j, date_left = as.POSIXct( date_left), date_right = as.POSIXct( date_right )) 
        
        
        
        dev_all_time24 <- rbind(dev_all_time24, devall_aux)
        dev_gen_time24<- rbind(dev_gen_time24, dev_gen_aux)
        
        
        
        
       

        
        ###################################
        #fim qp1
        #####################################
        
        
        #############################
        #inicio qp2
        #############################
        
        authors41 <- authors3 %>% select(android,linux,win, iphone,macosx, n_commit ) 
        authors41[colnames(authors41)][is.na(authors41[colnames(authors41)])] <- 0
        
        authors41 <- authors3 %>% select(android,linux,win, iphone,macosx, n_commit ) 
        
        authors42 <- authors3 %>% 
          arrange(desc(n_commit)) %>%
          head(ceiling(nrow(authors3)*0.2)) %>% 
          select(android,linux,win, iphone,macosx, n_commit ) 
        
        authors43 <- authors3 %>% 
          arrange(desc(n_commit)) %>%
          head(-ceiling(nrow(authors3)*0.2)) %>% 
          select(android,linux,win, iphone,macosx, n_commit ) 
        
        authors41[colnames(authors41)][is.na(authors41[colnames(authors41)])] <- 0
        authors42[colnames(authors42)][is.na(authors42[colnames(authors42)])] <- 0
        authors43[colnames(authors43)][is.na(authors43[colnames(authors43)])] <- 0
        
        authors6<- authors41 %>% mutate(Desktop=linux+win+macosx, Mobile=android+iphone)%>% select (Desktop,Mobile)
        authors62<- authors42 %>% mutate(Desktop=linux+win+macosx, Mobile=android+iphone)%>% select (Desktop,Mobile)
        authors63<- authors43 %>% mutate(Desktop=linux+win+macosx, Mobile=android+iphone)%>% select (Desktop,Mobile)
        
        
        
        #geral
        devicetype_aux <- data.frame(developer=c(
          filter(authors6, Desktop==0 &Mobile==0) %>% nrow(),
          filter(authors6, Desktop>0 &Mobile==0) %>% nrow(),
          filter(authors6, Desktop==0 &Mobile>0) %>% nrow(),
          filter(authors6, Desktop>0 &Mobile>0) %>% nrow() ), 
          dispositivo=c("None", "Desktop", "Mobile", "Both"))
        devicetype_aux <- devicetype_aux %>% mutate(porcentage= (developer/sum(developer))*100, tipo = "todos", iteracao = j, date_left = as.POSIXct( date_left), date_right = as.POSIXct( date_right ))
        
        devicetype_time24 <- rbind(devicetype_time24, devicetype_aux)
        
     
        
        ################################
        # fim qp2
        ##################################
        
      }

   
      
      
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
      platform_new_bind<-platform_new_bind %>% 
        mutate(ndev=n_dev)
      # saida_plat_bind e saida_device_bind sao as variaveis de saida  
      saida_plat <- platform_new_bind%>%
        mutate(iteracao=j,
               tipo = if_else(esp==1&gen==0&ind==0,"esp",
                              if_else(esp==0&gen==1&ind==0,"gen",
                                      if_else(esp==0&gen==0&ind==1,"ind",
                                              if_else(esp==1&gen==0&ind==1,"ind esp",
                                                      if_else(esp==0&gen==1&ind==1,"ind gen",
                                                              if_else(esp==1&gen==1&ind==1,"ind esp gen",
                                                                      if_else(esp==1&gen==1&ind==0,"esp gen","nada") )))))))%>%
        select(iteracao,author,prob,ind,esp,gen,mobile,desktop,indd,tipo,ndev)
      #%>%
      # group_by(prob,tipo)%>%     
      #summarise(n = n())
      saida_plat_bind<-bind_rows(saida_plat_bind,saida_plat)
      
      saida_device <- platform_new_bind%>%
        mutate(iteracao = j,
               tipo = if_else(desktop==1&mobile==0&indd==0,"desktop",
                              if_else(desktop==0&mobile==1&indd==0,"mobile",
                                      if_else(desktop==0&mobile==0&indd==1,"ind",
                                              if_else(desktop==1&mobile==0&indd==1,"ind desktop",
                                                      if_else(desktop==0&mobile==1&indd==1,"ind mobile",
                                                              if_else(desktop==1&mobile==1&indd==1,"ind desktop mobile",
                                                                      if_else(desktop==1&mobile==1&indd==0,"desktop mobile","nada") )))))))%>%
        select(iteracao,author,prob,ind,esp,gen,mobile,desktop,indd,tipo,ndev)
      
      saida_device_bind<-bind_rows(saida_device_bind,saida_device)
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













#######################################3

# analise de devs 
#gen
#esp e 
#esp gen  

xa<-saida_plat_bind %>%
  mutate( tipo = if_else( esp==1 & gen==0,"esp",
                          if_else( esp==0 & gen==1, "gen",
                                   if_else( esp==1 & gen==1 & ind==1, "esp gen", tipo ))))%>%
  filter(tipo!="ind") %>%
  group_by( iteracao, prob, tipo ) %>%
  summarise(n = n(),ndev=n_dev) %>%
  mutate(porc = n*100/ndev) %>%
  select (iteracao,prob, tipo, n, ndev, porc)


esp_bind <- xa %>% filter(tipo=="esp")

gen_bind <- xa %>% filter(tipo=="gen")

espgen_bind <-  xa %>% filter(tipo=="esp gen")


xb<-saida_device_bind %>%
  mutate( tipo = if_else( mobile==1 & desktop==0,"mobile",
                          if_else( mobile==0 & desktop==1, "desktop",
                                   if_else( mobile==1 & desktop==1 & indd==1, "mobile desktop", tipo ))))%>%
  filter(tipo!="indd")%>%
  group_by( iteracao, prob, tipo ) %>%
  summarise(n = n(),ndev=n_dev)%>%
  mutate(porc = n*100/ndev)%>%
  select (iteracao,prob, tipo, n, ndev, porc)


mobile_bind <- xb %>% filter(tipo=="mobile")

desktop_bind <- xb %>% filter(tipo=="desktop")

desktopmobile_bind <-  xb %>% filter(tipo=="mobile desktop")



# analise de devs 
#gen
#esp  



xc<-saida_plat_bind %>%
  mutate( tipo = if_else( esp==1 & gen==0,"esp",
                          if_else( esp==0 & gen==1, "gen",
                                   if_else( esp==1 & gen==1, "gen", tipo ))))%>%
  filter(tipo!="ind") %>%
  group_by( iteracao, prob, tipo ) %>%
  summarise(n = n(),ndev=n_dev) %>%
  mutate(porc = n*100/ndev) %>%
  select (iteracao,prob, tipo, n, ndev, porc)


esp_bind2 <- xc %>% filter(tipo=="esp")

gen_bind2 <- xc %>% filter(tipo=="gen")





























    
    # evoluçao dos desenvolvedores generalistas ao longo do tempo
    #dev_all_time2 <- dev_all_time %>% mutate ( generalista = n_platform>=2 ) %>% group_by( generalista, iteracao ) %>% summarise(soma_porc = sum(porcentage), soma_n = sum(n)) %>% filter( generalista == TRUE )
    dev_all_time24_2 <- dev_all_time24 %>% filter( tipo == "gen" )

    dev_gen_time24_2 <- dev_gen_time24 %>% select(n_platform,iteracao,porcentage,n) %>% filter(n_platform==2)
    dev_gen_time24_3 <- dev_gen_time24 %>% select(n_platform,iteracao,porcentage,n) %>% filter(n_platform==3)
    dev_gen_time24_4 <- dev_gen_time24 %>% select(n_platform,iteracao,porcentage,n) %>% filter(n_platform==4)
    dev_gen_time24_5 <- dev_gen_time24 %>% select(n_platform,iteracao,porcentage,n) %>% filter(n_platform==5)

    dev_gen_med24 <- dev_gen_time24 %>% select(n_platform,iteracao,n) %>% group_by(iteracao) %>% summarise(n_platform=mean(n_platform), n=sum(n) )

    
        
    # evolucao dos desenvolvedores que trabalham com dispositivos desktop e mobile ao longo do tempo
    devicetype_time24_2 <- devicetype_time24 %>%  filter( dispositivo == "Both" ) 

    commits <- as.data.frame.matrix(xtabs(~ rev + platform, data=data[c('platform', 'rev')]))
    commits <- commits %>% select (Android, Linux, Windows, iPhone, macOS, Independente)  %>%  filter (  !(rowSums(commits) == 0) )
    
    save.image(as.character(logs$ws_analisetemporaljanela[it]))
}
###############################################3
###########################################
###################################
# fim analise temporal com janela de tempo
###############################################
##############################################
###############################################

library(dplyr, warn.conflicts = FALSE)
library(arules)
library(stringr)
library(tidyr)
library(tseries)
################################
# Carregamento do log de commits
# Escolha apenas um log
# 
###############################
logs <- data.frame(sistema=c("Allegro", "SDL","SFML","Coco2dx"), caminho = c('~/bib/log_allegro.R', '~/bib/log_SDL.R', '~/bib/log_SFML.R', '~/bib/log_coco2dx.R'),ws_analise=c('~/bib/analise_allegro.RData', '~/bib/analise_SDL.RData', '~/bib/analise_SFML.RData', '~/bib/analise_coco2dx.RData'), ws_analisetemporal = c('~/bib/analisetemporal_allegro.RData', '~/bib/analisetemporal_SDL.RData', '~/bib/analisetemporal_SFML.RData', '~/bib/analisetemporal_coco2dx.RData'), ws_analisetemporaljanela = c('~/bib/analisetemporaljanela_allegro.RData', '~/bib/analisetemporaljanela_SDL.RData', '~/bib/analisetemporaljanela_SFML.RData', '~/bib/analisetemporaljanela_coco2dx.RData')  )
for (it in 1:length(logs$sistema)){
  rm(list = ls()[!ls() %in% c("logs","it")])
  source(as.character(logs$caminho[it]) )

########################################
########################################
########################################
#inicio da analise temporal com janela deslizante
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



dev_all_time12<-data.frame(tipo=NA,n=NA, porcentage=NA,iteracao=NA, date_left=NA, date_right=NA)
dev_gen_time12<-data.frame(n_platform=NA,n=NA, porcentage=NA,iteracao=NA, date_left=NA, date_right=NA)

dev_all_time24<-data.frame(tipo=NA,n=NA, porcentage=NA,iteracao=NA, date_left=NA, date_right=NA)
dev_gen_time24<-data.frame(n_platform=NA,n=NA, porcentage=NA,iteracao=NA, date_left=NA, date_right=NA)

dev_all_time36<-data.frame(tipo=NA,n=NA, porcentage=NA,iteracao=NA, date_left=NA, date_right=NA)
dev_gen_time36<-data.frame(n_platform=NA,n=NA, porcentage=NA,iteracao=NA, date_left=NA, date_right=NA)

dev_all_time48<-data.frame(tipo=NA,n=NA, porcentage=NA,iteracao=NA, date_left=NA, date_right=NA)
dev_gen_time48<-data.frame(n_platform=NA,n=NA, porcentage=NA,iteracao=NA, date_left=NA, date_right=NA)

#geral
devicetype_time12 <- data.frame(developer=NA, dispositivo="")
devicetype_time12 <- devicetype_time12 %>% mutate(porcentage= (developer/sum(developer))*100, tipo = "todos", iteracao = NA, date_left = as.POSIXct( date_left), date_right = as.POSIXct( date_right ))

devicetype_time24 <- data.frame(developer=NA, dispositivo="")
devicetype_time24 <- devicetype_time24 %>% mutate(porcentage= (developer/sum(developer))*100, tipo = "todos", iteracao = NA, date_left = as.POSIXct( date_left), date_right = as.POSIXct( date_right ))

devicetype_time36 <- data.frame(developer=NA, dispositivo="")
devicetype_time36 <- devicetype_time36 %>% mutate(porcentage= (developer/sum(developer))*100, tipo = "todos", iteracao = NA, date_left = as.POSIXct( date_left), date_right = as.POSIXct( date_right ))

devicetype_time48 <- data.frame(developer=NA, dispositivo="")
devicetype_time48 <- devicetype_time48 %>% mutate(porcentage= (developer/sum(developer))*100, tipo = "todos", iteracao = NA, date_left = as.POSIXct( date_left), date_right = as.POSIXct( date_right ))

janela<-c(-12,-24,-36, -48) 

#############################3
# Quantidade de desenvolvedores nas janelas de tempo
###########################
qtd_dev12 <- data_frame(iteracao=rep(0,as.integer(nperiods)),n_dev=rep(0,as.integer(nperiods)))
qtd_dev24 <- data_frame(iteracao=rep(0,as.integer(nperiods)),n_dev=rep(0,as.integer(nperiods)))
qtd_dev36 <- data_frame(iteracao=rep(0,as.integer(nperiods)),n_dev=rep(0,as.integer(nperiods)))
qtd_dev48 <- data_frame(iteracao=rep(0,as.integer(nperiods)),n_dev=rep(0,as.integer(nperiods)))

###########################
# Rastrear desenvolvedores desde o inicio do periodo de analise
###########################
date_right2<- seq.POSIXt( date_left, length=2, by='24 weeks')[2] 
platform3_aux <- platform3%>%
  filter(as.POSIXct(date)>=date_left  & as.POSIXct(date)<date_right2 )  
lista_dev <- platform3_aux %>% select(author) %>% unique()%>% arrange(author)
devs_inicio12 <- data.frame(row.names = lista_dev$author) 
devs_inicio24 <- data.frame(row.names = lista_dev$author) 
devs_inicio36 <- data.frame(row.names = lista_dev$author) 
devs_inicio48 <- data.frame(row.names = lista_dev$author) 
devs_inicio_list12 <- data.frame() 
devs_inicio_list24 <- data.frame() 
devs_inicio_list36 <- data.frame() 
devs_inicio_list48 <- data.frame() 
#colnames(devs_inicio) <- lista_dev$author
list_plats <- c("Windows","iPhone","Linux","macOS","Android")
####################3
###################3
# Janela 12 semanas
    for (j in 1:as.integer(nperiods)){
      platform3_aux <- platform3%>%
        filter(as.POSIXct(date)>=date_left  & as.POSIXct(date)<date_right )  
      
      if (as.integer( length(platform3_aux$rev))){
        ###################################
        # qtd de devs
        ###################################
        qtd_dev12$iteracao[j] <- j
        qtd_dev12$n_dev[j]  <- n_distinct(platform3_aux$author)
        
        #################################
        #rastrear desenvovedores (contagem)
        #################################
          # Pega os commits feitos pelos desenvolvedores iniciais na janela de tempo
        devs_inicio_aux<-platform3_aux %>% select(platform, author)%>% group_by(author) %>% summarise(commit = n(), platlist = str_replace_all(paste(unique(platform), collapse=" "),"(,)","")   )
          # Eliminar plataformas  duplicadas na coluna plat
        devs_inicio_aux$platlist <- vapply(lapply(strsplit(devs_inicio_aux$platlist, " "), unique), paste, character(1L), collapse = " ")
          # Conta o número de plataformas modificadas pelo desenvolvedor inicial na janela de tempo        
        devs_inicio_aux <- devs_inicio_aux %>%rowwise() %>%mutate (plat = if_else(platlist=="",0,as.numeric(sum(str_count(platlist,pattern = list_plats)) ) ) )
        devs_inicio_aux<-devs_inicio_aux %>% select(author,plat)
          # atribui nome das linhas e elimina a coluna author
        rownames(devs_inicio_aux) <- devs_inicio_aux$author
        devs_inicio_aux$author <- NULL
        #rownames(devs_inicio_list_aux) <- devs_inicio_list_aux$author
        #devs_inicio_list_aux$author <- NULL
        
        
   
        
          # passa os dados para um dataframe de mesmo tamanho devs_inicio
        devs_inicio_aux2 <- data.frame(row.names = lista_dev$author)
        devs_inicio_aux2<- merge(devs_inicio_aux,devs_inicio_aux2,by="row.names", all.y=TRUE)
        rownames(devs_inicio_aux2) <- devs_inicio_aux2$Row.names
        devs_inicio_aux2$Row.names <- NULL

          # Acumula o resultado dos desenvolvedores iniciais (contagem de plataformas)
        colnames(devs_inicio_aux2)<-j
        devs_inicio12 <- bind_cols(devs_inicio_aux2,devs_inicio12)
        rownames(devs_inicio12) <- lista_dev$author
        
        #################################
        #rastrear desenvovedores (lista de plataformas)
        #################################
        # Pega os commits feitos pelos desenvolvedores iniciais na janela de tempo
        devs_inicio_aux<-platform3_aux %>% select(platform, author)%>% group_by(author) %>% summarise(commit = n(), platlist = str_replace_all(paste(unique(platform), collapse=" "),"(,)","")   )
        # Eliminar plataformas  duplicadas na coluna plat
        devs_inicio_aux$platlist <- vapply(lapply(strsplit(devs_inicio_aux$platlist, " "), unique), paste, character(1L), collapse = " ")
        # variavel criada para controlar as plataformas modificadas pelos desenvolvedores iniciais
        devs_inicio_list_aux <- devs_inicio_aux %>% select(author,platlist) %>% mutate(iteracao=j, android=str_count(platlist, pattern = "Android") ,linux=str_count(platlist, pattern = "Linux"),windows=str_count(platlist, pattern = "Windows"),macOS=str_count(platlist, pattern = "macOS"),iphone=str_count(platlist, pattern = "iPhone"))
        
        devs_inicio_list_aux <-devs_inicio_list_aux %>% select(iteracao,author,android,iphone,linux,windows,macOS) 
        devs_inicio_list12 <- rbind(devs_inicio_list12, devs_inicio_list_aux)
        
        #################################
        #inicio qp1
        ##############################3333
        independente3 <- platform3_aux %>% filter(str_detect(platform, "Independente"))  %>% select(author) %>% group_by(author)%>% summarise(independente=n())
        android3 <- platform3_aux %>% filter(str_detect(platform, "Android")) %>% select(author) %>% group_by(author)%>% summarise(android=n())
        linux3 <- platform3_aux %>% filter(str_detect(platform, "Linux")) %>% select(author) %>% group_by(author)%>% summarise(linux=n())
        win3 <- platform3_aux %>% filter(str_detect(platform, "Windows")) %>% select(author) %>% group_by(author)%>% summarise(win=n())
        iphone3 <- platform3_aux %>% filter(str_detect(platform, "iPhone")) %>% select(author) %>% group_by(author)%>% summarise(iphone=n())
        macosx3 <- platform3_aux %>% filter(str_detect(platform, "macOS"))  %>% select(author) %>% group_by(author)%>% summarise(macosx=n())
        
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
    
        
        
        dev_all_time12 <- rbind(dev_all_time12, devall_aux)
        dev_gen_time12 <- rbind(dev_gen_time12, dev_gen_aux)
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
    
        devicetype_time12 <- rbind(devicetype_time12, devicetype_aux)
    
        
        ################################
        # fim qp2
        ##################################
 
      }

      #date_left <- date_right
      date_right <- seq.POSIXt( date_right, length = 2, by = '4 weeks' ) [ 2 ] 
      
    # Janela de tempo deslizante
      if (as.integer(difftime(date_right,date_left,units = "weeks"))>12)
        date_left<- seq.POSIXt( date_right, length = 2, by = '-12 weeks' ) [ 2 ] 
      
    }
    
    # evoluçao dos desenvolvedores generalistas ao longo do tempo
    #dev_all_time2 <- dev_all_time %>% mutate ( generalista = n_platform>=2 ) %>% group_by( generalista, iteracao ) %>% summarise(soma_porc = sum(porcentage), soma_n = sum(n)) %>% filter( generalista == TRUE )
    dev_all_time12_2 <- dev_all_time12 %>% filter( tipo == "gen" )
    
    dev_gen_time12_2 <- dev_gen_time12 %>% select(n_platform,iteracao,porcentage,n) %>% filter(n_platform==2)
    dev_gen_time12_3 <- dev_gen_time12 %>% select(n_platform,iteracao,porcentage,n) %>% filter(n_platform==3)
    dev_gen_time12_4 <- dev_gen_time12 %>% select(n_platform,iteracao,porcentage,n) %>% filter(n_platform==4)
    dev_gen_time12_5 <- dev_gen_time12 %>% select(n_platform,iteracao,porcentage,n) %>% filter(n_platform==5)
    
    # evolucao dos desenvolvedores que trabalham com dispositivos desktop e mobile ao longo do tempo
    devicetype_time12_2 <- devicetype_time12 %>%  filter( dispositivo == "Both" )


    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    date_left<- min(as.POSIXct(platform3$date))
    date_right<- seq.POSIXt( date_left, length=2, by='4 weeks')[2]   
    
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
        #rastrear desenvovedores
        #################################
        # Pega os commits feitos pelos desenvolvedores iniciais na janela de tempo
        devs_inicio_aux<-platform3_aux %>% select(platform, author)%>% group_by(author) %>% summarise(commit = n(), platlist = str_replace_all(paste(unique(platform), collapse=" "),"(,)|(Independente)|(Indeterminada)","")   )
        # Eliminar plataformas  duplicadas na coluna plat
        devs_inicio_aux$platlist <- vapply(lapply(strsplit(devs_inicio_aux$platlist, " "), unique), paste, character(1L), collapse = " ")
        # Conta o número de plataformas modificadas pelo desenvolvedor inicial na janela de tempo        
        devs_inicio_aux <- devs_inicio_aux %>%rowwise() %>%mutate (plat = if_else(platlist=="",0,as.numeric(sum(str_count(platlist,pattern = list_plats)) ) ) )
        devs_inicio_aux<-devs_inicio_aux %>% select(author,plat)
        # atribui nome das linhas e elimina a coluna author
        rownames(devs_inicio_aux) <- devs_inicio_aux$author
        devs_inicio_aux$author <- NULL
        
        # passa os dados para um dataframe de mesmo tamanho devs_inicio
        devs_inicio_aux2 <- data.frame(row.names = lista_dev$author)
        devs_inicio_aux2<- merge(devs_inicio_aux,devs_inicio_aux2,by="row.names", all.y=TRUE)
        rownames(devs_inicio_aux2) <- devs_inicio_aux2$Row.names
        devs_inicio_aux2$Row.names <- NULL
        
        # Acumula o resultado dos desenvolvedores iniciais 
        colnames(devs_inicio_aux2)<-j
        devs_inicio24 <- bind_cols(devs_inicio_aux2,devs_inicio24)
        rownames(devs_inicio24) <- lista_dev$author
        
        #################################
        #rastrear desenvovedores (lista de plataformas)
        #################################
        # Pega os commits feitos pelos desenvolvedores iniciais na janela de tempo
        devs_inicio_aux<-platform3_aux %>% select(platform, author)%>% group_by(author) %>% summarise(commit = n(), platlist = str_replace_all(paste(unique(platform), collapse=" "),"(,)","")   )
        # Eliminar plataformas  duplicadas na coluna plat
        devs_inicio_aux$platlist <- vapply(lapply(strsplit(devs_inicio_aux$platlist, " "), unique), paste, character(1L), collapse = " ")
        # variavel criada para controlar as plataformas modificadas pelos desenvolvedores iniciais
        devs_inicio_list_aux <- devs_inicio_aux %>% select(author,platlist) %>% mutate(iteracao=j, android=str_count(platlist, pattern = "Android") ,linux=str_count(platlist, pattern = "Linux"),windows=str_count(platlist, pattern = "Windows"),macOS=str_count(platlist, pattern = "macOS"),iphone=str_count(platlist, pattern = "iPhone"))
        
        devs_inicio_list_aux <-devs_inicio_list_aux %>% select(iteracao,author,android,iphone,linux,windows,macOS) 
        devs_inicio_list24 <- rbind(devs_inicio_list24, devs_inicio_list_aux)
        
        #################################
        #inicio qp1
        ##############################3333
        independente3 <- platform3_aux %>% filter(str_detect(platform, "Independente"))  %>% select(author) %>% group_by(author)%>% summarise(independente=n())
        android3 <- platform3_aux %>% filter(str_detect(platform, "Android")) %>% select(author) %>% group_by(author)%>% summarise(android=n())
        linux3 <- platform3_aux %>% filter(str_detect(platform, "Linux")) %>% select(author) %>% group_by(author)%>% summarise(linux=n())
        win3 <- platform3_aux %>% filter(str_detect(platform, "Windows")) %>% select(author) %>% group_by(author)%>% summarise(win=n())
        iphone3 <- platform3_aux %>% filter(str_detect(platform, "iPhone")) %>% select(author) %>% group_by(author)%>% summarise(iphone=n())
        macosx3 <- platform3_aux %>% filter(str_detect(platform, "macOS"))  %>% select(author) %>% group_by(author)%>% summarise(macosx=n())
        
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
      
      #date_left <- date_right
      date_right <- seq.POSIXt( date_right, length = 2, by = '4 weeks' ) [ 2 ] 
      
      # Janela de tempo deslizante
      if (as.integer(difftime(date_right,date_left,units = "weeks"))>24)
        date_left<- seq.POSIXt( date_right, length = 2, by = '-24 weeks' ) [ 2 ] 
      
    }
    
    # evoluçao dos desenvolvedores generalistas ao longo do tempo
    #dev_all_time2 <- dev_all_time %>% mutate ( generalista = n_platform>=2 ) %>% group_by( generalista, iteracao ) %>% summarise(soma_porc = sum(porcentage), soma_n = sum(n)) %>% filter( generalista == TRUE )
    dev_all_time24_2 <- dev_all_time24 %>% filter( tipo == "gen" )
 
    dev_all_time24_2_n_level <- kpss.test(dev_all_time24_2$n, null =  "Level") 
    dev_all_time24_2_n_trend <- kpss.test(dev_all_time24_2$n, null =  "Trend") 
    
    dev_all_time24_2_porc_level <- kpss.test(dev_all_time24_2$porcentage, null =  "Level") 
    dev_all_time24_2_porc_trend <- kpss.test(dev_all_time24_2$porcentage, null =  "Trend") 
       
    dev_gen_time24_2 <- dev_gen_time24 %>% select(n_platform,iteracao,porcentage,n) %>% filter(n_platform==2)
    dev_gen_time24_3 <- dev_gen_time24 %>% select(n_platform,iteracao,porcentage,n) %>% filter(n_platform==3)
    dev_gen_time24_4 <- dev_gen_time24 %>% select(n_platform,iteracao,porcentage,n) %>% filter(n_platform==4)
    dev_gen_time24_5 <- dev_gen_time24 %>% select(n_platform,iteracao,porcentage,n) %>% filter(n_platform==5)

    dev_gen_med24 <- dev_gen_time24 %>% select(n_platform,iteracao,n) %>% group_by(iteracao) %>% summarise(n_platform=mean(n_platform), n=sum(n) )
    dev_gen_med24_level <- kpss.test(dev_gen_med24$n, null =  "Level") 
    dev_gen_med24_trend <- kpss.test(dev_gen_med24$n, null =  "Trend") 
        
    dev_gen_time24_2_n_level <- kpss.test(dev_gen_time24_2$n, null =  "Level") 
    dev_gen_time24_2_n_trend <- kpss.test(dev_gen_time24_2$n, null =  "Trend") 
    dev_gen_time24_2_porc_level <- kpss.test(dev_gen_time24_2$porcentage, null =  "Level") 
    dev_gen_time24_2_porc_trend <- kpss.test(dev_gen_time24_2$porcentage, null =  "Trend")
    
    dev_gen_time24_3_n_level <- kpss.test(dev_gen_time24_3$n, null =  "Level") 
    dev_gen_time24_3_n_trend <- kpss.test(dev_gen_time24_3$n, null =  "Trend") 
    dev_gen_time24_3_porc_level <- kpss.test(dev_gen_time24_3$porcentage, null =  "Level") 
    dev_gen_time24_3_porc_trend <- kpss.test(dev_gen_time24_3$porcentage, null =  "Trend")
    
    dev_gen_time24_4_n_level <- kpss.test(dev_gen_time24_4$n, null =  "Level") 
    dev_gen_time24_4_n_trend <- kpss.test(dev_gen_time24_4$n, null =  "Trend") 
    dev_gen_time24_4_porc_level <- kpss.test(dev_gen_time24_4$porcentage, null =  "Level") 
    dev_gen_time24_4_porc_trend <- kpss.test(dev_gen_time24_4$porcentage, null =  "Trend")
    
    dev_gen_time24_5_n_level <- kpss.test(dev_gen_time24_5$n, null =  "Level") 
    dev_gen_time24_5_n_trend <- kpss.test(dev_gen_time24_5$n, null =  "Trend") 
    dev_gen_time24_5_porc_level <- kpss.test(dev_gen_time24_5$porcentage, null =  "Level") 
    dev_gen_time24_5_porc_trend <- kpss.test(dev_gen_time24_5$porcentage, null =  "Trend")   
    
        
    # evolucao dos desenvolvedores que trabalham com dispositivos desktop e mobile ao longo do tempo
    devicetype_time24_2 <- devicetype_time24 %>%  filter( dispositivo == "Both" ) 
    
    devicetype_time24_2_n_level <- kpss.test(devicetype_time24_2$developer, null =  "Level") 
    devicetype_time24_2_n_trend <- kpss.test(devicetype_time24_2$developer, null =  "Trend") 
    
    devicetype_time24_2_porc_level <- kpss.test(devicetype_time24_2$porcentage, null =  "Level") 
    devicetype_time24_2_porc_trend <- kpss.test(devicetype_time24_2$porcentage, null =  "Trend") 
    
      #rastreamento de desenvolvedores (contagem de plataformas)
    devs_inicio12<-devs_inicio12[,order(as.numeric( colnames(devs_inicio12) ))]
    devs_inicio24<-devs_inicio24[,order(as.numeric( colnames(devs_inicio24)))]


    devs_inicio12<-as.data.frame(t(devs_inicio12)) 
    devs_inicio24<-as.data.frame(t(devs_inicio24)) 


    #devs_inicio12<-devs_inicio12[,colSums(devs_inicio12,na.rm = T) > 15]
    #devs_inicio24<-devs_inicio24[,colSums(devs_inicio24,na.rm = T) > 15]


      
      # rastreamento de desenvolvedores (lista de plataformas)
   # devs_inicio_list12<-devs_inicio_list12[,order(as.numeric( colnames(devs_inicio_list12) ))]
  #  devs_inicio_list24<-devs_inicio_list24[,order(as.numeric( colnames(devs_inicio_list24)))]
  #  devs_inicio_list36<-devs_inicio_list36[,order(as.numeric( colnames(devs_inicio_list36)))]
  #  devs_inicio_list48<-devs_inicio_list48[,order(as.numeric( colnames(devs_inicio_list48)))]


    
    
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

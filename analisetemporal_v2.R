library(dplyr, warn.conflicts = FALSE)
library(arules)
library(stringr)
#Para instalar tseries tive que isntalar: 
#sudo apt-get install libcurl4-openssl-dev
library(tseries)
################################
# Carregamento do log de commits
# Escolha apenas um log
# 
###############################
logs <- data.frame(sistema=c("Allegro", "SDL"), caminho = c('~/bib/log_allegro.R', '~/bib/log_SDL.R'),ws_analise=c('~/bib/analise_allegro.RData', '~/bib/analise_SDL.RData'), ws_analisetemporal = c('~/bib/analisetemporal_allegro.RData', '~/bib/analisetemporal_SDL.RData'), ws_analisetemporaljanela = c('~/bib/analisetemporaljanela_allegro.RData', '~/bib/analisetemporaljanela_SDL.RData')  )
for (it in 1:length(logs$sistema)){
  rm(list = ls()[!ls() %in% c("logs","it")])
  source(as.character(logs$caminho[it]) )



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

#authors3 <- data %>% select(author) %>% distinct(author) %>% mutate(id=as.numeric(author))
#dev_all_time<-authors3%>%
#  filter(n_platform==300) %>%
#  select(n_platform) %>%  
#  group_by(n_platform) %>% 
#  summarise(n=n())%>%
#  mutate(porcentage = round(n/sum(n) * 100, 1), tipo = "todos", iteracao = NA_integer_, date_left = "", date_right = "")
#dev_core_time<-authors3%>%
#  filter(n_platform==300) %>%
#  select(n_platform) %>%  
#  group_by(n_platform) %>% 
#  summarise(n=n())%>%
#  mutate(porcentage = round(n/sum(n) * 100, 1), tipo = "nucleo", iteracao = NA_integer_, date_left = "", date_right = "")

#dev_peri_time<-authors3%>%
#  filter(n_platform==300) %>%
#  select(n_platform) %>%  
#  group_by(n_platform) %>% 
#  summarise(n=n())%>%
#  mutate(porcentage = round(n/sum(n) * 100, 1), tipo = "periferico", iteracao = NA_integer_, date_left = "", date_right = "")


dev_all_time<-data.frame(tipo=NA,n=NA, porcentage=NA,iteracao=NA, date_left=NA, date_right=NA)
dev_gen_time<-data.frame(n_platform=NA,n=NA, porcentage=NA,iteracao=NA, date_left=NA, date_right=NA)


#geral
devicetype_time <- data.frame(developer=NA, dispositivo="")
devicetype_time <- devicetype_time %>% mutate(porcentage= (developer/sum(developer))*100, tipo = "todos", iteracao = NA, date_left = as.POSIXct( date_left), date_right = as.POSIXct( date_right ))

#rules_dev_time <- data.frame(lhs=NA,o=NA,rhs=NA, support=NA, confidence=NA, lift=NA, iteracao=NA, date_left = as.POSIXct( date_left), date_right = as.POSIXct( date_right )) 

#rules_commit_time <- data.frame(lhs=NA,o=NA,rhs=NA, support=NA, confidence=NA, lift=NA, iteracao=NA, date_left = as.POSIXct( date_left), date_right = as.POSIXct( date_right )) 

qtd_dev <- data_frame(iteracao=rep(0,as.integer(nperiods)),n_dev=rep(0,as.integer(nperiods)))

for (j in 1:as.integer(nperiods)){
  platform3_aux <- platform3%>%
    filter(as.POSIXct(date)>=date_left  & as.POSIXct(date)<date_right )  
  
  if (as.integer( length(platform3_aux$rev))){
    
    ###############################
    #Contagem desenvolvedores
    ###################################
    qtd_dev$iteracao[j] <- j
    qtd_dev$n_dev[j]  <- n_distinct(platform3_aux$author)
    
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

    
    
    dev_all_time <- rbind(dev_all_time, devall_aux)
    dev_gen_time <- rbind(dev_gen_time, dev_gen_aux)
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

    devicetype_time <- rbind(devicetype_time, devicetype_aux)

    
    ################################
    # fim qp2
    ##################################
    
    
    
    ##########################################
    #inicio qp3
    #############################################
    
    
    #Regra de associação entre  desenvolvedor e plataforma 
   # authors4 <- authors3 %>% select(android,linux,win, iphone,macosx,independente) 
  #  authors4[colnames(authors4)][is.na(authors4[colnames(authors4)])] <- 0
    
   # authors5 <- ifelse(authors4 > 0, 1, 0)
  #  rules_dev_aux  <- apriori(authors5,parameter=list(support=0.007,confidence=0.8,minlen=1,maxlen=10))
    
   # if ( as.integer( length(rules_dev_aux) )  ){
  #    rules_dev_aux <- as.data.frame(inspect(sort (rules_dev_aux, by = "support")))
  #    colnames(rules_dev_aux) <- c("lhs","o","rhs", "support", "confidence", "lift")
  #    rules_dev_aux <- rules_dev_aux %>% mutate(iteracao = j, date_left = as.POSIXct( date_left), date_right = as.POSIXct( date_right ))
      
   #   rules_dev_time <- rbind(rules_dev_time,rules_dev_aux) 
  #  }
    
    ########################################
    #fim qp3
    ########################################
    
    
    ########################################
    #inicio qp4
    #############################################
    
 #   data_aux <- data%>%
 #  filter(as.POSIXct(date)>=date_left  & as.POSIXct(date)<date_right )  
 # commits <- as.data.frame.matrix(xtabs(~ rev + platform, data=data_aux[c('platform', 'rev')]))
    
#  commits <- commits %>% filter (  !(rowSums(commits) == 0) )
    #commits <- commits %>% select(Android, Linux,Windows,iPhone,macOS, Independente)

#    commits<- ifelse(commits>0, 1, 0)
    
 
 #   rules_commit_aux <- apriori(commits,parameter=list(support=0.007,confidence=0.8,minlen=1,maxlen=10))
    
  #  if ( as.integer( length(rules_commit_aux) )  ){
      
   #   rules_commit_aux <- as.data.frame(inspect(sort(rules_commit_aux, by = "support")))
 
    #  colnames(rules_commit_aux) <- c("lhs","o","rhs", "support", "confidence", "lift")
    #  rules_commit_aux <- rules_commit_aux %>% mutate(iteracao = j, date_left = as.POSIXct( date_left), date_right = as.POSIXct( date_right ))
      
    #  rules_commit_time <- rbind(rules_commit_time,rules_commit_aux) 
      
   # }
    ########################################
    #fim qp4
    #############################################
    
    
    
  }
  
 
  #date_left <- date_right
  date_right <- seq.POSIXt( date_right, length = 2, by = '4 weeks' ) [ 2 ] 
  
# Janela de tempo deslizante
 # if (as.integer(difftime(date_right,date_left,units = "weeks"))>24)
  #  date_left<- seq.POSIXt( date_right, length = 2, by = '-24 weeks' ) [ 2 ] 
  
}

# evoluçao dos desenvolvedores generalistas ao longo do tempo
#dev_all_time2 <- dev_all_time %>% mutate ( generalista = n_platform>=2 ) %>% group_by( generalista, iteracao ) %>% summarise(soma_porc = sum(porcentage), soma_n = sum(n)) %>% filter( generalista == TRUE )
dev_all_time2 <- dev_all_time %>% filter( tipo == "gen" )

dev_all_time2_n_level <- kpss.test(dev_all_time2$n, null =  "Level") 
dev_all_time2_n_trend <- kpss.test(dev_all_time2$n, null =  "Trend") 

dev_all_time2_porc_level <- kpss.test(dev_all_time2$porcentage, null =  "Level") 
dev_all_time2_porc_trend <- kpss.test(dev_all_time2$porcentage, null =  "Trend") 

dev_gen_time2 <- dev_gen_time %>% select(n_platform,iteracao,porcentage,n) %>% filter(n_platform==2)
dev_gen_time3 <- dev_gen_time %>% select(n_platform,iteracao,porcentage,n) %>% filter(n_platform==3)
dev_gen_time4 <- dev_gen_time %>% select(n_platform,iteracao,porcentage,n) %>% filter(n_platform==4)
dev_gen_time5 <- dev_gen_time %>% select(n_platform,iteracao,porcentage,n) %>% filter(n_platform==5)

dev_gen_med <- dev_gen_time %>% select(n_platform,iteracao,n) %>% group_by(iteracao) %>% summarise(n_platform=mean(n_platform), n=sum(n) )
dev_gen_med_level <- kpss.test(dev_gen_med$n, null =  "Level") 
dev_gen_med_trend <- kpss.test(dev_gen_med$n, null =  "Trend") 

dev_gen_time2_n_level <- kpss.test(dev_gen_time2$n, null =  "Level") 
dev_gen_time2_n_trend <- kpss.test(dev_gen_time2$n, null =  "Trend") 
dev_gen_time2_porc_level <- kpss.test(dev_gen_time2$porcentage, null =  "Level") 
dev_gen_time2_porc_trend <- kpss.test(dev_gen_time2$porcentage, null =  "Trend")

dev_gen_time3_n_level <- kpss.test(dev_gen_time3$n, null =  "Level") 
dev_gen_time3_n_trend <- kpss.test(dev_gen_time3$n, null =  "Trend") 
dev_gen_time3_porc_level <- kpss.test(dev_gen_time3$porcentage, null =  "Level") 
dev_gen_time3_porc_trend <- kpss.test(dev_gen_time3$porcentage, null =  "Trend")

dev_gen_time4_n_level <- kpss.test(dev_gen_time4$n, null =  "Level") 
dev_gen_time4_n_trend <- kpss.test(dev_gen_time4$n, null =  "Trend") 
dev_gen_time4_porc_level <- kpss.test(dev_gen_time4$porcentage, null =  "Level") 
dev_gen_time4_porc_trend <- kpss.test(dev_gen_time4$porcentage, null =  "Trend")

dev_gen_time5_n_level <- kpss.test(dev_gen_time5$n, null =  "Level") 
dev_gen_time5_n_trend <- kpss.test(dev_gen_time5$n, null =  "Trend") 
dev_gen_time5_porc_level <- kpss.test(dev_gen_time5$porcentage, null =  "Level") 
dev_gen_time5_porc_trend <- kpss.test(dev_gen_time5$porcentage, null =  "Trend")

# evolucao dos desenvolvedores que trabalham com dispositivos desktop e mobile ao longo do tempo
devicetype_time2 <- devicetype_time %>%  filter( dispositivo == "Both" )

devicetype_time2_n_level <- kpss.test(devicetype_time2$developer, null =  "Level") 
devicetype_time2_n_trend <- kpss.test(devicetype_time2$developer, null =  "Trend") 

devicetype_time2_porc_level <- kpss.test(devicetype_time2$porcentage, null =  "Level") 
devicetype_time2_porc_trend <- kpss.test(devicetype_time2$porcentage, null =  "Trend") 


save.image(as.character(logs$ws_analisetemporal[it]))
}
###############################################3
###########################################
###################################
# fim analise temporal
###############################################
##############################################
###############################################

#remove(platform3, platform3_aux,file_list, data_aux, date_left,date_right,nperiods, authors3, authors4, authors41, authors42, authors43, authors5, authors6, authors62, authors63,  commits, rules_dev_aux, rules_commit_aux, independente3, android3, linux3, win3,  iphone3, macosx3,devcore_aux, devall_aux, devperi_aux, devicetype_aux, devicetype2_aux, devicetype3_aux, j , rules_dev_aux, rules_commit_aux)

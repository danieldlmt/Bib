library(dplyr, warn.conflicts = FALSE)
library(arules)
library(stringr)

################################
# Carregamento do log de commits
# Escolha apenas um log
# 
###############################
#logs <- data.frame(sistema=c("Allegro", "SDL"), caminho = c('~/bib/log_allegro.R', '~/bib/log_SDL.R'))
#source('~/bib/log_SDL.R')
#source('~/bib/log_allegro.R')

########################################
########################################
########################################
#inicio da analise 
#######################################
#######################################
#######################################
logs <- data.frame(sistema=c("Allegro", "SDL"), caminho = c('~/bib/log_allegro.R', '~/bib/log_SDL.R'),ws_analise=c('~/bib/analise_allegro.RData', '~/bib/analise_SDL.RData'), ws_analisetemporal = c('~/bib/analisetemporal_allegro.RData', '~/bib/analisetemporal_SDL.RData'), ws_analisetemporaljanela = c('~/bib/analisetemporaljanela_allegro.RData', '~/bib/analisetemporaljanela_SDL.RData')  )
for (it in 1:length(logs$sistema)){
  rm(list = ls()[!ls() %in% c("logs","it")])
  source(as.character(logs$caminho[it]) )


######################################
# numero de plataformas modificadas em cada commit
    platform <- data %>%
      select(rev, platform, author)%>%
      group_by(rev) %>%
      summarise(n_plat = n_distinct(platform), platform = paste(unique(platform), collapse=", "), author=unique(author)) %>%
      arrange(rev)

# lista de arquivos modificados em cada commit
    file_list <- data %>%
      select(rev, path) %>%
      group_by(rev) %>%
      arrange(path)%>%
      summarise( n_files = n_distinct(path), path = paste(unique(path), collapse=", "))

# upgrade em platform, + numero de arquivos modificados em cada commit
    platform <- merge(platform,file_list,by="rev")

# quantidade de commits agrupados de acordo com o numero de plataformas
# por exemplo, 100 commits modificaram 2 plataformas, 40 commits modificaram 3 plataformas...
    platform2 <- platform %>%
      mutate(n_plat = if_else(str_detect(platform, "Independente"), as.numeric(n_plat-1) ,as.numeric(n_plat) )) %>%
      group_by(n_plat) %>%
      summarise(n_rev= n())%>%
      mutate (porcentage = round(n_rev/sum(n_rev) * 100, 1) ) %>%
      arrange(n_plat)

# distribuicao de plataformas que modificaram apenas um diretorio 
    platform3 <- platform %>% filter(n_plat==1)
    platform3 <-platform3 %>% select(platform) %>% group_by(platform) %>% summarise( n = n() ) %>% filter(!platform=="Independente")
    platform3 <-platform3 %>% mutate( porcentage = n*100/sum(n))

# verificacao de modificacao do diretorio independente nos commits que modificaram mais de um diretorio 
    platform4 <- platform %>% filter(!platform=="Independente")
    porcentage_independente <- sum( str_detect(platform4$platform,"Independente") ) * 100 / nrow(platform4) 
    
    

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

# numero total de modificacoes realizadas em cada plataforma
    os_summary <- data.frame(n_commit = c(sum(android$android),sum(linux$linux),sum(win$win),sum(iphone$iphone),sum(macosx$macosx)), n_dev = c(nrow(android),nrow(linux),nrow(win),nrow(iphone),nrow(macosx)), row.names= c('android', 'linux', 'win', 'iphone', 'macosx'))
    os_summary2 <- as.data.frame( t(os_summary))    
    os_summary3 <- data.frame(n_commit = c(sum(android$android)+sum(linux$linux)+sum(win$win)+sum(iphone$iphone)+sum(macosx$macosx),sum(independente$independente)), row.names= c('especifico', 'independente'))
    
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
    devesp <- ifelse(devesp>0, 1, 0)
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
  

  
########################
########################
# inicio QP3
########################
########################
  #Regra de associação 
    # [todos] numero de modificacoes por plataforma de cada desenvolvedor  
      authors4 <- authors3 %>% select(android,linux,win, iphone,macosx,independente) 
      authors4[colnames(authors4)][is.na(authors4[colnames(authors4)])] <- 0
      authors5 <- ifelse(authors4>0, 1, 0)
      capture.output(rules_dev_apriori  <-  apriori(authors5,parameter=list(support=0.12,confidence=0.8,minlen=1,maxlen=10, target="rules"), appearance=NULL, control = list(verbose = FALSE))  )
      capture.output(rules_dev_inspect <- inspect(sort(rules_dev_apriori, by = "support")))
      
  # Afinidade
    afi_dev <- affinity(authors5) 
    
  # Crosstable entre as plataformas (desenvolvedores)
    authors5_trans <- as(authors5, "transactions")
      # Parametro count
        Crosstable_dev_count <- crossTable(authors5_trans, measure="count", sort=TRUE)
      # Parametro support
        Crosstable_dev_support <- crossTable(authors5_trans, measure="support", sort=TRUE)
  
########################
########################
# fim QP3
########################
########################
  
  
########################
########################
# inicio QP4
########################
########################
      
  # Regra de associacao
    commits <- as.data.frame.matrix(xtabs(~ rev + platform, data=data[c('platform', 'rev')]))
    commits <- commits %>% select (Android, Linux, Windows, iPhone, macOS, Independente)  %>%  filter (  !(rowSums(commits) == 0) )
    rownames(commits)<-NULL
    commits<- ifelse(commits>0, 1, 0)

    capture.output(rules_commit_apriori <-  apriori(commits,parameter=list(support=0.0015,confidence=0.8,minlen=1,maxlen=10)))
    capture.output( rules_commit_inspect <-  inspect(sort(rules_commit_apriori, by = "support"))) 
  
  # Afinidade
    afi_commit <- affinity(commits) 
      
  # Crosstable entre as plataformas (commit)
    commits_trans <- as(commits, "transactions")
      # parametro count
      Crosstable_commit_count <- crossTable(commits_trans, measure="count", sort=TRUE)
      # parametro support
      Crosstable_commit_support <- crossTable(commits_trans, measure="support", sort=TRUE)

  
########################
########################
# fim QP4
########################
########################
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
  
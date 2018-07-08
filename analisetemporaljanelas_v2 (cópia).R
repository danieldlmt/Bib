library(dplyr, warn.conflicts = FALSE)
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
########################################
########################################
########################################
#inicio da analise temporal com janela deslizante
#######################################
#######################################
#######################################

#variaveis bind
saida_plat_bind <- data.frame(iteracao=NA,author=NA,prob=NA, ind=NA,esp=NA, gen=NA, mobile=NA, desktop=NA, indd=NA, tipo=NA,ndev=NA)
saida_device_bind <- data.frame(iteracao=NA,author=NA,prob=NA, ind=NA,esp=NA, gen=NA, mobile=NA, desktop=NA, indd=NA, tipo=NA,ndev=NA)


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

    

      # Limiares
      # n_line_add - numero de linhas adicionadas por plataforma de todos os devs
      # commits - numero de commits por plataforma (incluindo cod. independente) de todos os devs
              platform_new <- data %>%
                select( platform, author,n_line_add,n_line_del,rev,path,date)%>%
                group_by(author,platform) %>%
                summarise(n_line_add=sum(n_line_add),
                          n_line_del=sum(n_line_del), 
                          commits=n_distinct(rev),
                          files=n_distinct(path), 
                          first=min(as.POSIXct(date)),
                          last=max(as.POSIXct(date)) ) %>%
                mutate( ind = if_else(platform == "Independente",1,0))
              
              limiares <- platform_new %>% 
                ungroup() %>%
                select(n_line_add,commits)
              data_aux <- data%>%
                filter(as.POSIXct(date)>=date_left  & as.POSIXct(date)<date_right )  
              
            #  remove_outliers <- function(x, na.rm = TRUE) {
            #    qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
            #    H <- 1.5 * IQR(x, na.rm = TRUE)
            #    y <- x
            #    y[x < (qnt[1] - H)] <- NA
            #    y[x > (qnt[2] + H)] <- NA
            #    y
            #  }        
              qnt <- quantile(limiares$n_line_add, probs=c(.25, .75))
              H <- 1.5 * IQR(limiares$n_line_add, na.rm = TRUE)
              limiar_line <- limiares$n_line_add
              limiar_line[limiares$n_line_add < (qnt[1] - H)] <- median(limiares$n_line_add)
              limiar_line[limiares$n_line_add > (qnt[2] + H)] <- median(limiares$n_line_add)
              
              qnt <- quantile(limiares$commits, probs=c(.25, .75), na.rm = TRUE)
              H <- 1.5 * IQR(limiares$commits, na.rm = TRUE)
              limiar_commit <- limiares$commits
              limiar_commit[limiares$commits < (qnt[1] - H)] <- median(limiares$commits)
              limiar_commit[limiares$commits > (qnt[2] + H)] <- median(limiares$commits)
              
             # limiar_line<- remove_outliers(limiares$n_line_add) 
             # limiar_commit<- remove_outliers(limiares$commits) 
              
              n_dev<-n_distinct(data_aux$author)
              
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
      
     
      
      aux <- platform_new %>%
        select(author,ind)%>%
        group_by(author)%>%
        summarise(independente=max(ind),
                  n_plat = n()-independente)%>%
        mutate(esp = if_else(n_plat ==1,1,0),
               gen = if_else(n_plat >1,1,0))%>%
        select(author, independente, n_plat)
      
      platform_new <- platform_new %>% 
        inner_join(aux, by="author")
      
      platform_new$ind <- NULL
      
      # aqui precisarei de TRES platform_new
      # uma para o calculo dos devs especialistas
      # uma para o calculo dos generalistas tirando as médias dos parametros. 
      # uma para independente
      
      
      platform_new_gen <- platform_new %>%
        group_by(author)%>%
        summarise(platform = paste(platform, collapse = " "), 
                  n_line_add=mean(n_line_add),
                  n_line_del=mean(n_line_del), 
                  commits=mean(commits),
                  files=mean(files), 
                  first=min(as.POSIXct(first)),
                  last=max(as.POSIXct(last)),
                  independente = max(independente),
                  n_plat = max(n_plat)  )%>%
        filter( n_plat > 1 )
      
      platform_new_esp <- platform_new %>%
        filter(platform != "Independente")
      
      
      
      platform_new_ind <- platform_new %>% filter(platform == "Independente")
      
      
      
      # Considerarei 7 tipos de desenvolvedores de acordo com o tipo de codigo fonte que eles trabalham (Independente, Especifico)
      # Esp - Desenvolvedor que trabalha com apenas uma plataforma
      # Gen - Desenvolvedor que trabalha com mais de uma plataforma
      # Ind - Desenvolvedor que trabalha apenas com o codigo independente
      # Esp - Gen - Desenvolvedor generalista que possui alto conhecimento em pelo menos uma plataforma
      # Ind - Esp - Desenvolvedor especialista que tbm trabalha com o codigo independente
      # Ind - Gen - Desenvolvedor generalista que tbm trabalha com o codigo independente
      # Ind - Esp - Gen - Desenvolvedor generalista que possui alto conhecimento em pelo menos uma plataforma e tbm trabalha com o codigo independente
      
      # Logica de classificação de desenvovedores
      # Especialistas
      # sem restrição de tempo (por causa dos bug fixers)
      # n_lines_add > limiar ou commits > limiar
      # limiar um pelin mais alto do que o do generalista
      # Generalistas
      # media(n_lines_add) > limiar ou  media(commits) > limiar
      
      
      # vetor de limiares
      prob<- c(0.10,0.20,0.30,0.40,0.50,0.60,0.70)
      
      #proposital para opter matrix com 0 elements
      platform_new_ind_bind <- platform_new_ind%>%filter(author=="")
      platform_new_gen_bind <- platform_new_gen%>%filter(author=="")
      platform_new_esp_bind <- platform_new_esp%>%filter(author=="")
      platform_new_bind <- platform_new_esp%>%filter(author=="")
      
      for (itt in 1:length(prob)){
        #independente 
        aux <- platform_new_ind%>%
          filter(n_line_add > quantile(limiar_line, prob[itt]) ||  commits > quantile(limiar_commit, prob[itt]))%>%
          mutate(prob = prob[itt], 
                 ind = 1,
                 esp = 0,
                 gen = 0)
        platform_new_ind_bind<-bind_rows(platform_new_ind_bind, aux)
        
        aux <- platform_new_esp %>% 
          filter(n_line_add > quantile(limiar_line, prob[itt]*1.3) ||  commits > quantile(limiar_commit, prob[itt]*1.3) ) %>%
          mutate(prob = prob[itt], 
                 ind = 0,
                 esp = 1,
                 gen = 0 )
        platform_new_esp_bind<-bind_rows(platform_new_esp_bind, aux)
        
        aux <- platform_new_gen %>% filter(n_line_add > quantile(limiar_line, prob[itt]) ||  commits > quantile(limiar_commit, prob[itt]) ) %>%
          mutate(prob = prob[itt], 
                 ind = 0,
                 esp = 0,
                 gen = 1 )
        platform_new_gen_bind<-bind_rows(platform_new_gen_bind, aux)
      }
      
      platform_new_bind<-bind_rows(platform_new_bind, platform_new_ind_bind,platform_new_esp_bind,platform_new_gen_bind)

      
      # adicao da analise mobile desktop para facilitar pra logo 
      platform_new_bind<-platform_new_bind%>% 
        rowwise()%>%
        mutate(mobile = if_else( str_detect(platform,"Iphone|Android"),1,0 ),
               desktop = if_else( str_detect(platform,"Windows|Linux|macOS"),1,0 ),
               indd = if_else( str_detect(platform,"Independente"),1,0 ) )
      
      platform_new_bind<-platform_new_bind %>% 
        group_by(author,prob)%>%
        summarise(ind = max(ind),
                  esp = max(esp),
                  gen = max(gen),
                  mobile = max(mobile),
                  desktop = max(desktop),
                  indd = max(indd))%>%
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
  summarise(n = n(),ndev=max(ndev)) %>%
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
  summarise(n = n(),ndev=max(ndev))%>%
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
  summarise(n = n(),ndev=max(ndev)) %>%
  mutate(porc = n*100/ndev) %>%
  select (iteracao,prob, tipo, n, ndev, porc)


esp_bind2 <- xc %>% filter(tipo=="esp")

gen_bind2 <- xc %>% filter(tipo=="gen")








    save.image(as.character(logs$ws_analisetemporaljanela[it]))
}
###############################################3
###########################################
###################################
# fim analise temporal com janela de tempo
###############################################
##############################################
###############################################

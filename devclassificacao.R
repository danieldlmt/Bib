#######################################################################################
# Devs classification in generalist vs speacist and mobile vs desktop
  # used in both analise_v2 and analisetemporaljanelas_v2

#generalis vs specialist
  # We consider 7 dev type according to the source code (Independente, Especifico)
    # Esp - Desenvolvedor que trabalha com apenas uma plataforma
    # Gen - Desenvolvedor que trabalha com mais de uma plataforma
    # Ind - Desenvolvedor que trabalha apenas com o codigo independente
    # Esp - Gen - Desenvolvedor generalista que possui alto conhecimento em pelo menos uma plataforma
    # Ind - Esp - Desenvolvedor especialista que tbm trabalha com o codigo independente
    # Ind - Gen - Desenvolvedor generalista que tbm trabalha com o codigo independente
    # Ind - Esp - Gen - Desenvolvedor generalista que possui alto conhecimento em pelo menos uma plataforma e tbm trabalha com o codigo independente
#mobile and desktop
  # we consider 3 dev types
    # mobile
    # desktop
    # mobile and desktop
#######################################################################################

# count the number of platform of each developer and verify if the dev support the independent code
aux <- platform_new %>%
  select(author,ind)%>%
  group_by(author)%>%
  summarise(independente=max(ind),
            n_plat = n()-independente)%>%
  mutate(esp = if_else(n_plat ==1,1,0),
         gen = if_else(n_plat >1,1,0))%>%
  select(author, independente, n_plat)

#add column independente and n_plat in platform_new 
platform_new <- platform_new %>% 
  inner_join(aux, by="author")

#remove column 'ind'
platform_new$ind <- NULL

# filter to analyse the generalists
platform_new_gen <- platform_new %>%
  group_by(author)%>%
  summarise(platform = paste(platform, collapse = " "), 
            n_line_add=sum(n_line_add), ########
            n_line_del=mean(n_line_del), 
            commits=mean(commits),
            files=sum(files), ######
            first=min(as.POSIXct(first)),
            last=max(as.POSIXct(last)),
            independente = max(independente),
            n_plat = max(n_plat)  )%>%
  mutate(lineadd_file = n_line_add/files)%>% #mean added lines per file of each dev
  filter( n_plat > 1 )

# filter to analyse the specialists
platform_new_esp <- platform_new %>%
  mutate(lineadd_file = n_line_add/files)%>%
  filter(platform != "Independente")

# filter to analyse the devs who support the independent code
platform_new_ind <- platform_new %>%
  mutate(lineadd_file = n_line_add/files)%>%
  filter(platform == "Independente")

# probabiblity vector
prob<- c(0.10,0.20,0.30,0.40,0.50,0.60,0.70)

# empty matrix
platform_new_ind_bind <- platform_new_ind%>%filter(author=="")
platform_new_gen_bind <- platform_new_gen%>%filter(author=="")
platform_new_esp_bind <- platform_new_esp%>%filter(author=="")
platform_new_bind <- platform_new_esp%>%filter(author=="")

# Testing multiple probabilities
  # Developer classification loop
    # Specialist logic
     # n_lines_add > threshold OR commits > threshold
     # treshold is 1.3 higher
  # Generalistas
    # mean(n_lines_add) > threshold OR  mean(commits) > threshold
    for (itt in 1:length(prob)){
      #independente 
      aux <- platform_new_ind%>%
        filter(lineadd_file > quantile(limiar_line, prob[itt]) )%>%
        mutate(prob = prob[itt], 
               ind = 1,
               esp = 0,
               gen = 0)
      platform_new_ind_bind<-bind_rows(platform_new_ind_bind, aux)
      #specialist
      aux <- platform_new_esp %>% 
        filter(lineadd_file > quantile(limiar_line, prob[itt]*1.3)  ) %>%
        mutate(prob = prob[itt], 
               ind = 0,
               esp = 1,
               gen = 0 )
      platform_new_esp_bind<-bind_rows(platform_new_esp_bind, aux)
      #generalist
      aux <- platform_new_gen %>% filter(lineadd_file > quantile(limiar_line, prob[itt])  ) %>%
        mutate(prob = prob[itt], 
               ind = 0,
               esp = 0,
               gen = 1 )
      platform_new_gen_bind<-bind_rows(platform_new_gen_bind, aux)
    }
#join all results
platform_new_bind<-bind_rows(platform_new_bind, platform_new_ind_bind,platform_new_esp_bind,platform_new_gen_bind)

# Mobile and desktop analysis
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
            indd = max(indd))



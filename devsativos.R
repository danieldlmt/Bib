
# Classificacao de desenvolvedores ativos 
  # periodo de contribuição minimo de 24 semanas
    dev_ativo <-   data %>%
      select( author,n_line_add,n_line_del,rev,path,date)%>%
      group_by(author) %>%
      summarise(n_line_add=sum(n_line_add),
                n_line_del=sum(n_line_del), 
                commits=n_distinct(rev),
                files=n_distinct(path), 
                first=min(as.POSIXct(date)),
                last=max(as.POSIXct(date)) )%>%
      arrange(desc(n_line_add))%>%
      mutate(periodo = difftime(as.POSIXct(last) ,as.POSIXct(first), units = "weeks"))%>% 
      mutate(media_commit = commits/as.numeric(periodo),
             porc_line_add= n_line_add*100/sum(n_line_add),
             porc_line_add_cum = 100*cumsum(n_line_add)/sum(n_line_add),
             porc_commits= commits*100/sum(commits),
             porc_commits_cum = 100*cumsum(commits)/sum(commits) )%>%
      filter(as.numeric(periodo)>=24)    
    
    #data<- right_join(data, dev_ativo,by="author")
    
    #Como filtrar os desenvolvedores ? (talvez melhor não filtrar)
      # A case study apache, Mockus 2000
        # core : 88% line add
        # non core: rest
      # Analysis biodiversity, Maltragas 2014, MSR 2014
        # cluster para agrupar desenvolvedores com perfis similares
      # Process mining soft rep, ponein 2011
        # ativos: 1commit/30dias
      # OSSMETER report, 2014
        # ativo: 1commit/15dias
        # inativo: no commit in 3 months but 1 every 6 months
        # core: people with highest loc changes 
    
    
    #Plot de porcentagem acumulada
    #   plot(c(1:nrow(dev_ativo)),dev_ativo$porc_line_add_cum, type = "l", ylab = "% de linhas modificadas", xlab= "Numero de desenvolvedores")
    
    #%>%              filter(as.numeric(periodo)>=24 & media>=1/6)
    
    
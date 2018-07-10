
source("~/bib/log_allegro.R" )

# Filtro de desenvolvedores ativos 
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
         porc_commits = commits*100/sum(commits))%>%
  filter(as.numeric(periodo)>=24)    

allegro_dev <- data.frame(allegro =c(sum(dev_ativo$porc_line_add), sum(dev_ativo$porc_commits)),
                          row.names = c("line_add", "commits") ) 


source("~/bib/log_SDL.R" )

# Filtro de desenvolvedores ativos 
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
         porc_commits = commits*100/sum(commits))%>%
  filter(as.numeric(periodo)>=24)    

SDL_dev <- data.frame(SDL =c(sum(dev_ativo$porc_line_add), sum(dev_ativo$porc_commits)),
                          row.names = c("line_add", "commits") ) 



source("~/bib/log_SFML.R" )

# Filtro de desenvolvedores ativos 
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
         porc_commits = commits*100/sum(commits))%>%
  filter(as.numeric(periodo)>=24)    

SFML_dev <- data.frame(SFML =c(sum(dev_ativo$porc_line_add), sum(dev_ativo$porc_commits)),
                          row.names = c("line_add", "commits") ) 



source("~/bib/log_coco2dx.R" )

# Filtro de desenvolvedores ativos 
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
         porc_commits = commits*100/sum(commits))%>%
  filter(as.numeric(periodo)>=24)    

coco2dx_dev <- data.frame(coco2dx =c(sum(dev_ativo$porc_line_add), sum(dev_ativo$porc_commits)),
                          row.names = c("line_add", "commits") ) 


x<- cbind(allegro_dev,SDL_dev,SFML_dev,coco2dx_dev)

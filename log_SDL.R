#library(dplyr)
######################################### Import
#import data 
data <- readLines('data/log_SDL.csv')
data <- gsub(pattern="\t", replacement=";", data, fixed = TRUE)
datatemp <- tempfile() 
writeLines(data, con = datatemp) 
data <- read.table(datatemp, sep=";" , header=F, col.names=c('rev', 'author', 'date','n_line_add' ,'n_line_del' , 'path'))
#data <- read.csv('data/log_SDL.csv', sep=";" , header=F, col.names=c('rev', 'author', 'date', 'path'))


##Suporte
##Android foi a ultima plataforma jun/2010

###################################### Filter 1
#filtro de data
data <- data %>%
  mutate(module = gsub('/[^/]+$', '', path)) %>%
  filter(grepl('^src.*', module)  )%>%
  filter(as.Date(date)>as.Date("2010-06-10 00:00:2018-11-05 -0000")) 
  #%>%filter(n_line_add>=3)

#########################################3
#add column platform (module is obsolete)

plats<- read.csv("data/plats.csv")
plats<- plats %>% select(diretorio,plataforma)

data <- data %>% mutate(platform = "Outros")

for (i in 1:dim(plats)[1]){
  #print(plats[i,1]) 
  data$platform <- ifelse(data$platform=="Outros" & grepl(plats[i,1],data$path),paste("", plats[i,2], sep=""),data$platform)
}

data <- data %>% filter(platform=="Independente" |platform=="Windows"|platform=="Linux"|platform=="macOS"|platform=="Android"|platform=="iPhone" )


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
  filter(as.numeric(periodo)>=24)     %>%
  mutate(media_commit = commits/as.numeric(periodo),
         porc_line_add= n_line_add*100/sum(n_line_add),
         porc_line_add_cum = 100*cumsum(n_line_add)/sum(n_line_add))%>%
  select(author)

#data<- right_join(data, dev_ativo,by="author")

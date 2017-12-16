library(dplyr, warn.conflicts = FALSE)
######################################### Import
#import data 
data <- read.csv('data/log_libosmscout.csv', sep=";" , header=F, col.names=c('rev', 'author', 'date', 'path'))

#write.csv(plats, "plats.csv")
plats<- read.csv("data/plats.csv")
plats<- plats %>% select(diretorio,plataforma)

##Suporte
##Android foi a ultima plataforma jun/2010

###################################### Filter 1
#filtro de data
data <- data %>%
  mutate(module = gsub('/[^/]+$', '', path))# %>%
  #filter(grepl('([.]c|[.]h)', path))
  #filter(grepl('^src.*', module)  ) %>%
  #filter(as.Date(date)>as.Date("2011-01-01 00:00:2016-11-05 -0000"))


data <- data %>% mutate(platform = ifelse(path%in%Makefiles$Path, Makefiles$Codigo[match(path, Makefiles$Path)], "Outros"))

for (i in 1:dim(plats)[1]){
  #print(plats[i,1]) 
  data$platform <- ifelse(data$platform=="Outros" & grepl(plats[i,1],data$path),paste("", plats[i,2], sep=""),data$platform)
}

remove(MakefileAndroid,MakefileiOS,MakefileLinux,MakefilemacOS, Makefiles, MakefileWindows,plats, max.len, i)

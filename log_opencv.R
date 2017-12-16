library(dplyr, warn.conflicts = FALSE)

######################################### Import
#import data 
data <- read.csv('data/log_opencv.csv', sep=";" , header=F, col.names=c('rev', 'author', 'date', 'path'))
#import data 

###################################### Filter 1
#add column of module/directory and filter unwanted modules/directory
data <- data %>%
  mutate(module = gsub('/[^/]+$', '', path)) %>%
  #filter(grepl('([.]c|[.]h)', path))
  filter(grepl('^src.*', module)  ) %>%
  filter(as.Date(date)>as.Date("2011-01-01 00:00:2016-11-05 -0000"))

#########################################3
#add column platform (module is obsolete)
plats<- read.csv("data/plats.csv")
plats<- plats %>% select(diretorio,plataforma)

data <- data %>% mutate(platform = "Outros")

for (i in 1:dim(plats)[1]){
  #print(plats[i,1]) 
  data$platform <- ifelse(data$platform=="Outros" & grepl(plats[i,1],data$path),paste("", plats[i,2], sep=""),data$platform)
}

remove(plats, i)

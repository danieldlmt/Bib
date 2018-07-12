library(dplyr)

# Read log file .csv
  # Log extractor is available at https://github.com/danieldlmt/Extrator-de-Log-GIT
    # .csv file format
     # cols: [rev];[author];[date];[added_line removed_line];[path]

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
  filter(as.Date(date)>=as.Date("2010-06-26")) 
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



remove(plats, i)
save  (data, file ="~/bib/workspace/SDL_data.RData")

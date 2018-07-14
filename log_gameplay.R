library(dplyr, warn.conflicts = FALSE)

# Read log file .csv
  # Log extractor is available at https://github.com/danieldlmt/Extrator-de-Log-GIT
    # .csv file format
     # cols: [rev];[author];[date];[added_line removed_line];[path]

######################################### Import
#import data 
data <- readLines('./data/log_gameplay.csv')
data <- gsub(pattern="\t", replacement=";", data, fixed = TRUE)
datatemp <- tempfile() 
writeLines(data, con = datatemp) 
data <- read.table(datatemp, sep=";" , header=F, col.names=c('rev', 'author', 'date','n_line_add' ,'n_line_del' , 'path'))
#data <- read.csv('data/log_allegro.csv', sep=";" , header=F, col.names=c('rev', 'author', 'date', 'path'))
#import data 

###################################### Filter 1
#add column of module/directory and filter unwanted modules/directory
data <- data %>%
  mutate(module = gsub('/[^/]+$', '', path)) %>%
  #filter(grepl('([.]c|[.]h)', path))
  filter(grepl('^gameplay/src/*', module)  ) %>% 
  filter(!str_detect( path,"blackberry"))%>%
  filter(as.Date(date)>=as.Date("2001-04-12")) 
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
save  (data, file = "./workspace/gameplay_data.RData" )

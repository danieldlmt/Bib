library(dplyr, warn.conflicts = FALSE)
# Read log file .csv
  # Log extractor is available at https://github.com/danieldlmt/Extrator-de-Log-GIT
  # .csv file format
    # cols: [rev];[author];[date];[added_line removed_line];[path]

######################################### Import
#import data 
data <- readLines('./data/log_SFML.csv')
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
  filter(grepl('^src.*', module)  ) %>%
  filter(as.Date(date)>=as.Date("2014-04-20 ")) 
#%>%filter(n_line_add>=3)

data <- data %>% mutate(platform = "Independente")

# platform association
source("./associacao.R")

remove(plats,i)
save  (data, file ="./workspace/SFML_data.RData")
library(dplyr, warn.conflicts = FALSE)
library(stringr)
# Generate the appendix 

  # Create variable 'log'  that contains the paths of the scripts and workspaces (.RData)
       source("./paths.R")

    # Loop that walks through 'logs' to generate the appendix 
       # appendix contains a list with the association of platform we did for each file
    for (it in 1:length(logs$sistema)){
      # rm(list = ls()[!ls() %in% c("logs","it")])
      source(as.character(logs$caminho[it]) )
      #load (as.character(logs$data[it]))
      
      x<-data %>%select(module, platform,path) %>% group_by(module) %>% summarise(platform = paste(unique(platform), collapse=" "),path = paste(unique(path), collapse=", "))
      x<-x%>% select (module,platform) %>%
        filter(!str_detect(module,"^\\.")) %>% # Remove hidden filees
        filter(!str_detect(module,"\\..{1,3}$")) %>%  # remove files in module
        mutate(module = gsub('^(([^/]+/){3}).*$','\\1',module) ) %>% # cut module in three slashes  
        mutate(module = gsub('/$','',module) ) %>% # slaches at the end
        group_by(module) %>%
        summarise( platform = paste(unique(trimws(unlist(strsplit(platform,split=" ",fixed=F,perl=T)))),collapse = " ")) # paste removing duplicateed
      
      

    #    group_by ( module ) %>%
    #    summarise ( platform = paste ( unique ( platform ) , collapse = " " ) )%>%
    #    mutate ( platform =  )
      
      
     # gsub('^(([^/]+/){4}).*$','\\1',module)
       
    #sample <- "cocos2dx/platform/third_party/android/prebuilt/libfreetype2/include/freetype2"
    #gsub('^((.*?/){3}).*$','\\1',sample)
     # grep('^[^/]*/[^/]*/[^/]*','',sample1)
      #gsub('^(?:[^/]*/){3}','',sample1)
      #gsub('^([^/]*/[^/]*)/.*$','\1',sample1)

      path<-paste("./anexos/filelist_",tolower(as.character(logs$sistema[it])),".csv",sep = "")
      write.csv(x, file = , path, quote = FALSE,row.names = FALSE)
    }
    

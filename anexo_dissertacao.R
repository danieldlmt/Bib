library(dplyr, warn.conflicts = FALSE)
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
      x<-x%>% select (module,platform) #%>%
      #  mutate(module = gsub('^([^/]*(?:/[^/]*){2})/.*$','',module) ) 
      
    #    group_by ( module ) %>%
    #    summarise ( platform = paste ( unique ( platform ) , collapse = " " ) )%>%
    #    mutate ( platform =  )
       
      #sample1= "cocos/platform/android/java/src/org/cocos2dxlib"
      #gsub('^([^/]*(?:/[^/]*){3})/.*$','',sample1)
      
     # grep('^[^/]*/[^/]*/[^/]*','',sample1)
      #gsub('^(?:[^/]*/){3}','',sample1)
      #gsub('^([^/]*/[^/]*)/.*$','\1',sample1)

      path<-paste("./anexos/filelist_",tolower(as.character(logs$sistema[it])),".csv",sep = "")
      write.csv(x, file = , path, quote = FALSE,row.names = FALSE)
    }
    

library(dplyr, warn.conflicts = FALSE)
# Generate the appendix 

  # Create variable 'log'  that contains the paths of the scripts and workspaces (.RData)
       source("~/bib/paths.R")

    # Loop that walks through 'logs' to generate the appendix 
       # appendix contains a list with the association of platform we did for each file
    for (it in 1:length(logs$sistema)){
      # rm(list = ls()[!ls() %in% c("logs","it")])
      #source(as.character(logs$caminho[it]) )
      load (as.character(logs$data[it]))
      
      x<-data %>%select(module, platform,path) %>% group_by(module) %>% summarise(platform = paste(unique(platform), collapse=" "),path = paste(unique(path), collapse=", "))
      x<-x%>% select (module,platform)
      path<-paste("~/bib/anexos/filelist_",tolower(as.character(logs$sistema[it])),".csv",sep = "")
      write.csv(x, file = , path, quote = FALSE,row.names = FALSE)
    }
    

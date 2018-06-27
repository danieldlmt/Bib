x<-data %>%select(module, platform,path) %>% group_by(module) %>% summarise(platform = paste(unique(platform), collapse=" "),path = paste(unique(path), collapse=", "))
x<-x%>% select (module,platform)
write.csv(x, file = "filelist.csv", sep =",", quote = FALSE,row.names = FALSE)

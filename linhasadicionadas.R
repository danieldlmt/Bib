source("./paths.R")

linhasadd <- data.frame (  )

# Loop that walks through 'logs' to get .RData of each library
for (it in 1:length(logs$sistema)){
  # rm(list = ls()[!ls() %in% c("logs","it")])
  #source(as.character(logs$caminho[it]) )
  load (as.character(logs$data[it]))
  
  x <- data %>%  
    select( n_line_add)
  
  limiares <- x %>% 
    ungroup() %>%
    select(n_line_add)
  
  qnt <- quantile(limiares$n_line_add, probs=c(.25, .75))
  H <- 1.5 * IQR(limiares$n_line_add, na.rm = TRUE)
  limiar_line<- limiares$n_line_add
  limiar_line[limiares$n_line_add < (qnt[1] - H)] <- median(limiares$n_line_add)
  limiar_line[limiares$n_line_add > (qnt[2] + H)] <- median(limiares$n_line_add)
  #boxplot(limiar_line)
  #summary(limiar_line)
  #length(limiar_line)
  
  #lines per file for each dev vector
  x <- data %>%  
    select( platform, author,n_line_add,n_line_del,rev,path,date)%>%
    group_by(author)%>%
    summarise(
      lineadd_file = sum(n_line_add)/n()
    )
  limiares <- x %>% 
    ungroup() %>%
    select(lineadd_file)
  
  qnt <- quantile(limiares$lineadd_file, probs=c(.25, .75))
  H <- 1.5 * IQR(limiares$lineadd_file, na.rm = TRUE)
  xlimiar_line <- limiares$lineadd_file
  xlimiar_line[limiares$lineadd_file < (qnt[1] - H)] <- median(limiares$lineadd_file)
  xlimiar_line[limiares$lineadd_file > (qnt[2] + H)] <- median(limiares$lineadd_file)
  #boxplot(xlimiar_line)
  #summary(xlimiar_line)
  #length(xlimiar_line)  
  xlimiar_line <- as.data.frame(xlimiar_line) %>% mutate(sistema=as.character(logs$sistema[it]))
 # colnames(xlimiar_line) <- as.character(logs$sistema[it])
  
  #boxplot comparin lineadd/file (all file) and lineadd/file (each dev)  
  #boxplot(limiar_line , xlimiar_line, names = c("lineadd/file (all files)","lineadd/file (for each dev) "))    
  
  linhasadd <-rbind(linhasadd,xlimiar_line)
}

aux<- linhasadd
x<- aux %>% 
  mutate(ind = row_number()) %>% 
  spread(sistema, xlimiar_line, fill = NA)%>%
  mutate(ind=NA)


name <- paste ( "./graficos/pt/devs_linhasadd.pdf",sep = "" )
pdf ( name , width = 8 , height = 6 )
boxplot(x,	xlab="Desenvolvedores de interesse", ylab="Média de linhas adicionadas por arquivo") 
dev.off()
name <- paste ( "./graficos/en/devs_ativos.pdf",sep = "" )
pdf ( name , width = 8 , height = 6 )
boxplot(x,	xlab="Target developers", ylab="Mean added lines per file") 
dev.off()


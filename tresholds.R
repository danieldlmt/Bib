library(tseries)

# Outliers
  # Remove outliers from 'commits' and 'added lines' to better calculate tresholds
    # limiar_line - contains the mean number of added lines per file
    # xlimiar_line - contains the mean number of added lines per file of each dev

#lines per file vector (not considering devs)
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
    
#boxplot comparin lineadd/file (all file) and lineadd/file (each dev)  
#boxplot(limiar_line , xlimiar_line, names = c("lineadd/file (all files)","lineadd/file (for each dev) "))    



#funcao que nao sei fazer funcinar no r
#  remove_outliers <- function(x, na.rm = TRUE) {
#    qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
#    H <- 1.5 * IQR(x, na.rm = TRUE)
#    y <- x
#    y[x < (qnt[1] - H)] <- NA
#    y[x > (qnt[2] + H)] <- NA
#    y
#  }  
    rm(x,qnt,limiares)
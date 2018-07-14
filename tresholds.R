library(tseries)

# Outliers
  # Remove outliers from 'commits' and 'added lines' to better calculate tresholds
    # limiar_commit -  commits vector
    # limiar_line - contains the mean number of added lines per file of each dev  


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
#####################    
#####################
#the rest has not beeen used
###################
###################
#lines per commit vector
    x <- data %>%  
      select( platform, author,n_line_add,n_line_del,rev,path,date)%>%
      group_by(rev)%>%
      summarise(
        lineadd_commit = sum(n_line_add)/n()
      )
    limiares <- x %>% 
      ungroup() %>%
      select(lineadd_commit)
      
    qnt <- quantile(limiares$lineadd_commit, probs=c(.25, .75))
    H <- 1.5 * IQR(limiares$lineadd_commit, na.rm = TRUE)
    xlimiar_line <- limiares$lineadd_commit
    xlimiar_line[limiares$lineadd_commit < (qnt[1] - H)] <- median(limiares$lineadd_commit)
    xlimiar_line[limiares$lineadd_commit > (qnt[2] + H)] <- median(limiares$lineadd_commit)
    #boxplot(xlimiar_line)
    #summary(xlimiar_line)
    #length(xlimiar_line)
    
#lines per weeks for each dev vector
    x <- data %>%  
      select( platform, author,n_line_add,n_line_del,rev,path,date)%>%
      group_by(author) %>%
      summarise(n_line_add=sum(n_line_add),
                first=min(as.POSIXct(date)),
                last=max(as.POSIXct(date)),
                periodo = difftime(last,first,units="weeks")) %>%
      mutate( 
              nline_semana = n_line_add/as.numeric(periodo)
              )
    limiares <- x %>% 
      ungroup() %>%
      select(nline_semana)
    
    qnt <- quantile(limiares$nline_semana, probs=c(.25, .75))
    H <- 1.5 * IQR(limiares$nline_semana, na.rm = TRUE)
    xlimiar_line <- limiares$nline_semana
    xlimiar_line[limiares$nline_semana < (qnt[1] - H)] <- median(limiares$nline_semana)
    xlimiar_line[limiares$nline_semana > (qnt[2] + H)] <- median(limiares$nline_semana)
    #boxplot(xlimiar_line)
    #summary(xlimiar_line)
    #length(xlimiar_line)

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


    
    
#commits per dev
    x <- data %>%  
      select( platform, author,n_line_add,n_line_del,rev,path,date)%>%
      group_by(author)%>%
      summarise(
        commits = n_distinct(rev)
      )
    limiares <- x %>% 
      ungroup() %>%
      select(commits)
    
    qnt <- quantile(limiares$commits, probs=c(.25, .75))
    H <- 1.5 * IQR(limiares$commits, na.rm = TRUE)
    xlimiar_line <- limiares$commits
    xlimiar_line[limiares$commits < (qnt[1] - H)] <- median(limiares$commits)
    xlimiar_line[limiares$commits > (qnt[2] + H)] <- median(limiares$commits)
    #boxplot(xlimiar_line)
    #summary(xlimiar_line)
    #length(xlimiar_line)


#funcao que nao sei fazer funcinar no r
#  remove_outliers <- function(x, na.rm = TRUE) {
#    qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
#    H <- 1.5 * IQR(x, na.rm = TRUE)
#    y <- x
#    y[x < (qnt[1] - H)] <- NA
#    y[x > (qnt[2] + H)] <- NA
#    y
#  }  
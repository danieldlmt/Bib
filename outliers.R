library(tseries)

# Outliers
  # Remove outliers from 'commits' and 'added lines' to better calculate tresholds
    # limiar_commit - threshold for commits
    # limiar_line - threshold for added lines

# TODO: bug: Error: 'origin' must be supplied
platform_new <- data %>%
  select( platform, author,n_line_add,n_line_del,rev,path,date)%>%
  group_by(author,platform) %>%
  summarise(n_line_add=sum(n_line_add),
            n_line_del=sum(n_line_del), 
            commits=n_distinct(rev),
            files=n_distinct(path), 
            first=min(as.POSIXct(date)),
            last=max(as.POSIXct(date)),
            periodo = difftime(last,first,units="weeks")) %>%
  mutate( ind = if_else(platform == "Independente",1,0),
          n_line_semana = n_line_add/as.numeric(periodo),
          commit_semana= commits/as.numeric(periodo))

limiares <- platform_new %>% 
  ungroup() %>%
  select(n_line_add,commits)

qnt <- quantile(limiares$n_line_add, probs=c(.25, .75))
H <- 1.5 * IQR(limiares$n_line_add, na.rm = TRUE)
limiar_line <- limiares$n_line_add
limiar_line[limiares$n_line_add < (qnt[1] - H)] <- median(limiares$n_line_add)
limiar_line[limiares$n_line_add > (qnt[2] + H)] <- median(limiares$n_line_add)

qnt <- quantile(limiares$commits, probs=c(.25, .75), na.rm = TRUE)
H <- 1.5 * IQR(limiares$commits, na.rm = TRUE)
limiar_commit <- limiares$commits
limiar_commit[limiares$commits < (qnt[1] - H)] <- median(limiares$commits)
limiar_commit[limiares$commits > (qnt[2] + H)] <- median(limiares$commits)


#funcao que nao sei fazer funcinar no r
#  remove_outliers <- function(x, na.rm = TRUE) {
#    qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
#    H <- 1.5 * IQR(x, na.rm = TRUE)
#    y <- x
#    y[x < (qnt[1] - H)] <- NA
#    y[x > (qnt[2] + H)] <- NA
#    y
#  }  
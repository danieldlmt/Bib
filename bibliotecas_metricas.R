
library(dplyr, warn.conflicts = FALSE)
library(stringr)

# Extract the library metrics

# Create variable 'log'  that contains the paths of the scripts and workspaces (.RData)
#   source("./paths.R")
logs <- data.frame(sistema=c("Allegro", 
                             "SDL",
                             "SFML",
                             "Coco2dx", 
                             "Ogre3D", #cannot identify linux files
                             "Gameplay", #iphone do not have commits
                             "Godot"
),  
caminho = c("./log_allegro.R", 
            "./log_SDL.R", 
            "./log_SFML.R", 
            "./log_coco2dx.R", 
            "./log_ogre3d.R", 
            "./log_gameplay.R",
            "./log_godot.R"),
data = c("./workspace/allegro_data.RData", 
         "./workspace/SDL_data.RData", 
         "./workspace/SFML_data.RData", 
         "./workspace/coco2dx_data.RData", 
         "./workspace/ogre3d_data.RData", 
         "./workspace/gameplay_data.RData",
         "./workspace/godot_data.RData"))

metricas <- data.frame (  )

# Loop that walks through 'logs' to get .RData of each library
for (it in 1:length(logs$sistema)){
  # rm(list = ls()[!ls() %in% c("logs","it")])
  #source(as.character(logs$caminho[it]) )
  load (as.character(logs$data[it]))
  
  
  commits <- n_distinct(data$rev)
  devs<- n_distinct(data$author)
  source("./devsativos.R")
  ativos <- nrow(dev_ativo)
  ativos_porc <- ativos*100/devs
  ativos_lineadd_porc <- sum(dev_ativo$porc_line_add)
  ativos_commit_porc <- sum(dev_ativo$porc_commits)
  data_inicio <- min(as.POSIXct(data$date))
  data_ultimo <- max(as.POSIXct(data$date))
  periodo_anos <- as.numeric (difftime(max(as.POSIXct(data$date)),min(as.POSIXct(data$date)),units = "weeks") / 48)
  commits_anos <- commits/periodo_anos
  
  aux<- data.frame (  n_commits=commits, 
                      n_devs = devs, 
                      n_devs_ativos = ativos, 
                      porc_devs_ativos = ativos_porc,
                      porc_lineadd_devsativos = ativos_lineadd_porc,
                      porc_commits_devsativos = ativos_commit_porc, 
                      periodo_anos = periodo_anos, 
                      commit_anos = commits_anos,
                      data_inicio = data_inicio,
                      data_ultimo= data_ultimo)
  
  metricas <-rbind(metricas,aux)
  
}

row.names(metricas)<- logs$sistema

View(metricas)


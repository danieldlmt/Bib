
# log - Contains the paths of the scripts and workspaces (.RData)
# sistema - system name
# caminho - file that read the commit log .cvs
# data - file that keep .RData with the commit log after data cleaning and transformations
# ws analise - file that keep the results of the analise_v2.R, that does overall analysis in the commit log 
# ws_analisetemporal - deprecated
# ws_analisetemporaljanela - file that keep the results of the analisetemporaljanelas_v2.R, that does the analysis using sliding time windows in the commit log
logs <- data.frame(sistema=c("Allegro", 
                             "SDL",
                             "SFML",
                             "Coco2dx", 
                             # "Ogre3D", cannot identify linux files
                             # "Gameplay" iphone do not have commits
                             "Godot"
                  ),  
                  caminho = c("./log_allegro.R", 
                              "./log_SDL.R", 
                              "./log_SFML.R", 
                              "./log_coco2dx.R", 
                              # "./log_ogre3d.R", 
                              # "./log_gameplay.R",
                              "./log_godot.R"),
                  data = c("./workspace/allegro_data.RData", 
                           "./workspace/SDL_data.RData", 
                           "./workspace/SFML_data.RData", 
                           "./workspace/coco2dx_data.RData", 
                           # "./workspace/ogre3d_data.RData", 
                           # "./workspace/gameplay_data.RData",
                           "./workspace/godot_data.RData"),
                  ws_analise=c("./workspace/analise_allegro.RData", 
                               "./workspace/analise_SDL.RData", 
                               "./workspace/analise_SFML.RData", 
                               "./workspace/analise_coco2dx.RData",
                               # "./workspace/analise_ogre3d.RData",
                               # "./workspace/analise_gameplay.RData",
                               "./workspace/analise_godot.RData"), 
                  ws_analisetemporal = c("./workspace/analisetemporal_allegro.RData", 
                                         "./workspace/analisetemporal_SDL.RData", 
                                         "./workspace/analisetemporal_SFML.RData", 
                                         "./workspace/analisetemporal_coco2dx.RData",
                                         # "./workspace/analisetemporal_ogre3d.RData",
                                         # "./workspace/analisetemporal_gameplay.RData",
                                         "./workspace/analisetemporal_godot.RData"), 
                  ws_analisetemporaljanela = c("./workspace/analisetemporaljanela_allegro.RData", 
                                               "./workspace/analisetemporaljanela_SDL.RData", 
                                               "./workspace/analisetemporaljanela_SFML.RData", 
                                               "./workspace/analisetemporaljanela_coco2dx.RData",
                                               # "./workspace/analisetemporaljanela_ogre3d.RData",
                                               # "./workspace/analisetemporaljanela_gameplay.RData",
                                               "./workspace/analisetemporaljanela_godot.RData")  )
                  

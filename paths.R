
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
                  caminho = c("~/bib/log_allegro.R", 
                              "~/bib/log_SDL.R", 
                              "~/bib/log_SFML.R", 
                              "~/bib/log_coco2dx.R", 
                              # "~/bib/log_ogre3d.R", 
                              # "~/bib/log_gameplay.R",
                              "~/bib/log_godot.R"),
                  data = c("~/bib/workspace/allegro_data.RData", 
                           "~/bib/workspace/SDL_data.RData", 
                           "~/bib/workspace/SFML_data.RData", 
                           "~/bib/workspace/coco2dx_data.RData", 
                           # "~/bib/workspace/ogre3d_data.RData", 
                           # "~/bib/workspace/gameplay_data.RData",
                           "~/bib/workspace/godot_data.RData"),
                  ws_analise=c("~/bib/workspace/analise_allegro.RData", 
                               "~/bib/workspace/analise_SDL.RData", 
                               "~/bib/workspace/analise_SFML.RData", 
                               "~/bib/workspace/analise_coco2dx.RData",
                               # "~/bib/workspace/analise_ogre3d.RData",
                               # "~/bib/workspace/analise_gameplay.RData",
                               "~/bib/workspace/analise_godot.RData"), 
                  ws_analisetemporal = c("~/bib/workspace/analisetemporal_allegro.RData", 
                                         "~/bib/workspace/analisetemporal_SDL.RData", 
                                         "~/bib/workspace/analisetemporal_SFML.RData", 
                                         "~/bib/workspace/analisetemporal_coco2dx.RData",
                                         # "~/bib/workspace/analisetemporal_ogre3d.RData",
                                         # "~/bib/workspace/analisetemporal_gameplay.RData",
                                         "~/bib/workspace/analisetemporal_godot.RData"), 
                  ws_analisetemporaljanela = c("~/bib/workspace/analisetemporaljanela_allegro.RData", 
                                               "~/bib/workspace/analisetemporaljanela_SDL.RData", 
                                               "~/bib/workspace/analisetemporaljanela_SFML.RData", 
                                               "~/bib/workspace/analisetemporaljanela_coco2dx.RData",
                                               # "~/bib/workspace/analisetemporaljanela_ogre3d.RData",
                                               # "~/bib/workspace/analisetemporaljanela_gameplay.RData",
                                               "~/bib/workspace/analisetemporaljanela_godot.RData")  )
                  

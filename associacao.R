
# Platform association 

plats<- read.csv("./data/plats.csv")
plats<- plats %>% select(palavra,plataforma)

for (i in 1:dim(plats)[1]){
  #print(plats[i,1]) 
  data$platform <- ifelse(data$platform=="Independente" & grepl(plats[i,1],data$path),paste("", plats[i,2], sep=""),data$platform)
}

data <- data %>% filter(platform=="Independente" |platform=="Windows"|platform=="Linux"|platform=="macOS"|platform=="Android"|platform=="iPhone" )

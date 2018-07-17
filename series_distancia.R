library("TSdist")
# documentacao TSdist
# https://journal.r-project.org/archive/2016/RJ-2016-058/RJ-2016-058.pdf
# https://cran.r-project.org/web/packages/TSdist/TSdist.pdf
 load("./workspace/seriesagrupadas_allegro.RData")
 
 
 # probabiblity vector
 x<-saida_plat_bind%>% 
   filter(prob==0.30)%>%
   filter(tipo=="gen")
 aux<-data.frame(iteracao=c(1:max(x$iteracao,y$iteracao)))#igualar tamanho do vetor
 x<-right_join(x,aux,by="iteracao",rm.na=TRUE)
 rm(aux)
 x<-x%>% mutate(porc= if_else(is.na(porc),0,porc) )#substituir NAs
 
 y<-saida_plat_bind%>% 
   filter(prob==0.30)%>%
   filter(tipo=="esp")
 aux<-data.frame(iteracao=c(1:max(x$iteracao,y$iteracao)))#igualar tamanho do vetor
 y<-right_join(y,aux,by="iteracao",rm.na=TRUE)
 rm(aux)
 y<-y%>% mutate(porc= if_else(is.na(porc),0,porc) )#substituir NAs
 
 
 
 
#calcular distancia de  duas series 
# Computes the distance measure based on the cross-correlation between a pair of numeric time series.
CCorDistance(x$porc,y$porc)
# Computes two different distance measure based on Pearson’s correlation between a pair of numeric time series of the same length.
CorDistance(x$porc,y$porc)
# Computes the Dissim distance between a pair of numeric series.
DissimDistance(x$porc,y$porc) #nao funciona
# Computes the Dynamic Time Warping distance between a pair of numeric time series.
# do not nedd same size
DTWDistance(x$porc,y$porc)


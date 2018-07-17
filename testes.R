 pdf('rplot.pdf')

 esp <- esp_bind2%>% filter(prob==0.1)  
 gen <-gen_bind2%>% filter(prob==0.1)  
 
 par(mar=c(5.1, 5, 4.1, 2.1)) 
 plot(esp$iteracao,esp$porc,col="red",type = "l",ylim = c(0,100),xlim=c(1,j), xlab = "Tempo (semanas)", ylab = "% de desenvolvedores",cex.names = 1.7,cex.axis = 1.7,cex.lab=1.7)
 lines(gen$iteracao,gen$porc,col="green",type = "l" )
 legend("top",c("Especialista", "Generalista"), col=c("red","green"),lty=1)
 

dev.off()




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

# documentacao TSdist
# https://journal.r-project.org/archive/2016/RJ-2016-058/RJ-2016-058.pdf
# https://cran.r-project.org/web/packages/TSdist/TSdist.pdf

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






x<-saida_plat_bind%>% 
  filter(prob==0.20)%>%
  filter(tipo=="gen")
lines(x$iteracao,x$porc,col="red")
x<-saida_plat_bind%>% 
  filter(prob==0.30)%>%
  filter(tipo=="gen")
plot(x$iteracao,x$porc,col="orange", type="l")
x<-saida_plat_bind%>% 
  filter(prob==0.40)%>%
  filter(tipo=="gen")
lines(x$iteracao,x$porc,col="pink")
x<-saida_plat_bind%>% 
  filter(prob==0.50)%>%
  filter(tipo=="gen")
lines(x$iteracao,x$porc,col="green")
x<-saida_plat_bind%>% 
  filter(prob==0.60)%>%
  filter(tipo=="gen")
lines(x$iteracao,x$porc,col="blue")
legend("topright",c("0.1", "0.2", "0.3","0.4","0.5","0.6"), col=c("black","red", "orange","pink","green","blue"),lty=1)

# probabiblity vector
x<-saida_plat_bind%>% 
  filter(prob==0.10)%>%
  filter(tipo=="esp")
plot(x$iteracao,x$porc,col="black")
x<-saida_plat_bind%>% 
  filter(prob==0.20)%>%
  filter(tipo=="esp")
lines(x$iteracao,x$porc,col="red")
x<-saida_plat_bind%>% 
  filter(prob==0.30)%>%
  filter(tipo=="esp")
lines(x$iteracao,x$porc,col="orange")
x<-saida_plat_bind%>% 
  filter(prob==0.40)%>%
  filter(tipo=="esp")
lines(x$iteracao,x$porc,col="pink")
x<-saida_plat_bind%>% 
  filter(prob==0.50)%>%
  filter(tipo=="esp")
lines(x$iteracao,x$porc,col="green")
x<-saida_plat_bind%>% 
  filter(prob==0.60)%>%
  filter(tipo=="esp")
lines(x$iteracao,x$porc,col="blue")
legend("topright",c("0.1", "0.2", "0.3","0.4","0.5","0.6"), col=c("black","red", "orange","pink","green","blue"),lty=1)


# probabiblity vector
x<-saida_device%>% 
  filter(prob==0.10)%>%
  filter(tipo=="desktop mobile")
plot(x$iteracao,x$porc,col="black")
x<-saida_device%>% 
  filter(prob==0.20)%>%
  filter(tipo=="desktop mobile")
lines(x$iteracao,x$porc,col="red")
x<-saida_device%>% 
  filter(prob==0.30)%>%
  filter(tipo=="desktop mobile")
lines(x$iteracao,x$porc,col="orange")
x<-saida_device%>% 
  filter(prob==0.40)%>%
  filter(tipo=="desktop mobile")
lines(x$iteracao,x$porc,col="pink")
x<-saida_device%>% 
  filter(prob==0.50)%>%
  filter(tipo=="desktop mobile")
lines(x$iteracao,x$porc,col="green")
x<-saida_device%>% 
  filter(prob==0.60)%>%
  filter(tipo=="desktop mobile")
lines(x$iteracao,x$porc,col="blue")
legend("topright",c("0.1", "0.2", "0.3","0.4","0.5","0.6"), col=c("black","red", "orange","pink","green","blue"),lty=1)

# probabiblity vector
x<-saida_device%>% 
  filter(prob==0.10)%>%
  filter(tipo=="desktop")
plot(x$iteracao,x$porc,col="black")
x<-saida_device%>% 
  filter(prob==0.20)%>%
  filter(tipo=="desktop")
lines(x$iteracao,x$porc,col="red")
x<-saida_device%>% 
  filter(prob==0.30)%>%
  filter(tipo=="desktop")
lines(x$iteracao,x$porc,col="orange")
x<-saida_device%>% 
  filter(prob==0.40)%>%
  filter(tipo=="desktop")
lines(x$iteracao,x$porc,col="pink")
x<-saida_device%>% 
  filter(prob==0.50)%>%
  filter(tipo=="desktop")
lines(x$iteracao,x$porc,col="green")
x<-saida_device%>% 
  filter(prob==0.60)%>%
  filter(tipo=="desktop")
lines(x$iteracao,x$porc,col="blue")
legend("topright",c("0.1", "0.2", "0.3","0.4","0.5","0.6"), col=c("black","red", "orange","pink","green","blue"),lty=1)


# probabiblity vector
x<-saida_device%>% 
  filter(prob==0.10)%>%
  filter(tipo=="mobile")
plot(x$iteracao,x$porc,col="black")
x<-saida_device%>% 
  filter(prob==0.20)%>%
  filter(tipo=="mobile")
lines(x$iteracao,x$porc,col="red")
x<-saida_device%>% 
  filter(prob==0.30)%>%
  filter(tipo=="mobile")
lines(x$iteracao,x$porc,col="orange")
x<-saida_device%>% 
  filter(prob==0.40)%>%
  filter(tipo=="mobile")
lines(x$iteracao,x$porc,col="pink")
x<-saida_device%>% 
  filter(prob==0.50)%>%
  filter(tipo=="mobile")
lines(x$iteracao,x$porc,col="green")
x<-saida_device%>% 
  filter(prob==0.60)%>%
  filter(tipo=="mobile")
lines(x$iteracao,x$porc,col="blue")
legend("topright",c("0.1", "0.2", "0.3","0.4","0.5","0.6"), col=c("black","red", "orange","pink","green","blue"),lty=1)

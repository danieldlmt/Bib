 pdf('rplot.pdf')

 esp <- esp_bind2%>% filter(prob==0.1)  
 gen <-gen_bind2%>% filter(prob==0.1)  
 
 par(mar=c(5.1, 5, 4.1, 2.1)) 
 plot(esp$iteracao,esp$porc,col="red",type = "l",ylim = c(0,100),xlim=c(1,j), xlab = "Tempo (semanas)", ylab = "% de desenvolvedores",cex.names = 1.7,cex.axis = 1.7,cex.lab=1.7)
 lines(gen$iteracao,gen$porc,col="green",type = "l" )
 legend("top",c("Especialista", "Generalista"), col=c("red","green"),lty=1)
 

dev.off()

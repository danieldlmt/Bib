plot (dev_all_time24_new_genmedio$iteracao,dev_all_time24_new_genmedio$porcentage,type="l",pch=20, ylim=c(0,100), col="black",xlab = "Tempo (semanas)", ylab = "% de generalistas")
lines(dev_all_time24_new_genmuito$iteracao,dev_all_time24_new_genmuito$porcentage, col="green",type="l",pch=20)
lines(dev_all_time24_new_espmedio$iteracao,dev_all_time24_new_espmedio$porcentage, col="red",type="l",pch=20)
lines(dev_all_time24_new_espmuito$iteracao,dev_all_time24_new_espmuito$porcentage, col="orange",type="l",pch=20)

legend("topright",main="Generalistas", legend=c("médio", "muito"),col=c("black", "green"),lty=1,inset = 0.02)
legend("topleft",main="Generalistas", legend=c("médio", "muito"),col=c("red", "orange"),lty=1,inset = 0.02)



g <- ggplot(dev_all_time24_new_hist, aes(iteracao)) + scale_fill_brewer(palette = "Spectral")
g + geom_histogram(aes(fill=nivel),col="black", size=.1) 

g <- ggplot(devicetype_time24_new_hist, aes(iteracao)) + scale_fill_brewer(palette = "Spectral")
g + geom_histogram(aes(fill=nivel2),col="black", size=.1) 

g <- ggplot(devicetype_time24_new_hist2, aes(iteracao)) + scale_fill_brewer(palette = "Spectral")
g + geom_histogram(aes(fill=nivel3),col="black", size=.1) 

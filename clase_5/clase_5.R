#Cipriano Guerrero Cabrera
#clase 5
#09-08-2019

# diferencia entre tres variables analisis de varianza-----------------------------------------


#ho no existe diferencia entre tratamientos 
#h1 existe diferencia entre tratamientos

arena<- c(6, 10, 8, 6, 14, 17, 9, 11, 7, 11)
arcilla<- c(17, 15, 3, 11, 14, 12, 12, 8, 10, 13)
limo<-c(13, 16, 9, 12, 15, 16, 17, 13, 18, 14)
y.ton<-c(arena, arcilla, limo)
suelo <-gl(3, 10, 30, labels=c("arena", "arcilla","limo"))
prod<-data.frame(suelo, y.ton)
head(prod)
tapply(prod$y.ton,prod$suelo,mean)
tapply(prod$y.ton,prod$suelo,var)
shapiro.test(prod$y.ton)
# los valores son normales para 

bartlett.test(prod$y.ton, prod$suelo)
fligner.test(prod$y.ton, prod$suelo)

boxplot(prod$y.ton~prod$suelo, xlab="Tipos de suelo",
        ylab="ton/ha",col="blue")

aov.suelo<-aov(prod$y.ton~prod$suelo)
aov.suelo
summary(aov.suelo)
par(mfrow=c(2,2))
plot(aov(prod$y.ton~prod$suelo))
par(mfrw=c(1,1))

TukeyHSD(aov.suelo, conf.level = 0.95)

plot(TukeyHSD(aov.suelo))
summary(aov.suelo)

summary.lm(aov.suelo)









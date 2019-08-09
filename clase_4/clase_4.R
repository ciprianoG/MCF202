#Cipriano Guerrero Cabrera
#09/08/
#clase_4

erupciones <- read.csv("C:/MCF 202-2019/MCF202/Datos/erupciones.csv")
head(erupciones)
plot(log(erupciones$eruptions), log(erupciones$waiting), pch=19, col="blue", 
     xlab= "tiempo de espera (min)")

library(pastecs)
stat.desc(erupciones$eruptions, basic=FALSE, norm=TRUE)
shapiro.test(erupciones$eruptions)
shapiro.test(erupciones$waiting)

cor.test(erupciones$eruptions, erupciones$waiting)


# regrecion lineal --------------------------------------------------------

# h0. no es sinifivaticva la prediccion de las erupciones 
# h1. es significativa kla predioccion de las erupciones.
#el comando (lm) para realizar la regrecion lineal

lm.erup<-lm(erupciones$eruptions~erupciones$waiting)

plot(erupciones$waiting, erupciones$eruptions, pch=19, col="blue", 
     xlab= "tiempo de espera (min)",
     ylab= "duracion(min)")
#en la grafica se pone la la variable dependiente  "x" y de la independiente "y"
abline(lm.erup, col="red")
text(52, 4.5, "y = -1.87 + 0.07*x")
#este comando es para poner liea en la grafica plot
text(52, 4, "r^2 = 0.81")

lm.erup

summary(lm.erup)

#los valores residuales es la diferencia entre el valor observado y el valor predicho
length(erupciones$eruptions)

y.60<- -1.87 + 0.07*60 

y.60

# datos de regrecipon -----------------------------------------------------
espera<-erupciones$waiting
duracion<-erupciones$eruptions
res<-resid(lm.erup)
pre<- fitted(lm.erup)
res.2<-res^2

cuadro<- round(data.frame(espera, duracion, pre, res,res.2)^2)

sse<- sum((duracion - pre)^2)
sse
vari<- sse/(length(erupciones$waiting)-2)
vari

#esta varianza es del modelo que estamos estimando


# prueba de hipotesis de la regrecion -------------------------------------

an.erup<-anova(lm.erup)
an.erup
#se hacepta la hipotesis alternativa en el modelo lineal esto quiere decir que 
#las erupciones son


# ejercicio 2 ebanos ------------------------------------------------------

ebano <- read.csv("C:/MCF 202-2019/MCF202/Datos/ebanos.csv")
head(ebano)
plot(ebano$altura, ebano$diametro, pch=19, col="red", 
     xlab = "Diametro (cm)", 
     ylab = "Altura (m)")
library(pastecs)
stat.desc(ebano$diametro, basic = FALSE, norm = TRUE)
stat.desc(ebano$altura, basic = FALSE, norm = TRUE)

shapiro.test(ebano$diametro)
shapiro.test(ebano$altura)


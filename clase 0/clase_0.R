# Cipriano Guerrero Cabrera
# 05/08/2019
# clase 0


# # Pasos Basicos ---------------------------------------------------------

2+2
a <- 2
a*a
a+5


# # importar datos --------------------------------------------------------


diametro <- c(12, 8.6, 9.2, 7.7, 12.9, 11.7, 9.7, 14.2,
              11.8, 14.3, 12.5)

diametro
# medidas de tendencia central
mean(diametro)
median(diametro)
# medidas de dispercion
sd(diametro)
var(diametro)


# boxplot (diametro) ------------------------------------------------------

boxplot(diametro, horizontal=TRUE,  col="lightblue", main="diametro",
        xlab="D (cm)")


# importar excel ----------------------------------------------------------

DB_alturas <- read.csv("C:/MCF 202-2019/Datos/alturas.csv", header= T)
head(DB_alturas)

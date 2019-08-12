#cipriano guerrero cabrera
#clase 6

# instalar libreria gabminder ---------------------------------------------

library(repmis)
edad <- source_data("https://www.dropbox.com/s/nxoijhgmutuho0s/datos_control_Rascon.csv?dl=1")
edad$SP<-factor(edad$SP)
str(edad)


# separar factor ----------------------------------------------------------

ariz<- subset(edad,SP=="arizonica")

ariz.lm<-lm(ariz$EDAD~ariz$DAP)

dura<-subset(edad,SP=="durangensis")


# regresion dos factores --------------------------------------------------

cov.edad<-lm(edad$EDAD~edad$DAP+edad$SP)
summary(cov.edad)

plot(edad$DAP[edad$SP=="arizonica"],edad$EDAD[edad$SP=="arizonica"],
       col="red", pch=16, xlim=c(0,50), ylim = c(0,130))

abline(cov.edad$coefficients[1],cov.edad$coefficients[2], col="blue")

text(30,20,"Ya=-7.65+1.98*x")
points(edad$DAP[edad$SP == "durangencis"], edad$EDAD[edad$SP == "surangencis"],
       col= "blue", pch="d")
abline(cov.edad$coefficients[1]+cov.edad$coefficients[3],
       cov.edad$coefficients[2],col="blue", lty="dashed")
text(19,100, "Yd=11.41+1.98")

  
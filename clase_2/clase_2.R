#cipriano guerrero cabrera
#06/08/2019
#clase 2

# importar datos viveros --------------------------------------------------

vivero <- read.csv("C:/MCF 202-2019/MCF202/Datos/Clase2.csv", header = T)
summary(vivero)

# prueba de t una muestra ----------------------------------------------
par(mfrow=c(1,1))
boxplot(vivero$IE)
t.test(vivero$IE, mu = 0.85)
#la media observada no es diferente estadisticamente ya que el valor de p
#es mayor que el alfa establecido(0.05). ademas la media teoretica se
#encuentra dentro del rango del rango de los valores de intervalo sde confianza.
t.test(vivero$IE, mu = 0.90)
#la media observada es diferenre estadisticamente ya que el valor de p (0.01)
#es menor que el valor de alfa establecido (0.05).por lo tanto se acepta H1.


# pruebas de t muestras independientes ------------------------------------

boxplot(vivero$IE ~ vivero$Tratamiento, col="red",  xlab="tratamineto", 
        ylab="IE")
shapiro.test(vivero$IE)

var.test(vivero$IE ~ vivero$Tratamiento)
#la varianza de ambos tratamientos son igual asi lo prueba el valor de p
#obtenido mediante una prueba de varianza (var.test).

t.test(vivero$IE ~ vivero$Tratamiento, var.ecual= T)

#existe una diferencia entre el IE de las plantulas fertilizadas
#el valor p comprueba nuestra hipotesis de que el fertilizante 
#"power# mejora el IE


# inportar datos produccion -----------------------------------------------

inventario <- read.csv("C:/MCF 202-2019/MCF202/Datos/produccion.csv", header = T)
summary(inventario)
boxplot(inventario$Kgsem ~ inventario$Tiempo)
t.test(inventario$Kgsem ~ inventario$Tiempo, paired = T)

tapply(inventario$Germ, inventario$Tiempo, mean)

boxplot(inventario$Germ ~ inventario$Tiempo)

t.test(inventario$Germ ~ inventario$Tiempo, paired = T)


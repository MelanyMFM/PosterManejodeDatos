#Por simetría, P(Z>-x) = P(Z<x) y P(Z<x) + P(Z>x) = 1
#Letra_numero: P(letra>q) = numero
#d = f(x) (altura), p= F(x) (area a la izquierda de q), q = P(X<q) = p
#fitdistr() sirve para hallar los parámetros de la distribución de un conjunto de datos
#recibe x=vector con los datos; desfun=funcion a hallar parámetros, ej: "chi-squared"
#Los valores entre paréntesis que salen se llaman “errores estándar”, son la variabilidad que tiene la estimación.
#Para gráficar plot(density(x)), x = rnorm o vector; plot(y=dbinom)
#para verificar normalidad: qqnorm() y qqline()
#para bandas en los gráficos de normalidad  usar: require(CAR) y qqPlot(vector)
#para pruebas de normalidad como ad.test usar paquete nortest
#si los valores arrojados son mayor a un nivel de significancia, no rechazar Normalidad, de lo contrario si
#library(gamlss) para a <- fitDist(vector) e imprimir a$fits, el valor más pequeño es el más acertado
#usar t.test() si se desconoce desviación estándar
library(MASS)
if (!require('shiny')) install.packages("shiny")
shiny::runGitHub(repo="semilleroApps", user="fhernanb", sub="samplesize") #usar app para calcular tamaño de muestra

x <- c(39, 54, 61, 72, 59)
t.test(x=x, conf.level = 0.9)$conf.int #intervalo para la media sin conocer sd

x_bar <- mean(x)
y <- qnorm(0.05/2, lower.tail = F)*#desvi_pobl/sqrt(n)
rta <- x_bar - y #intervalo para la media conociendo sd

stests::var.test(x=x, conf.level=0.99)$conf.int #intervalo de confianza para la varianza

prop.test(x=54, n=120, conf.level=0.90)$conf.int #intervalo de confianza para proporciones

z <- c(34, 36, 39, 31, 33, 26, 45, 34, 39, 38, 37, 36)
a <- c(33, 41, 39,32, 29, 28, 33, 34, 25, 28, 36, 33, 35, 35)
stests::var.test(x=z, y=a, conf.level=0.95)$conf.int #intervalo de confianza para cociente de varianzas
t.test(x=z, y=a, paired=FALSE, var.equal=T, conf.level=0.95)$conf.int #intervalo confianza para diferencia de medias
prop.test(x=c(825, 760), n=c(1000, 1000), conf.level=0.95)$conf.int #intervalo confianza para diferencia de proporciones
prop.test(x=c(10, 40), n=c(100, 200), conf.level = 0.95)$conf.int
t.test(x=c(73, 77, 68, 62, 72, 80, 76, 64, 70, 72), y=c(68, 72, 64, 60, 71, 77, 74, 60, 64, 68), paired=T, var.equal=F, conf.level=0.95)$conf.int
#intervalo de confianza para diferencia de medias pareadas
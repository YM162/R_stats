
#1.3. El conjunto de datos 168Perros.txt contiene medidas de los niveles de glucosa (en mg/dl)
#y alanina aminotransferasa (ALT, U/l) en una muestra de 168 perros aparentemente sanos

datos = read.table("168Perros.txt",header=TRUE)

#a) Dibuja un histograma de la variable ALT e indica razonadamente si los datos se podrian modelizar
#mediante una distribucion normal. Dibuja despues un histograma del logaritmo de dicha variable y
#responde a la misma pregunta.

hist(datos$ALT)
#Estos datos no se corresponden a una distribución normal, ya que no es una distribución simétrica,
#se trata de una lognormal.
hist(log(datos$ALT))
#Al analizar el logarítmo de los datos sí encontramos una distribución que se ajusta a la del modelo
#normal: Simétrica, con frecuencias que descienden rápidamente a medida que se alejan del centro.


#b) Si quisieramos ajustar un modelo N(µ,σ) a log(ALT), ¿como estimariamos los valores de los
#parametros µ y σ? Dibuja la densidad normal con los parametros estimados sobre el histograma
#para comprobar la bondad del ajuste.

#los parametros µ y σ, la media y desviación tipica se pueden obtener mediante las funciones mean()
#y una modificación de la función sd()
logALT = log(datos$ALT)
m = mean(logALT);m
#4.08
n=length(logALT)
SD = sd(logALT)*sqrt((n-1)/n);SD
#0.4745
#Para dibujarlo, primero generamos un histograma con frecuencias relativas (La suma de frecuencias es 1)
hist(logALT, freq=FALSE)
#Utilizamos las funciones seq, dnorm y lines para trazar la distrubución normal con la media y
#desviación típica de nuestra variable logALT
puntos = seq(m-3*SD,m+3*SD,0.1)
densidad = dnorm(puntos,m,SD)
lines(puntos,densidad,col="red")
#Podemos comprobar que en general el ajuste es bueno, y el modelo nos servirá.


#c) Bajo la hipotesis de normalidad de (b), ¿que probabilidad hay de que un perro sano tenga un
#nivel de log(ALT) superior a 4?. Compara esta probabilidad con la frecuencia observada en los datos.

#Hallamos la probabilidad usando pnorm()
prob = pnorm(4,m,SD) ; 1-prob
#La probabilidad de ser superior a 4 será de 0.56.
(1-prob)*n
length(which(logALT>4))
#La predicción hayando la probabilidad con el modelo normal es de 95.27, y el número real es de 92,
#por lo que el modelo es una buena aproximación.


#d) Supongamos que el logaritmo de la alanina aminotransferasa, log(ALT), sigue una distribucion
#normal. Calcular un intervalo de confianza al 95 % para la media y la varianza de esta distribucion.

#Queremos un intervalo de confianza del 95% en la media por lo que:
#Para 1-alpha=0.95 calculamos el margen de error.
alpha=1-0.95; alpha
z = qnorm(alpha/2,lower.tail=FALSE);z
#z=1.96
E=z*SD/sqrt(n)
#En nuestro caso el margen de error es 0.0717, por lo que nuestro intervalo estará delimitado por estos
#dos valores:
inferiorm = m-E
superiorm = m+E
c(inferior=inferiorm ,media=m ,superior=superiorm)
#inferior    media superior 
#4.008383 4.080134 4.151884

#Para el intervalo de confianza del 95% de la varianza:
var=var(logALT)
#gl siendo los grados de libertad
gl=n-1
#A partir de la formula obtenemos:
inferiorv=gl*var/qchisq(alpha/2,gl,lower.tail=FALSE)
superiorv=gl*var/qchisq(1-alpha/2,gl,lower.tail=FALSE)
c(inferior=inferiorv ,varianza=var ,superior=superiorv)
#inferior  varianza  superior 
#0.1848029 0.2264940 0.2841585 




p1=31706752/61890823;p1
num1=31706752
n1=61890823;p2
p2=19727605/38360531
num2=19727605
n2=38360531
alpha=0.05; alpha
z = qnorm(1-alpha,lower.tail=FALSE);z
p=(n1*p1+n2*p2)/(n1+n2)
R=p1-p2<z*sqrt(p*(1-p)*((1/n1)+(1/n2)));R









prop.test(c(num1,num2),c(n1,n2),alternative="less",correct=FALSE)


Datos = read.table("oilvit.txt",header=TRUE) 
Aceite = Datos$type 
Maiz = (Aceite == "corn") 
X = Datos[Maiz,2] 
Y = Datos[!Maiz,2] 
mean(X) 
mean(Y) 
nx=10
ny=10
var(X) 
var(Y) 
t.test(X,Y,var.equal=TRUE) 


var.test(X,Y,alternative="t")




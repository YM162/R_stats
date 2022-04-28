                       #Pues eso, funciones útiles y bien preparaditas



                   #Primera pregunta - Tema 1 - Estadística descriptiva
#Una sola variable
x=c()
n=length(x);n
mean(x)
median(x)
quantile(x,0.25)

#Cuasivarianza (s^2)
var(x)
#Varianza (sigma^2)
var(x)*(n-1)/n

#Cuasidesviación tipica (s)
sd(x)
#Desviación típica (sigma)
sd(x)*sqrt((n-1)/n)

#Estandarizar
z=(x-mean(x))/(var(x)*(n-1)/n);z

#Dos variables
x=c()
y=c()

#Diagrama de dispersión
plot(x,y)

#covarianza
cov(x,y)

#Regresión lineal
lm(x ~ y)

#Correlación lineal
cor(x,y)





               #Segunda pregunta - Tema 2 - Distribuciones de probabilidad

#Probabilidad dejada a la izq de x (Normal)
pnorm(137,140,3,lower.tail=TRUE)

pnorm(145,140,3,lower.tail=TRUE)-pnorm(137,140,3,lower.tail=TRUE)

pnorm(6,7.95,2.58,lower.tail=TRUE)

#número z que deja ese porcentaje de números a su izquierda (Normal)
z=qnorm(0.22,m,sd);z

#Probabilidad dejada a la izq de x (Exponencial)
pexp(1.56,lower.tail=TRUE)
#número z que deja ese porcentaje de números a su izquierda (Exponencial)
z=qexp(0.67);z

#Probabilidad dejada a la izq de x (Binomial)
pbinom(6,50,0.159,lower.tail=TRUE)

pbinom(85,100,0.794,lower.tail=TRUE)-pbinom(75,100,0.794,lower.tail=TRUE)

#número z que deja ese porcentaje de números a su izquierda (Binomial)
z=qbinom(0.8849,n,p);z


                  #Tercera pregunta - Tema 3 - Estimación puntual
#Estimador de la media ()
x=c()
n=length(x)
mean.est=sum(x)/n ; mean.est

#Estimador de la cuasivarianza (s^2)
cuasivar.est=sum((x-mean.est)^2)/(n-1) ; cuasivar.est


                 #Tercera pregunta - Tema 4 - Intervalos de confianza
#Normal varianza conocida(media)   IC=(1-alpha)
alpha=  ;aplha
x=c()
mean=
sd=
n=
E=qnorm(alpha/2,lower.tail=FALSE)*sd/sqrt(n)

#Normal varianza desconocida(media)
alpha=  ;aplha
x=c()
mean=
cuasisd.est=sqrt(sum((x-mean.est)^2)/(n-1))
n=
E=qt(alpha/2,n-1,lower.tail=FALSE)*cuasisd.est/sqrt(n)

#Normal (varianza)
alpha=  ;aplha
x=c()
mean=
cuasisd.est=sqrt(sum((x-mean.est)^2)/(n-1))
n=
#Limite inferior del IC
(n-1)*cuasisd.est/qchisq(alpha/2,n-1,lower.tail=FALSE)
#Limite superior del IC
(n-1)*cuasisd.est/qchisq((1-alpha)/2,n-1,lower.tail=FALSE)

  E=qt(alpha/2,n-1,lower.tail=FALSE)*cuasisd.est/sqrt(n)

#Poisson de muestras grandes (TCL)
alpha=  ;aplha
x=c()
mean=
n=
E=qpois(alpha/2,lower.tail=FALSE)*sqrt(mean/n)

#Binomial de muestras grandes (TCL)
alpha=  ;aplha
x=c()
mean=
n=
E=qbinom(alpha/2,lower.tail=FALSE)*sqrt((mean*(1-mean))/n)

#Intervalo de igualdad de varianzas
alpha=0.05  ;alpha
x1=c()
x2=c()
mean.est1=
mean.est2=
n1=20
n2=21
cuasisd.est.x1=2581.168
cuasisd.est.x2=2341.957

#Limite inferior
(cuasisd.est.x1/cuasisd.est.x2)/qf(alpha/2,n1-1,n2-1,lower.tail=FALSE)
#Limite superior
(cuasisd.est.x1/cuasisd.est.x2)*qf(alpha/2,n2-1,n1-1,lower.tail=FALSE)


#Intervalo de confianza de p1-p2, si contiene 0 es verda
mean1=
n1=
mean2=
n2=
  
I=(qnorm(alpha/2,lower.tail = FALSE)*sqrt((mean1(1-mean1)/n1)+(mean2(1-mean2)/n2)))





                #Tercera pregunta - Tema 5 - Contrastes de hipótesis

#Población normal con varianza desconocida         two.sided,less,greater
x=c()
mean=
t.test(x, alternative=,mu=mean)
#Si el p-valor es más pequeño que alpha, se demuestra que H0 es falsa y H1 verdadera

#Población binomial         two.sided,less,greater
binom.test(x,n,p)

#Población poisson         two.sided,less,greater
poisson.test(L)

#Comparar proporciones         two.sided,less,greater
num1=
n1=
num2=
n2=

prop.test(c(num1,num2),c(n1,n2),alternative=,correct=FALSE)

                
                



X = c(0.55, 0.15, 0, 0.13, 0.26, 0.07, 0.2, 0.16, 0.03, 0.42)
Y = c(0.14, 0.08, 0, 0.13, 0.1, 0.08, 0.11, 0, 0.05, 0.21)
cov(X,Y)
plot(X,Y)     



C=c(272,283,316,321,329,345,349,350,356,356,360,366,380,384,399,402,410,431,455,462)

M=c(290,318,326,339,361,375,392,393,401,403,406,407,410,420,426,427,430,434,447,467,477)
summary(C
        )
summary(M)



boxplot(C,M)

s2m=2341.957
s2c=2581.168
n1=21
n2=20
sqrt(((n1-1)*s2m+(n2-1)*s2c)/(n1+n2-2))


qt(0.05,39,lower.tail = FALSE)

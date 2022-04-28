#Ejercicio 4
Poco = scan("AcnePocoSHBG.txt")
Severo = scan("AcneSeveroSHBG.txt")

mean(Poco)
mean(Severo)

quantile(Poco,0.5)
quantile(Severo,0.5)

sd(Poco)
sd(Severo)

np=length(Poco)
cvarp= var(Poco);cvarp
varp = (cvarp*(n-1))/n ;varp
sqrt(varp)

ns=length(Severo)
cvars= var(Severo);cvars
vars = (cvars*(n-1))/n ;vars
sqrt(vars)


sd(Poco)/mean(Poco)
sd(Severo)/mean(Severo)
quantile(Poco,0.75)-quantile(Poco,0.25)
quantile(Severo,0.75)-quantile(Severo,0.25)
quantile(Poco,0.9)
quantile(Poco,0.75)
quantile(Poco,0.5)
quantile(Poco,0.25)
quantile(Poco,0.1)
quantile(Severo,0.9)
quantile(Severo,0.75)
quantile(Severo,0.5)
quantile(Severo,0.25)
quantile(Severo,0.1)
boxplot(Poco,Severo, col="turquoise2")
#En severo, más concentración



#Datos chapala
install.packages('evd')
library(evd)
library(ismev)
library(fitdistrplus)
install.packages('nortest')
library('nortest')

datos=df$Rainfall....MM
plot(datos)

par(mfrow=c(1,1))

gpd.fitrange(datos,10,50)

#Consideremos el ajuste propuesto por Coles de 30
ajuste_trans=gpd.fit(datos,40)
gpd.diag(ajuste_trans)
#xi: -0.04089251

###---- primero escogemos un conjunto prueba equidistante
uv=seq(from =10, to = 50, length.out = 100)
uv=sort(uv)
uv

estimadores2=estimadores

estimadores=seq(0,0,length=length(uv))
i=1
for (u in uv){
  #Ajustamos GPD
  #print(u)
  ajuste=gpd.fit(datos,u)
  #Obtenemos las estimaciones de los parámetros
  xi=ajuste$mle[2]
  au=ajuste$mle[1]
  #Calculamos Tau_uj
  estimadores[i]=au-xi*u
  i=i+1
}

estimadores
plot(estimadores)
hist(estimadores)


#Ahora obtenemos las diferencias
diferencias=seq(0,0,length=length(uv)-1)
for (i in 1:length(uv)-1){
  diferencias[i]=estimadores[i+1]-estimadores[i]
}


#Graficamos las diferencias respecto a los umbrales propuestos
plot(uv[-99],diferencias,xlab = 'Umbrales candidatos',ylab = 'Diferencias')
abline(v=uv[2], col="red",lwd=3)

#-----Aplicamos el test de normalidad--------

N=length(diferencias)
for (i in 1:N-1){
  datos_aux=diferencias[i:N]
  #print(length(datos_aux))
  pval=pearson.test(datos_aux)$p.value
  #print(round(pval,digits=10))
  if (pval>0.05){
    print('Se logro')
    print(i)
    break
  }
}

#Se logró en el i=95
uv[2]
pearson.test(diferencias[43:N])$p.value

hist(diferencias[43:N])
length(uv)

uv[43]
#.... Estudiamos la normalidad.............
install.packages('MASS')
library(MASS)
fit <- fitdistr(diferencias[2:N], "normal")
para <- fit$estimate
para
hist(diferencias[2:N], prob = TRUE)
curve(dnorm(x, para[1], para[2]), col = 2, add = TRUE)

uv[2]

ajuste_trans=gpd.fit(datos,uv[43])
gpd.diag(ajuste_trans)
#-0.02416068

#Obtenemos las estimaciones de los parámetros
plot(datos,xlab = 'Año',ylab = 'Precipitación pluvial')
abline(h=40, col="blue",lwd=3)
abline(h=uv[2], col="magenta",lwd=3)

uv[97]
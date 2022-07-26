####-------------RAINFALL-------------
df = read.table("./rainfall.csv",header=TRUE,sep = ",")
plot(df$Year, df$Rainfall....MM.,xlab = 'Año',ylab = 'Precipitación pluvial')

head(df)
install.packages('ggplot2')
install.packages('dplyr')
install.packages('plotly')
install.packages('hrbrthemes')
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

df2=data.frame(df$Year, df$Rainfall....MM)
min(df$Year)
ggplot(df2)

install.packages('dygraphs')

library(dygraphs)
dygraph(df2) %>% dyOptions( drawPoints = TRUE, pointSize = 4 )

#https://www.kaggle.com/zusmani/rainfall-in-pakistan?select=Rainfall_1901_2016_PAK.csv

#Estudio gráfico
library(ismev)
library(fitdistrplus)
#-------------FUNCIONES----------
FEP=function(u,y){
  z=y[y>u]
  zz=length(z)
  e=(1/zz)*sum(y[y>u]-u)
  return(e) }
ED_estimator <- function(X, k){
  n <- length(X)
  res <- X[n]
  for (i in 1:k){
    a_ik <- log( (k+i)/(k+i - 1.0) )/log(2.0)
    res <- res + a_ik*( X[n - k] - X[n - k - i - 1] )
  }
  return(res)
}
GPDtail=function(x,a,y){
  (1+x*(y/a))^(-1/x)
}

#--------------------------------
datos=df$Rainfall....MM
ajuste1=gev.fit(datos)
gev.diag(ajuste1)


#--Graficamos la función de excesos
#ordenamos los datos
wo=sort(datos)
#no de datos
N=length(wo)
N

EE=seq(0,1,length=N)
for (i in 1:N){ EE[i]=FEP(wo[i],wo)[1] }
EE

par(mfrow=c(1,1))


#FME empírica
plot(wo,EE,type="l",xlim=c(100,160),lwd='3',xlab = "Precipitación Pluvial",ylab = "FME Empirica")
#Observamos un comportamiento de cola ligera

#FME empírica normalizada
#Tomamos sólo los valores positivos
plot(wo,EE/wo,type="l",xlim=c(0,200),xlab = "Precipitación Pluvial",ylab = "FME Empirica")


#Presenta un comportamiento como de Gumbel

###---------Transformamos los datos---------


#Estimamos el valor extremo derecho
par(mfrow=c(1,1))

max(datos)

wo=sort(datos)
w_F <- ED_estimator(wo,1)
w_F
for (k in 2:(N^0.5)){ w_F <- c(w_F,ED_estimator(wo,k)) }
plot(w_F, type="l")

#tomamos como extremo derecho
w=160

#Maximo de los datos 
max(datos) #---- 154.055

#Transformamos y comparamos las gráficas de dispersión 
datos_trans=1/(w-datos)
par(mfrow=c(1,2))
  plot(datos_trans,ylab='Precipitación pluvial')
  plot(datos,ylab='Precipitación pluvial')

#Se acentúa mas el comportamiento de cola pesada

#Graficamos las funciones de exceso promedio para los datos transformados
wo=sort(datos_trans)
N=length(wo)
N

EE=seq(0,1,length=N)
for (i in 1:N){ EE[i]=FEP(wo[i],wo)[1] }
EE
par(mfrow=c(1,2))
#FME empírica
plot(wo,EE,type="l",lwd='3',xlim=c(0,0.09),xlab = "Precipitación Pluvial (transformados)",ylab = "FME Empirica")
#Observamos un comportamiento de cola ligera

#FME empírica normalizada
#Tomamos sólo los valores positivos
plot(wo,EE/wo,type="l",xlim=c(0,0.09),xlab = "Precipitación Pluvial (transformados)",ylab = "FME Empirica Normalizada")

#Los transformados tienen aparentemente viven el dominio Frechet.
#Por tanto hay evidencia que viven en el dominio de atracción Weibull

#-----------Selección del umbral--------------------
par(mfrow=c(1,1))
plot(datos_trans)

length(datos_trans[datos_trans>.001])/length((datos_trans))
length(datos_trans[datos_trans>.0002])/length((datos_trans))


gpd.fitrange(datos_trans,0.007,.01)

#Observamos que en el 0.008 es un comportamiento más uniforme
ajuste_trans=gpd.fit(datos_trans,.008)
gpd.diag(ajuste_trans)


#Obtenemos las estimaciones de los parámetros
xi=ajuste_trans$mle[2]
au=ajuste_trans$mle[1]
fu=ajuste_trans$rate

xi
au
fu

#####---Automated-Selection-Method-----------

###---- primero escogemos un conjunto prueba equidistante
uv=seq(from = 0.007, to = 0.01, length.out = 100)
uv

datos=datos_trans
estimadores=seq(0,1,length=length(uv))
i=1
for (u in uv){
  #Ajustamos GPD
  ajuste=gpd.fit(datos,u)
  #Obtenemos las estimaciones de los parámetros
  xi=ajuste_trans$mle[2]
  au=ajuste_trans$mle[1]
  #Calculamos Tau_uj
  tau=au-xi*u
  #llenamos el vector
  estimadores[i]=tau
}

#Ahora obtenemos las diferencias
diferencias=seq(0,1,length=length(uv)-1)

for (i in 2:length(uv)-1){
    diferencias[i]=estimadores[i+1]-estimadores[i]
}

estimadores
diferencias

install.packages('nortest')
library('nortest')
pearson.test(rnorm(100, mean = 5, sd = 3))

#-----Aplicamos el test de normalidad--------
pearson.test(diferencias[2:length(diferencias)])$p.value

N=length(diferencias)

for (i in 1:N-1){
  datos_aux=diferencias[i:N]
  #print(datos_aux)
  pval=pearson.test(datos_aux)$p.value
  #print(round(pval,digits=10))
  if (pval>0.05){
    print('Se logro')
    print(i)
  }
}

N
pearson.test(diferencias[97:N])$p.value

diferencias[97:N]
length(uv)

uv[97]

ajuste_trans=gpd.fit(datos_trans,uv[97])
gpd.diag(ajuste_trans)

#Obtenemos las estimaciones de los parámetros
xi=ajuste_trans$mle[2]
au=ajuste_trans$mle[1]
fu=ajuste_trans$rate

xi
au
fu

for (i in 1:N){ print(i)}

diferencias[2:3]

print('Hola')

par(mfrow=c(1,1))
plot(datos_trans)
abline(h=.008, col="blue")
abline(h=uv[97], col="yellow")


#----------- con los datos sin transformar------
datos=df$Rainfall....MM
plot(df$Year, df$Rainfall....MM.,xlab = 'Año',ylab = 'Precipitación pluvial')


length(datos[datos>5])/length((datos))
length(datos[datos>10])/length((datos))


gpd.fitrange(datos,5,10)

#Observamos que en el 0.008 es un comportamiento más uniforme
ajuste_trans=gpd.fit(datos,8)
gpd.diag(ajuste_trans)


#Obtenemos las estimaciones de los parámetros
xi=ajuste_trans$mle[2]
au=ajuste_trans$mle[1]
fu=ajuste_trans$rate

xi
au
fu


###---- primero escogemos un conjunto prueba equidistante
uv=seq(from =5, to = 10, length.out = 100)
uv=sort(uv)

estimadores=seq(0,1,length=length(uv))
for (u in uv){
  #Ajustamos GPD
  ajuste=gpd.fit(datos,u)
  #Obtenemos las estimaciones de los parámetros
  xi=ajuste$mle[2]
  au=ajuste$mle[1]
  #Calculamos Tau_uj
  tau=au-xi*u
  #llenamos el vector
  estimadores[i]=tau
}

#Ahora obtenemos las diferencias
diferencias=seq(0,1,length=length(uv)-1)

for (i in 2:length(uv)-1){
  diferencias[i]=estimadores[i+1]-estimadores[i]
}

estimadores
diferencias

install.packages('nortest')
library('nortest')
pearson.test(rnorm(100, mean = 5, sd = 3))

#-----Aplicamos el test de normalidad--------
pearson.test(diferencias[2:length(diferencias)])$p.value

N=length(diferencias)

for (i in 1:N-1){
  datos_aux=diferencias[i:N]
  #print(datos_aux)
  pval=pearson.test(datos_aux)$p.value
  #print(round(pval,digits=10))
  if (pval>0.05){
    print('Se logro')
    print(i)
  }
}

N
pearson.test(diferencias[97:N])$p.value

diferencias[97:N]
length(uv)

uv[97]

ajuste_trans=gpd.fit(datos,uv[97])
gpd.diag(ajuste_trans)

#Obtenemos las estimaciones de los parámetros
xi=ajuste_trans$mle[2]
au=ajuste_trans$mle[1]
fu=ajuste_trans$rate

xi
au
fu


plot(df$Year, df$Rainfall....MM.,xlab = 'Año',ylab = 'Precipitación pluvial')
abline(h=8, col="blue",lwd=3)
abline(h=uv[97], col="magenta",lwd=3)

uv[97]
#--------Algoritmo----------
datos=df$Rainfall....MM
plot(df$Year, df$Rainfall....MM.,xlab = 'Año',ylab = 'Precipitación pluvial')

install.packages('tseries')
library('tseries')
adf.test(datos)



datos=read.csv(paste( "C:/Users/alexander.szyrej/Desktop/Seniales/tp/tp-proc-se/data/10068.MIB.csv" ,sep=','))
print (names(datos))
attach(datos)
plot(time,RR,type='l')
points(time,RR,pch=20)

#begin-interpolacion

N = length(time)
print(paste('N =',N))

for (j in 1:N)
{
	abline (v=time[j], lty=2)
}
Nmuestreo = as.integer(4*max(time))
print(paste('NM =',Nmuestreo))

RRlineal = approx(time,RR,n=Nmuestreo)

plot(RRlineal$x,RRlineal$y,type='l')

#end-interpolacion

fftRRLineal = fft(RRlineal)
plot(Mod(fftRRLineal),type='l')
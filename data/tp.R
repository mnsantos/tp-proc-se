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

#begin-mean-decomposition

RRlinealMean <- mean(RRlineal$y)
print(RRlinealMean)

for (j in 1:Nmuestreo)
{
	RRlineal$y[j] <- RRlineal$y[j] - RRlinealMean
}

#end-mean-decomposition

fftRRLineal = fft(RRlineal$y)
#plot(RRlineal$x,RRlineal$y,type='l')
plot(Mod(fftRRLineal),type='l')

#begin-HF-filter

HFFilter = rep(1,Nmuestreo)
HFFilter[1:Nmuestreo-1] = 0

filteredFFT = fftRRLineal*HFFilter

#end-HF-filter

#plot(Mod(filteredFFT),type='l')

RRLinealFiltered = Re(fft(filteredFFT ,inverse=TRUE)/Nmuestreo)
plot(RRlineal$x,RRLinealFiltered,type='l')

RRLinealDecomposed = (RRlinealOriginal - RRlinealMeanFunction)
for (j in 1:Nmuestreo)
{
	RRLinealDecomposed[j] <- RRLinealDecomposed[j] - RRLinealFiltered[j]
}

op <- par(mfrow = c(2,2))
plot(RRlineal$x,RRlinealOriginal,type='l')
plot(RRlineal$x,RRlinealMeanFunction,type='l')
plot(RRlineal$x,RRLinealFiltered,type='l')
plot(RRlineal$x,RRLinealDecomposed,type='l')
data=read.csv(paste( "C:/Users/alexander.szyrej/Desktop/Seniales/tp/tp-proc-se/data/10068.MIB.csv" ,sep=','))
print (names(data))
attach(data)
plot(time,RR,type='l')

#################begin-interpolacion

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

RRLinealOriginal = RRlineal$y

#################end-interpolacion

#################begin-mean-decomposition

RRlinealMean <- mean(RRlineal$y)
RRLinealMeanFunction = rep(RRlinealMean,Nmuestreo)

for (j in 1:Nmuestreo)
{
	RRlineal$y[j] <- RRlineal$y[j] - RRlinealMean
}

#################end-mean-decomposition

fftRRLineal = fft(RRlineal$y)
#################plot(RRlineal$x,RRlineal$y,type='l')
plot(Mod(fftRRLineal),type='l')

#################begin-DayNightFilter

DayNightFilter = rep(1,Nmuestreo)
DayNightFilter[2:Nmuestreo-1] = 0

filteredFFT = fftRRLineal*DayNightFilter

#################end-DayNightFilter

outMA21 <- filter(RRlineal$y,rep(1/1024,1024), circular =TRUE)
plot(RRlineal$x,outMA21,type='l')

#################plot(Mod(filteredFFT),type='l')

RRLinealFiltered = Re(fft(filteredFFT ,inverse=TRUE)/Nmuestreo)
plot(RRlineal$x,RRLinealFiltered,type='l')

RRLinealDecomposed1 = (RRLinealOriginal - RRLinealMeanFunction)
RRLinealDecomposed2 = RRLinealDecomposed1
for (j in 1:Nmuestreo)
{
	RRLinealDecomposed2[j] <- RRLinealDecomposed2[j] - RRLinealFiltered[j]
}

op <- par(mfrow = c(2,2))
plot(RRlineal$x,RRLinealOriginal,type='l')
plot(RRlineal$x,RRLinealMeanFunction,type='l')
plot(RRlineal$x,RRLinealFiltered,type='l')
plot(RRlineal$x,RRLinealDecomposed2,type='l')

op <- par(mfrow = c(1,1))
plot(RRlineal$x,RRLinealDecomposed1,type='l')
lines(RRlineal$x,RRLinealFiltered,col='red')

DeltaT = RRlineal$x[2] - RRlineal$x[1]
FrecMuestreo = 1/DeltaT
DeltaF = FrecMuestreo/Nmuestreo

N1= 0.04/DeltaF
N2= 0.15/DeltaF
N3= 0.4/DeltaF

print(DeltaT)
print(DeltaF)
print(FrecMuestreo)
print(N1)
print(N2)
print(N3)

for (i in 1:Nmuestreo)
{
	if(abs(RRlineal$x[i] - N1) < 0.1){
		I1 = i
		print(paste("I1= ",I1))
	}
	if(abs(RRlineal$x[i] - N2) < 0.1){
		I2 = i
		print(paste("I2= ",I2))
	}
	if(abs(RRlineal$x[i] - N3) < 0.1){
		I3 = i
		print(paste("I3= ",I3))
	}
}

#################begin-LFFilter

LFFilter = rep(0,Nmuestreo)
LFFilter[I1:I2] = 1

LFfilteredFFT = fftRRLineal*LFFilter

LFFunction = Re(fft(LFfilteredFFT ,inverse=TRUE)/Nmuestreo)

#################end-LFFilter

#################begin-HFFilter

HFFilter = rep(0,Nmuestreo)
HFFilter[I2:I3] = 1

HFfilteredFFT = fftRRLineal*HFFilter
HFFunction = Re(fft(HFfilteredFFT ,inverse=TRUE)/Nmuestreo)

#################end-HFFilter

op <- par(mfrow = c(1,2))
plot(RRlineal$x,LFFunction,type='l')
plot(RRlineal$x,HFFunction,type='l')

RRLinealDecomposed3 = RRLinealDecomposed1
for (j in 1:Nmuestreo)
{
	RRLinealDecomposed3[j] <- (RRLinealDecomposed3[j] - HFFunction[j]) - LFFunction[j]
}

op <- par(mfrow = c(1,1))
plot(RRlineal$x,RRLinealDecomposed3,type='l')

EHF = 0
for (j in 1:length(HFFunction)){
	EHF = EHF + HFFunction[j]**2
}
EHF = EHF * DeltaF

ELF = 0
for (j in 1:length(LFFunction)){
	ELF = ELF + LFFunction[j]**2
}
ELF = ELF * DeltaF

print(paste('EHF =',EHF))
print(paste('ELF =',ELF))
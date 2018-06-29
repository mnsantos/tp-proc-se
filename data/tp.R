data=read.csv(paste( "./10068.MIB.csv" ,sep=','))
print (names(data))
attach(data)
plot(time,RR,type='l')

#################begin-interpolacion

N = length(time)

NUniforme = as.integer(4*max(time))
print(paste('NM =',NUniforme))

InterpoladaLineal = approx(time,RR,n=NUniforme)

#################end-interpolacion

#################begin-mean-decomposition

InterpoladaLinealMean <- mean(InterpoladaLineal$y)
#plot(InterpoladaLineal,type='l',xlab="Time",ylab="RR")
#abline(h=InterpoladaLinealMean,color="blue")
InterpoladaLinealMeanFunction = rep(InterpoladaLinealMean,NUniforme)

#for (j in 1:NUniforme)
#{
#	InterpoladaLineal$y[j] <- InterpoladaLineal$y[j] - InterpoladaLinealMean
#}
InterpoladaLineal$y <- InterpoladaLineal$y - InterpoladaLinealMean
plot(InterpoladaLineal,type='l')

#################end-mean-decomposition

#################begin-DayNightFilter

#DayNightFilter = rep(1,NUniforme)
#DayNightFilter[2:NUniforme-1] = 0

#filteredFFT = fftInterpoladaLineal*DayNightFilter

#Means = rep(0,60)
#Coefs = rep(0,60)
#for (i in 1:60){
#	Coef = i*500
#	print(Coef)
#	FiltroMediaMovil <- filter(InterpoladaLineal$y,rep(1/Coef,Coef), circular =TRUE)
#	Temp <- InterpoladaLineal$y - FiltroMediaMovil
#	Means[i] = mean(Temp)
#	print(mean(Temp))
#	Coefs[i] = Coef
#}
#plot(Coefs,Means,type='l')

FiltroMediaMovil <- filter(InterpoladaLineal$y,rep(1/26000,26000), circular =TRUE)
plot(FiltroMediaMovil,type='l',xlab="Time",ylab="RR")
InterpoladaLineal$y <- InterpoladaLineal$y - FiltroMediaMovil

plot(InterpoladaLineal,type='l',xlab="Time",ylab="RR")


#################end-DayNightFilter

fftInterpoladaLineal = fft(InterpoladaLineal$y)

DeltaT = InterpoladaLineal$x[2] - InterpoladaLineal$x[1]
FrecMuestreo = 1/DeltaT
DeltaF = FrecMuestreo/NUniforme

EjeX = 0:(length(Mod(fftInterpoladaLineal))-1)
FrecX = DeltaF * EjeX
plot(FrecX,Mod(fftInterpoladaLineal),type='l',xlim=c(0,0.5))
abline(v=0.04,color="red")
abline(v=0.15,color="red")
abline(v=0.4,color="red")

#################begin-LFFilter

N1 = 0.04/DeltaF
N2 = 0.15/DeltaF
LFFilter = rep(0,length(EjeX))
LFFilter[N1:N2] = 1

LFfilteredFFT = fftInterpoladaLineal*LFFilter

LFFunction = Re(fft(LFfilteredFFT ,inverse=TRUE)/NUniforme)
plot(InterpoladaLineal$x,LFFunction,type='l')

#################end-LFFilter

#################begin-HFFilter
N3 = 0.4/DeltaF
HFFilter = rep(0,length(EjeX))
HFFilter[N2:N3] = 1

HFfilteredFFT = fftInterpoladaLineal*HFFilter
HFFunction = Re(fft(HFfilteredFFT ,inverse=TRUE)/NUniforme)
plot(InterpoladaLineal$x,HFFunction,type='l')

#################end-HFFilter

op <- par(mfrow = c(1,2))
plot(InterpoladaLineal$x,LFFunction,type='l')
plot(InterpoladaLineal$x,HFFunction,type='l')


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

print(paste('EHF =',EHF)) #774.6
print(paste('ELF =',ELF)) #1330.5
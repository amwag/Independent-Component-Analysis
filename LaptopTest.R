library(tuneR)#For reading/writing .wav files
library(fastICA)#For computing ICA
diagnostics=TRUE

RWave=readWave(filename="Data/RyansLaptop2.wav")
AWave=readWave(filename="Data/AnnesLaptop2.wav")
rawrate=48000; rawbit=16;

if(diagnostics){
par(mfrow=c(2,1))
plot(AWave@left,type='l')
plot(RWave@left,type='l')
}

AStart=135794+48000/2
RStart=130150+48000/2
AEnd=AStart+48000*29
REnd=RStart+48000*29

data=matrix(c(AWave@left[c(AStart:AEnd)],RWave@left[c(RStart:REnd)]),ncol=2,byrow=FALSE)

out=fastICA(data,n.comp=2,alg.typ=c("deflation"))

writeWave(Wave(out$S[,1]*500,samp.rate=rawrate,bit=rawbit),filename="LaptopTest1.wav")
writeWave(Wave(out$S[,2]*500,samp.rate=rawrate,bit=rawbit),filename="LaptopTest2.wav")
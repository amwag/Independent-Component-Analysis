library(tuneR)#For reading/writing .wav files
library(fastICA)#For computing ICA
#library(e1071)
diagnostics=TRUE

AWave=readWave(filename="Data/IndependentRecordingAnne.wav")
RWave=readWave(filename="Data/IndependentRecordingRyan.wav")
rawrate=48000; rawbit=16;

if(diagnostics){
par(mfrow=c(2,1))
plot(AWave@left,type='l')
plot(RWave@left,type='l')
}

AStart=rawrate
RStart=rawrate*4
AEnd=rawrate*31
REnd=rawrate*34

adat=AWave@left[c(AStart:AEnd)]
rdat=RWave@left[c(RStart:REnd)]
rscale=sqrt(var(adat)/var(rdat))
test1=.75*adat+1.5*rdat*rscale
test2=1.5*adat+.75*rdat*rscale
test1=test1*32766/max(abs(test1))
test2=test2*32766/max(abs(test2))

writeWave(Wave(round(test1),samp.rate=rawrate,bit=rawbit),filename="MixTest1.wav")
writeWave(Wave(round(test2),samp.rate=rawrate,bit=rawbit),filename="MixTest2.wav")

data=matrix(c(test1,test2),byrow=FALSE,ncol=2)

out=fastICA(data,n.comp=2,alg.typ=c("deflation"))
#out2=ica(data,lrate=1)

writeWave(Wave(round(out$S[,1]*500),samp.rate=rawrate,bit=rawbit),filename="LaptopIndepTest1.wav")
writeWave(Wave(round(out$S[,2]*500),samp.rate=rawrate,bit=rawbit),filename="LaptopIndepTest2.wav")

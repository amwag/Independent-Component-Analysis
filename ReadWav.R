library(tuneR)#For reading/writing .wav files
library(fastICA)#For computing ICA

w1=readWave(filename="Data/001101000mix1.wav")
w2=readWave(filename="Data/001101000mix2.wav")
w3=readWave(filename="Data/001101000mix3.wav")
rawrate=8000;rawbit=8


combined=matrix(c(w1@left,w2@left,w3@left),byrow=FALSE,ncol=3)
out=fastICA(combined, n.comp=3, alg.typ=c("parallel"))

dat.source=out$S
dat.source=round(dat.source*127/max(abs(dat.source))+127)

writeWave(Wave(dat.source[,1], samp.rate=rawrate, bit=rawbit), filename="CPP1.wav")
writeWave(Wave(dat.source[,2], samp.rate=rawrate, bit=rawbit), filename="CPP2.wav")
writeWave(Wave(dat.source[,3], samp.rate=rawrate, bit=rawbit), filename="CPP3.wav")




#NOT CURRENTLY WORKING - DOESN'T SEPARATE THE TWO CHATTERS

#rawread=readWave(filename="scene01(Chatting).wav")
#Data.FL=rawread@.Data[,1]
#Data.FR=rawread@.Data[,2]
#Data.BL=rawread@.Data[,3]
#Data.BR=rawread@.Data[,4]

#FrontMatrix=matrix(c(Data.FL,Data.FR),byrow=FALSE,ncol=2)
#RightMatrix=matrix(c(Data.BR,Data.FR),byrow=FALSE,ncol=2)
#LeftMatrix=matrix(c(Data.FL,Data.BL),byrow=FALSE,ncol=2)
#BackMatrix=matrix(c(Data.BR,Data.BL),byrow=FALSE,ncol=2)

#FrontICA=fastICA(FrontMatrix, n.comp=2, alg.typ=c("parallel"))
#RightICA=fastICA(RightMatrix, n.comp=2, alg.typ=c("parallel"))
#LeftICA=fastICA(LeftMatrix, n.comp=2, alg.typ=c("parallel"))
#BackICA=fastICA(BackMatrix, n.comp=2, alg.typ=c("parallel"))

#FrontSource1=FrontICA$S[,1]
#FrontSource2=FrontICA$S[,2]
#RightSource1=RightICA$S[,1]
#RightSource2=RightICA$S[,2]
#LeftSource1=LeftICA$S[,1]
#LeftSource2=LeftICA$S[,2]
#BackSource1=BackICA$S[,1]
#BackSource2=BackICA$S[,2]

#writeWave(Wave(round(FrontSource1*500), samp.rate=48000, bit=16), filename="SC1F1.wav")
#writeWave(Wave(round(FrontSource2*500), samp.rate=48000, bit=16), filename="SC1F2.wav")
#writeWave(Wave(RightSource1, samp.rate=48000, bit=16), filename="SC1R1.wav")
#writeWave(Wave(RightSource2, samp.rate=48000, bit=16), filename="SC1R2.wav")
#writeWave(Wave(LeftSource1, samp.rate=48000, bit=16), filename="SC1L1.wav")
#writeWave(Wave(LeftSource2, samp.rate=48000, bit=16), filename="SC1L2.wav")
#writeWave(Wave(BackSource1, samp.rate=48000, bit=16), filename="SC1B1.wav")
#writeWave(Wave(BackSource2, samp.rate=48000, bit=16), filename="SC1B2.wav")
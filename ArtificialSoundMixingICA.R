library(tuneR)#For reading/writing .wav files
library(fastICA)#For computing ICA
library(pracma)
#library(e1071)
runreport=TRUE

if(runreport){print("Reading Files")}

AWave=downsample(readWave(filename="AudioMixingData/AnneRecording.wav"),samp.rate=44100)
RWave=downsample(readWave(filename="AudioMixingData/RyanRecording.wav"),samp.rate=44100)
DWave=readWave(filename="AudioMixingData/DogBark.wav")
PWave=readWave(filename="AudioMixingData/PedestrianAmbiance.wav")
MWave=readWave(filename="AudioMixingData/Passacaglia.wav")
rawrate=44100; rawbit=16;

if(runreport){print("Subsampling Data")}

#Thirty Second Clips
#Anne: 0:01-0:31
Ainds=c((rawrate*1):(rawrate*31))
#Ryan: 0:04-0:34
Rinds=c((rawrate*4):(rawrate*34))
#Bark: 0:10-0:40
Dinds=c((rawrate*10):(rawrate*40))
#Mall: 0:36-1:06
Pinds=c((rawrate*36):(rawrate*66))
#Music: 1:00-1:30
Minds=c((rawrate*60):(rawrate*90))

adat=AWave@left[Ainds]/sqrt(var(AWave@left[Ainds]))
rdat=RWave@left[Rinds]/sqrt(var(RWave@left[Rinds]))
ddat=DWave@left[Dinds]/sqrt(var(DWave@left[Dinds]))
pdat=PWave@left[Pinds]/sqrt(var(PWave@left[Pinds]))
mdat=MWave@left[Minds]/sqrt(var(MWave@left[Minds]))

#Indices are 1-Anne, 2-Ryan, 3-Dog, 4-Pedestrians, 5-Music
fulldat=matrix(c(adat,rdat,ddat,pdat,mdat),byrow=FALSE,ncol=5)

if(runreport){print("Mixing Data")}

if(TRUE){
fivemix=rortho(5)
fourmix=rortho(4)
threemix=rortho(3)
twomix=rortho(2)
} else if(FALSE) {
fivemix=matrix(runif(5**2,-1,1),nrow=5)
fourmix=matrix(runif(4**2,-1,1),nrow=4)
threemix=matrix(runif(3**2,-1,1),nrow=3)
twomix=matrix(runif(2**2,-1,1),nrow=2)
} else {
fivemix=matrix(rexp(5**2)*sample(c(-1,1),5**2,rep=T),nrow=5)
fourmix=matrix(rexp(4**2)*sample(c(-1,1),4**2,rep=T),nrow=4)
threemix=matrix(rexp(3**2)*sample(c(-1,1),3**2,rep=T),nrow=3)
twomix=matrix(rexp(2**2)*sample(c(-1,1),2**2,rep=T),nrow=2)
}

fivemixdat=fulldat%*%fivemix
fourmixdat=fulldat[,c(1,2,3,4)]%*%fourmix
threemixdat=fulldat[,c(1,2,5)]%*%threemix
twomixdat=fulldat[,c(1,2)]%*%twomix

#ADD NOISE TO DATA
if(TRUE){
fivemixdat=fivemixdat+matrix(rnorm(prod(dim(fivemixdat))),ncol=5)/2
fourmixdat=fourmixdat+matrix(rnorm(prod(dim(fourmixdat))),ncol=4)/2
threemixdat=threemixdat+matrix(rnorm(prod(dim(threemixdat))),ncol=3)/2
twomixdat=twomixdat+matrix(rnorm(prod(dim(twomixdat))),ncol=2)/2
}

if(runreport){print("Running deflation ICA")}

out5d=fastICA(fivemixdat,n.comp=5,alg.typ=c('deflation'))
out4d=fastICA(fourmixdat,n.comp=4,alg.typ=c('deflation'))
out3d=fastICA(threemixdat,n.comp=3,alg.typ=c('deflation'))
out2d=fastICA(twomixdat,n.comp=2,alg.typ=c('deflation'))

if(runreport){print("Running parallel ICA")}

out5p=fastICA(fivemixdat,n.comp=5,alg.typ=c('parallel'))
out4p=fastICA(fourmixdat,n.comp=4,alg.typ=c('parallel'))
out3p=fastICA(threemixdat,n.comp=3,alg.typ=c('parallel'))
out2p=fastICA(twomixdat,n.comp=2,alg.typ=c('parallel'))

WriteFiles=TRUE
if(WriteFiles){
if(runreport){print("Writing Files")}

directory=paste("AudioICAOutput/",paste(sample(LETTERS,6,rep=T),collapse=""),sep='')
dir.create(directory)

volscale=30000/max(c(max(abs(fivemixdat)),max(abs(out5d$S)),max(abs(out5p$S)),
	       max(abs(fourmixdat)),max(abs(out4d$S)),max(abs(out4p$S)),
	       max(abs(threemixdat)),max(abs(out3d$S)),max(abs(out3p$S)),
	       max(abs(twomixdat)),max(abs(out2d$S)),max(abs(out2p$S))))


for(i in 1:5){
writeWave(Wave(round(fivemixdat[,i]*(volscale)),samp.rate=rawrate,bit=rawbit),filename=paste(directory,"/fiveMixing",i,".wav",sep=''))
writeWave(Wave(round(out5d$S[,i]*(volscale)),samp.rate=rawrate,bit=rawbit),filename=paste(directory,"/fiveSeparatedD",i,".wav",sep=''))
writeWave(Wave(round(out5p$S[,i]*(volscale)),samp.rate=rawrate,bit=rawbit),filename=paste(directory,"/fiveSeparatedP",i,".wav",sep=''))
}
if(runreport){print("Finished 5mixing")}
for(i in 1:4){
writeWave(Wave(round(fourmixdat[,i]*(volscale)),samp.rate=rawrate,bit=rawbit),filename=paste(directory,"/fourMixing",i,".wav",sep=''))
writeWave(Wave(round(out4d$S[,i]*(volscale)),samp.rate=rawrate,bit=rawbit),filename=paste(directory,"/fourSeparatedD",i,".wav",sep=''))
writeWave(Wave(round(out4p$S[,i]*(volscale)),samp.rate=rawrate,bit=rawbit),filename=paste(directory,"/fourSeparatedP",i,".wav",sep=''))
}
if(runreport){print("Finished 4mixing")}
for(i in 1:3){
writeWave(Wave(round(threemixdat[,i]*(volscale)),samp.rate=rawrate,bit=rawbit),filename=paste(directory,"/threeMixing",i,".wav",sep=''))
writeWave(Wave(round(out3d$S[,i]*(volscale)),samp.rate=rawrate,bit=rawbit),filename=paste(directory,"/threeSeparatedD",i,".wav",sep=''))
writeWave(Wave(round(out3p$S[,i]*(volscale)),samp.rate=rawrate,bit=rawbit),filename=paste(directory,"/threeSeparatedP",i,".wav",sep=''))
}
if(runreport){print("Finished 3mixing")}
for(i in 1:2){
writeWave(Wave(round(twomixdat[,i]*(volscale)),samp.rate=rawrate,bit=rawbit),filename=paste(directory,"/twoMixing",i,".wav",sep=''))
writeWave(Wave(round(out2d$S[,i]*(volscale)),samp.rate=rawrate,bit=rawbit),filename=paste(directory,"/twoSeparatedD",i,".wav",sep=''))
writeWave(Wave(round(out2p$S[,i]*(volscale)),samp.rate=rawrate,bit=rawbit),filename=paste(directory,"/twoSeparatedP",i,".wav",sep=''))
}
if(runreport){print("Finished 2mixing")}

sink(paste(directory,"/MixingLevels.txt",sep=''))
print("Five Channel Mixing")
print("Actual:")
print(fivemix)
print("Estimated (D):")
print(out5d$A)
print("Estimated (P):")
print(out5p$A)
print("Four Channel Mixing")
print("Actual:")
print(fourmix)
print("Estimated (D):")
print(out4d$A)
print("Estimated (P):")
print(out4p$A)
print("Three Channel Mixing")
print("Actual:")
print(threemix)
print("Estimated (D):")
print(out3d$A)
print("Estimated (P):")
print(out3p$A)
print("Two Channel Mixing")
print("Actual:")
print(twomix)
print("Estimated (D):")
print(out2d$A)
print("Estimated (P):")
print(out2p$A)
sink()

}#To suppress filewriting


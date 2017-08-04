data <- read.delim("C:/Users/SaMi GiMiX/Desktop/R/data.txt")
isna=apply(data, 1, function(x)is.na(sum(x)))
sum(isna)
isna=apply(data, 1, function(x)!is.na(sum(x)))
sum(isna)
fit <- lm(SCORE~Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q10+Q11+Q12+Q13+Q14+Q15+Q16+Q17+Q18+Q19+Q20+Q21+Q22-1,data = data[isna,])
summary(fit)
fit2 <- lm(SCORE~Q1+Q2+Q3+Q4+Q5+Q6+Q8+Q9+Q10+Q12+Q13+Q14+Q15+Q16+Q17+Q20+Q21+Q22-1,data = data[isna,])
summary(fit2)
cof <- fit2$coefficients
cof1=cof+abs(cof[9])*2
nCOF=cof1/sum(cof1)
apply(data, 2, sum)
for(i in 1:(dim(data)[2]-1)){data[is.na(data[,i]),i]=mean(data[,i], na.rm=T)}
apply(data, 2, sum)
ncof1=c(nCOF[1:6],0,nCOF[7:9],0,nCOF[10:15], 0,0,nCOF[16:18])
CSI=as.matrix(data[,-23])%*%ncof1
(mean(CSI)-1)*25
CSImeans <- matrix(c((mean(CSI[1:191])-1)*25,(mean(CSI[192:312])-1)*25,(mean(CSI[313:402])-1)*25,(mean(CSI[403:473])-1)*25,(mean(CSI[474:500])-1)*25),ncol=1,byrow=TRUE)
colnames(CSImeans) <- c("CSI Mean")
rownames(CSImeans) <- c("Adabiat","Modiriat","Riazi","Ravanshenasi","Tarbiatbadani")
CSImeans <- as.table(CSImeans)
CSImeans
AttrMeans <- c(dim(data)[2]-1)
for(i in 1:(dim(data)[2]-1)){AttrMeans[i]=(mean(data[,i])-1)*25}
AttrMeans <- matrix(AttrMeans,ncol=1,byrow=TRUE)
colnames(AttrMeans) <- c("Attribute Mean")
rownames(AttrMeans) <- c(paste("Q",1:22, sep=""))
AttrMeans <- as.table(AttrMeans)
AttrMeans
Adabiat <- c(dim(data)[2]-1)
for(i in 1:(dim(data)[2]-1)){Adabiat[i]=(mean(data[1:191,i])-1)*25}
Adabiat <- matrix(Adabiat,ncol=1,byrow=TRUE)
colnames(Adabiat) <- c("Adabiat")
rownames(Adabiat) <- c(paste("Q",1:22, sep=""))
Adabiat <- as.table(Adabiat)
Modiriat <- c(dim(data)[2]-1)
for(i in 1:(dim(data)[2]-1)){Modiriat[i]=(mean(data[192:312,i])-1)*25}
Modiriat <- matrix(Modiriat,ncol=1,byrow=TRUE)
colnames(Modiriat) <- c("Modiriat")
rownames(Modiriat) <- c(paste("Q",1:22, sep=""))
Modiriat <- as.table(Modiriat)
Riazi <- c(dim(data)[2]-1)
for(i in 1:(dim(data)[2]-1)){Riazi[i]=(mean(data[313:402,i])-1)*25}
Riazi <- matrix(Riazi,ncol=1,byrow=TRUE)
colnames(Riazi) <- c("Riazi")
rownames(Riazi) <- c(paste("Q",1:22, sep=""))
Riazi <- as.table(Riazi)
Ravanshenasi <- c(dim(data)[2]-1)
for(i in 1:(dim(data)[2]-1)){Ravanshenasi[i]=(mean(data[403:473,i])-1)*25}
Ravanshenasi <- matrix(Ravanshenasi,ncol=1,byrow=TRUE)
colnames(Ravanshenasi) <- c("Ravanshenasi")
rownames(Ravanshenasi) <- c(paste("Q",1:22, sep=""))
Ravanshenasi <- as.table(Ravanshenasi)
Tarbiatbadani <- c(dim(data)[2]-1)
for(i in 1:(dim(data)[2]-1)){Tarbiatbadani[i]=(mean(data[473:500,i])-1)*25}
Tarbiatbadani <- matrix(Tarbiatbadani,ncol=1,byrow=TRUE)
colnames(Tarbiatbadani) <- c("Tarbiatbadani")
rownames(Tarbiatbadani) <- c(paste("Q",1:22, sep=""))
Tarbiatbadani <- as.table(Tarbiatbadani)
CtgrAttr <- cbind(Adabiat,Modiriat,Riazi,Ravanshenasi,Tarbiatbadani)
CtgrAttr
Amuzeshi <- c(dim(22))
for(i in 1:22){if(i<8){Amuzeshi[i]=ncof1[i]}else{Amuzeshi[i]=0}}
Amuzeshi <- matrix(Amuzeshi,ncol=1,byrow=TRUE)
colnames(Amuzeshi) <- c("Amuzeshi")
rownames(Amuzeshi) <- c(paste("Q",1:22, sep=""))
Amuzeshi <- as.table(Amuzeshi)
Pazhuheshi <- c(dim(22))
for(i in 1:22){if(i>7 && i<11){Pazhuheshi[i]=ncof1[i]}else{Pazhuheshi[i]=0}}
Pazhuheshi <- matrix(Pazhuheshi,ncol=1,byrow=TRUE)
colnames(Pazhuheshi) <- c("Pazhuheshi")
rownames(Pazhuheshi) <- c(paste("Q",1:22, sep=""))
Pazhuheshi <- as.table(Pazhuheshi)
Refahi <- c(dim(22))
for(i in 1:22){if(i>10 && i<17){Refahi[i]=ncof1[i]}else{Refahi[i]=0}}
Refahi <- matrix(Refahi,ncol=1,byrow=TRUE)
colnames(Refahi) <- c("Refahi")
rownames(Refahi) <- c(paste("Q",1:22, sep=""))
Refahi <- as.table(Refahi)
FogheBarname <- c(dim(22))
for(i in 1:22){if(i>16 && i<20){FogheBarname[i]=ncof1[i]}else{FogheBarname[i]=0}}
FogheBarname <- matrix(FogheBarname,ncol=1,byrow=TRUE)
colnames(FogheBarname) <- c("FogheBarname")
rownames(FogheBarname) <- c(paste("Q",1:22, sep=""))
FogheBarname <- as.table(FogheBarname)
Sayer <- c(dim(22))
for(i in 1:22){if(i>19){Sayer[i]=ncof1[i]}else{Sayer[i]=0}}
Sayer <- matrix(Sayer,ncol=1,byrow=TRUE)
colnames(Sayer) <- c("Sayer")
rownames(Sayer) <- c(paste("Q",1:22, sep=""))
Sayer <- as.table(Sayer)
nCofCtgr <- cbind(Amuzeshi,Pazhuheshi,Refahi,FogheBarname,Sayer)
nCofCtgr
nAmuzeshi=Amuzeshi/sum(Amuzeshi)
AmuzeshiCSI <- c(dim(1))
AmuzeshiCSI=(mean(as.matrix(data[,-23])%*%nAmuzeshi)-1)*25
AmuzeshiCSI <- matrix(AmuzeshiCSI,ncol=1,byrow=TRUE)
colnames(AmuzeshiCSI) <- c("Amuzeshi")
rownames(AmuzeshiCSI) <- c("CSI Means")
AmuzeshiCSI <- as.table(AmuzeshiCSI)
nPazhuheshi=Pazhuheshi/sum(Pazhuheshi)
PazhuheshiCSI <- c(dim(1))
PazhuheshiCSI=(mean(as.matrix(data[,-23])%*%nPazhuheshi)-1)*25
PazhuheshiCSI <- matrix(PazhuheshiCSI,ncol=1,byrow=TRUE)
colnames(PazhuheshiCSI) <- c("Pazhuheshi")
rownames(PazhuheshiCSI) <- c("CSI Means")
PazhuheshiCSI <- as.table(PazhuheshiCSI)
nRefahi=Refahi/sum(Refahi)
RefahiCSI <- c(dim(1))
RefahiCSI=(mean(as.matrix(data[,-23])%*%nRefahi)-1)*25
RefahiCSI <- matrix(RefahiCSI,ncol=1,byrow=TRUE)
colnames(RefahiCSI) <- c("Refahi")
rownames(RefahiCSI) <- c("CSI Means")
RefahiCSI <- as.table(RefahiCSI)
nFogheBarname=FogheBarname/sum(FogheBarname)
FogheBarnameCSI <- c(dim(1))
FogheBarnameCSI=(mean(as.matrix(data[,-23])%*%nFogheBarname)-1)*25
FogheBarnameCSI <- matrix(FogheBarnameCSI,ncol=1,byrow=TRUE)
colnames(FogheBarnameCSI) <- c("FogheBarname")
rownames(FogheBarnameCSI) <- c("CSI Means")
FogheBarnameCSI <- as.table(FogheBarnameCSI)
nSayer=Sayer/sum(Sayer)
SayerCSI <- c(dim(1))
SayerCSI=(mean(as.matrix(data[,-23])%*%nSayer)-1)*25
SayerCSI <- matrix(SayerCSI,ncol=1,byrow=TRUE)
colnames(SayerCSI) <- c("Sayer")
rownames(SayerCSI) <- c("CSI Means")
SayerCSI <- as.table(SayerCSI)
TotalCSI <- c(dim(1))
TotalCSI=(mean(as.matrix(data[,-23])%*%ncof1)-1)*25
TotalCSI <- matrix(TotalCSI,ncol=1,byrow=TRUE)
colnames(TotalCSI) <- c("Total")
rownames(TotalCSI) <- c("CSI Means")
TotalCSI <- as.table(TotalCSI)
CSITypeMeans <- cbind(AmuzeshiCSI,PazhuheshiCSI,RefahiCSI,FogheBarnameCSI,SayerCSI,TotalCSI)
CSITypeMeans
SubAmuzeshi <- c(dim(7))
SubAmuzeshi[1:7]=AttrMeans[1:7]
SubAmuzeshi <- matrix(SubAmuzeshi,ncol=1,byrow=TRUE)
colnames(SubAmuzeshi) <- c("Amuzeshi")
rownames(SubAmuzeshi) <- c(paste("Q",1:7, sep=""))
SubAmuzeshi <- as.table(SubAmuzeshi)
SubPazhuheshi <- c(dim(3))
SubPazhuheshi[1:3]=AttrMeans[8:10]
SubPazhuheshi <- matrix(SubPazhuheshi,ncol=1,byrow=TRUE)
colnames(SubPazhuheshi) <- c("Pazhuheshi")
rownames(SubPazhuheshi) <- c(paste("Q",8:10, sep=""))
SubPazhuheshi <- as.table(SubPazhuheshi)
SubRefahi <- c(dim(6))
SubRefahi[1:6]=AttrMeans[11:16]
SubRefahi <- matrix(SubRefahi,ncol=1,byrow=TRUE)
colnames(SubRefahi) <- c("Refahi")
rownames(SubRefahi) <- c(paste("Q",11:16, sep=""))
SubRefahi <- as.table(SubRefahi)
SubFogheBarname <- c(dim(3))
SubFogheBarname[1:3]=AttrMeans[17:19]
SubFogheBarname <- matrix(SubFogheBarname,ncol=1,byrow=TRUE)
colnames(SubFogheBarname) <- c("FogheBarname")
rownames(SubFogheBarname) <- c(paste("Q",17:19, sep=""))
SubFogheBarname <- as.table(SubFogheBarname)
SubSayer <- c(dim(3))
SubSayer[1:3]=AttrMeans[20:22]
SubSayer <- matrix(SubSayer,ncol=1,byrow=TRUE)
colnames(SubSayer) <- c("Sayer")
rownames(SubSayer) <- c(paste("Q",20:22, sep=""))
SubSayer <- as.table(SubSayer)
SubAmuzeshi
SubPazhuheshi
SubRefahi
SubFogheBarname
SubSayer
AmuzeshiDep <- c(dim(5))
AmuzeshiDep[1]=(mean(as.matrix(data[1:191,-23])%*%nAmuzeshi)-1)*25
AmuzeshiDep[2]=(mean(as.matrix(data[192:312,-23])%*%nAmuzeshi)-1)*25
AmuzeshiDep[3]=(mean(as.matrix(data[313:402,-23])%*%nAmuzeshi)-1)*25
AmuzeshiDep[4]=(mean(as.matrix(data[403:473,-23])%*%nAmuzeshi)-1)*25
AmuzeshiDep[5]=(mean(as.matrix(data[474:500,-23])%*%nAmuzeshi)-1)*25
AmuzeshiDep <- matrix(AmuzeshiDep,ncol=1,byrow=TRUE)
colnames(AmuzeshiDep) <- c("Amuzeshi")
rownames(AmuzeshiDep) <- c("Adabiat","Modiriat","Riazi","Ravanshenasi","Tarbiatbadani")
AmuzeshiDep <- as.table(AmuzeshiDep)
PazhuheshiDep <- c(dim(5))
PazhuheshiDep[1]=(mean(as.matrix(data[1:191,-23])%*%nPazhuheshi)-1)*25
PazhuheshiDep[2]=(mean(as.matrix(data[192:312,-23])%*%nPazhuheshi)-1)*25
PazhuheshiDep[3]=(mean(as.matrix(data[313:402,-23])%*%nPazhuheshi)-1)*25
PazhuheshiDep[4]=(mean(as.matrix(data[403:473,-23])%*%nPazhuheshi)-1)*25
PazhuheshiDep[5]=(mean(as.matrix(data[474:500,-23])%*%nPazhuheshi)-1)*25
PazhuheshiDep <- matrix(PazhuheshiDep,ncol=1,byrow=TRUE)
colnames(PazhuheshiDep) <- c("Pazhuheshi")
rownames(PazhuheshiDep) <- c("Adabiat","Modiriat","Riazi","Ravanshenasi","Tarbiatbadani")
PazhuheshiDep <- as.table(PazhuheshiDep)
RefahiDep <- c(dim(5))
RefahiDep[1]=(mean(as.matrix(data[1:191,-23])%*%nRefahi)-1)*25
RefahiDep[2]=(mean(as.matrix(data[192:312,-23])%*%nRefahi)-1)*25
RefahiDep[3]=(mean(as.matrix(data[313:402,-23])%*%nRefahi)-1)*25
RefahiDep[4]=(mean(as.matrix(data[403:473,-23])%*%nRefahi)-1)*25
RefahiDep[5]=(mean(as.matrix(data[474:500,-23])%*%nRefahi)-1)*25
RefahiDep <- matrix(RefahiDep,ncol=1,byrow=TRUE)
colnames(RefahiDep) <- c("Refahi")
rownames(RefahiDep) <- c("Adabiat","Modiriat","Riazi","Ravanshenasi","Tarbiatbadani")
RefahiDep <- as.table(RefahiDep)
FogheBarnameDep <- c(dim(5))
FogheBarnameDep[1]=(mean(as.matrix(data[1:191,-23])%*%nFogheBarname)-1)*25
FogheBarnameDep[2]=(mean(as.matrix(data[192:312,-23])%*%nFogheBarname)-1)*25
FogheBarnameDep[3]=(mean(as.matrix(data[313:402,-23])%*%nFogheBarname)-1)*25
FogheBarnameDep[4]=(mean(as.matrix(data[403:473,-23])%*%nFogheBarname)-1)*25
FogheBarnameDep[5]=(mean(as.matrix(data[474:500,-23])%*%nFogheBarname)-1)*25
FogheBarnameDep <- matrix(FogheBarnameDep,ncol=1,byrow=TRUE)
colnames(FogheBarnameDep) <- c("FogheBarname")
rownames(FogheBarnameDep) <- c("Adabiat","Modiriat","Riazi","Ravanshenasi","Tarbiatbadani")
FogheBarnameDep <- as.table(FogheBarnameDep)
SayerDep <- c(dim(5))
SayerDep[1]=(mean(as.matrix(data[1:191,-23])%*%nSayer)-1)*25
SayerDep[2]=(mean(as.matrix(data[192:312,-23])%*%nSayer)-1)*25
SayerDep[3]=(mean(as.matrix(data[313:402,-23])%*%nSayer)-1)*25
SayerDep[4]=(mean(as.matrix(data[403:473,-23])%*%nSayer)-1)*25
SayerDep[5]=(mean(as.matrix(data[474:500,-23])%*%nSayer)-1)*25
SayerDep <- matrix(SayerDep,ncol=1,byrow=TRUE)
colnames(SayerDep) <- c("Sayer")
rownames(SayerDep) <- c("Adabiat","Modiriat","Riazi","Ravanshenasi","Tarbiatbadani")
SayerDep <- as.table(SayerDep)
TotalDep <- c(dim(5))
for (i in 1:5) {TotalDep[i]=CSImeans[i]}
TotalDep <- matrix(TotalDep,ncol=1,byrow=TRUE)
colnames(TotalDep) <- c("Total")
rownames(TotalDep) <- c("Adabiat","Modiriat","Riazi","Ravanshenasi","Tarbiatbadani")
TotalDep <- as.table(TotalDep)
DepartmentCSIType <- cbind(AmuzeshiDep,PazhuheshiDep,RefahiDep,FogheBarnameDep,SayerDep,TotalDep)
DepartmentCSIType
GenderAgeData <- read.delim("C:/Users/SaMi GiMiX/Desktop/Gender&Age data.txt")
GenderSort <- GenderAgeData[order(GenderAgeData$Gender),]
GenderSort[] <- lapply(GenderSort, function(x) if(is.integer(x)) as.numeric(x) else x)
AmuzeshiGen <- c(dim(3))
AmuzeshiGen[1] <- (mean(as.matrix(GenderSort[10:346,3:24])%*%nAmuzeshi)-1)*25
AmuzeshiGen[2] <- (mean(as.matrix(GenderSort[347:500,3:24])%*%nAmuzeshi)-1)*25
AmuzeshiGen[3] <- (mean(as.matrix(GenderSort[1:9,3:24])%*%nAmuzeshi)-1)*25
AmuzeshiGen <- matrix(AmuzeshiGen,ncol=1,byrow=TRUE)
colnames(AmuzeshiGen) <- c("Amuzeshi")
rownames(AmuzeshiGen) <- c("Female","Male","Unknown")
AmuzeshiGen <- as.table(AmuzeshiGen)
PazhuheshiGen <- c(dim(3))
PazhuheshiGen[1] <- (mean(as.matrix(GenderSort[10:346,3:24])%*%nPazhuheshi)-1)*25
PazhuheshiGen[2] <- (mean(as.matrix(GenderSort[347:500,3:24])%*%nPazhuheshi)-1)*25
PazhuheshiGen[3] <- (mean(as.matrix(GenderSort[1:9,3:24])%*%nPazhuheshi )-1)*25
PazhuheshiGen <- matrix(PazhuheshiGen ,ncol=1,byrow=TRUE)
colnames(PazhuheshiGen ) <- c("Pazhuheshi")
rownames(PazhuheshiGen ) <- c("Female","Male","Unknown")
PazhuheshiGen <- as.table(PazhuheshiGen)
RefahiGen  <- c(dim(3))
RefahiGen[1] <- (mean(as.matrix(GenderSort[10:346,3:24])%*%nRefahi)-1)*25
RefahiGen[2] <- (mean(as.matrix(GenderSort[347:500,3:24])%*%nRefahi)-1)*25
RefahiGen[3] <- (mean(as.matrix(GenderSort[1:9,3:24])%*%nRefahi )-1)*25
RefahiGen <- matrix(RefahiGen ,ncol=1,byrow=TRUE)
colnames(RefahiGen) <- c("Refahi")
rownames(RefahiGen) <- c("Female","Male","Unknown")
RefahiGen <- as.table(RefahiGen)
FogheBarnameGen  <- c(dim(3))
FogheBarnameGen[1] <- (mean(as.matrix(GenderSort[10:346,3:24])%*%nFogheBarname)-1)*25
FogheBarnameGen[2] <- (mean(as.matrix(GenderSort[347:500,3:24])%*%nFogheBarname)-1)*25
FogheBarnameGen[3] <- (mean(as.matrix(GenderSort[1:9,3:24])%*%nFogheBarname)-1)*25
FogheBarnameGen  <- matrix(FogheBarnameGen ,ncol=1,byrow=TRUE)
colnames(FogheBarnameGen) <- c("FogheBarname")
rownames(FogheBarnameGen) <- c("Female","Male","Unknown")
FogheBarnameGen <- as.table(FogheBarnameGen)	
SayerGen  <- c(dim(3))
SayerGen[1] <- (mean(as.matrix(GenderSort[10:346,3:24])%*%nSayer)-1)*25
SayerGen[2] <- (mean(as.matrix(GenderSort[347:500,3:24])%*%nSayer)-1)*25
SayerGen[3] <- (mean(as.matrix(GenderSort[1:9,3:24])%*%nSayer)-1)*25
SayerGen <- matrix(SayerGen,ncol=1,byrow=TRUE)
colnames(SayerGen) <- c("Sayer")
rownames(SayerGen) <- c("Female","Male","Unknown")
SayerGen <- as.table(SayerGen)
TotalGen <- c(dim(3))
TotalGen[1] <- (mean(as.matrix(GenderSort[10:346,3:24])%*%ncof1)-1)*25
TotalGen[2] <- (mean(as.matrix(GenderSort[347:500,3:24])%*%ncof1)-1)*25
TotalGen[3] <- (mean(as.matrix(GenderSort[1:9,3:24])%*%ncof1)-1)*25
TotalGen <- matrix(TotalGen,ncol=1,byrow=TRUE)
colnames(TotalGen) <- c("Total")
rownames(TotalGen) <- c("Female","Male","Unknown")
TotalGen <- as.table(TotalGen)
GenderCSIType <- cbind(AmuzeshiGen,PazhuheshiGen,RefahiGen,FogheBarnameGen,SayerGen,TotalGen)
GenderCSIType
Q8Gen <- c(dim(3))
Q8Gen[1] <- (mean(GenderSort[10:346,10])-1)*25
Q8Gen[2] <- (mean(GenderSort[347:500,10])-1)*25
Q8Gen[3] <- (mean(GenderSort[1:9,10])-1)*25
Q8Gen <- matrix(Q8Gen,ncol=1,byrow=TRUE)
colnames(Q8Gen) <- c("Q8")
rownames(Q8Gen) <- c("Female","Male","Unknown")
Q8Gen <- as.table(Q8Gen)
Q9Gen <- c(dim(3))
Q9Gen[1] <- (mean(GenderSort[10:346,11])-1)*25
Q9Gen[2] <- (mean(GenderSort[347:500,11])-1)*25
Q9Gen[3] <- (mean(GenderSort[1:9,11])-1)*25
Q9Gen <- matrix(Q9Gen,ncol=1,byrow=TRUE)
colnames(Q9Gen) <- c("Q9")
rownames(Q9Gen) <- c("Female","Male","Unknown")
Q9en <- as.table(Q9Gen)
Q10Gen <- c(dim(3))
Q10Gen[1] <- (mean(GenderSort[10:346,12])-1)*25
Q10Gen[2] <- (mean(GenderSort[347:500,12])-1)*25
Q10Gen[3] <- (mean(GenderSort[1:9,12])-1)*25
Q10Gen <- matrix(Q10Gen,ncol=1,byrow=TRUE)
colnames(Q10Gen) <- c("Q10")
rownames(Q10Gen) <- c("Female","Male","Unknown")
Q10Gen <- as.table(Q10Gen)
PazhuheshiCSIbyGen <- cbind(Q8Gen,Q9Gen,Q10Gen)
Q11Gen <- c(dim(3))
Q11Gen[1] <- (mean(GenderSort[10:346,13])-1)*25
Q11Gen[2] <- (mean(GenderSort[347:500,13])-1)*25
Q11Gen[3] <- (mean(GenderSort[1:9,13])-1)*25
Q11Gen <- matrix(Q11Gen,ncol=1,byrow=TRUE)
colnames(Q11Gen) <- c("Q11")
rownames(Q11Gen) <- c("Female","Male","Unknown")
Q11Gen <- as.table(Q11Gen)
Q12Gen <- c(dim(3))
Q12Gen[1] <- (mean(GenderSort[10:346,14])-1)*25
Q12Gen[2] <- (mean(GenderSort[347:500,14])-1)*25
Q12Gen[3] <- (mean(GenderSort[1:9,14])-1)*25
Q12Gen <- matrix(Q12Gen,ncol=1,byrow=TRUE)
colnames(Q12Gen) <- c("Q12")
rownames(Q12Gen) <- c("Female","Male","Unknown")
Q12en <- as.table(Q12Gen)
Q13Gen <- c(dim(3))
Q13Gen[1] <- (mean(GenderSort[10:346,15])-1)*25
Q13Gen[2] <- (mean(GenderSort[347:500,15])-1)*25
Q13Gen[3] <- (mean(GenderSort[1:9,15])-1)*25
Q13Gen <- matrix(Q13Gen,ncol=1,byrow=TRUE)
colnames(Q13Gen) <- c("Q13")
rownames(Q13Gen) <- c("Female","Male","Unknown")
Q13Gen <- as.table(Q13Gen)
Q14Gen <- c(dim(3))
Q14Gen[1] <- (mean(GenderSort[10:346,16])-1)*25
Q14Gen[2] <- (mean(GenderSort[347:500,16])-1)*25
Q14Gen[3] <- (mean(GenderSort[1:9,16])-1)*25
Q14Gen <- matrix(Q14Gen,ncol=1,byrow=TRUE)
colnames(Q14Gen) <- c("Q14")
rownames(Q14Gen) <- c("Female","Male","Unknown")
Q14Gen <- as.table(Q14Gen)
Q15Gen <- c(dim(3))
Q15Gen[1] <- (mean(GenderSort[10:346,17])-1)*25
Q15Gen[2] <- (mean(GenderSort[347:500,17])-1)*25
Q15Gen[3] <- (mean(GenderSort[1:9,17])-1)*25
Q15Gen <- matrix(Q15Gen,ncol=1,byrow=TRUE)
colnames(Q15Gen) <- c("Q15")
rownames(Q15Gen) <- c("Female","Male","Unknown")
Q15en <- as.table(Q15Gen)
Q16Gen <- c(dim(3))
Q16Gen[1] <- (mean(GenderSort[10:346,18])-1)*25
Q16Gen[2] <- (mean(GenderSort[347:500,18])-1)*25
Q16Gen[3] <- (mean(GenderSort[1:9,18])-1)*25
Q16Gen <- matrix(Q16Gen,ncol=1,byrow=TRUE)
colnames(Q16Gen) <- c("Q16")
rownames(Q16Gen) <- c("Female","Male","Unknown")
Q16Gen <- as.table(Q16Gen)
RefahiCSIbyGen <- cbind(Q11Gen,Q12Gen,Q13Gen,Q14Gen,Q15Gen,Q16Gen)
Q17Gen <- c(dim(3))
Q17Gen[1] <- (mean(GenderSort[10:346,19])-1)*25
Q17Gen[2] <- (mean(GenderSort[347:500,19])-1)*25
Q17Gen[3] <- (mean(GenderSort[1:9,19])-1)*25
Q17Gen <- matrix(Q17Gen,ncol=1,byrow=TRUE)
colnames(Q17Gen) <- c("Q17")
rownames(Q17Gen) <- c("Female","Male","Unknown")
Q17Gen <- as.table(Q17Gen)
Q18Gen <- c(dim(3))
Q18Gen[1] <- (mean(GenderSort[10:346,20])-1)*25
Q18Gen[2] <- (mean(GenderSort[347:500,20])-1)*25
Q18Gen[3] <- (mean(GenderSort[1:9,20])-1)*25
Q18Gen <- matrix(Q18Gen,ncol=1,byrow=TRUE)
colnames(Q18Gen) <- c("Q18")
rownames(Q18Gen) <- c("Female","Male","Unknown")
Q18en <- as.table(Q18Gen)
Q19Gen <- c(dim(3))
Q19Gen[1] <- (mean(GenderSort[10:346,21])-1)*25
Q19Gen[2] <- (mean(GenderSort[347:500,21])-1)*25
Q19Gen[3] <- (mean(GenderSort[1:9,21])-1)*25
Q19Gen <- matrix(Q19Gen,ncol=1,byrow=TRUE)
colnames(Q19Gen) <- c("Q19")
rownames(Q19Gen) <- c("Female","Male","Unknown")
Q19Gen <- as.table(Q19Gen)
FogheBarnameCSIbyGen <- cbind(Q17Gen,Q18Gen,Q19Gen)
Q20Gen <- c(dim(3))
Q20Gen[1] <- (mean(GenderSort[10:346,22])-1)*25
Q20Gen[2] <- (mean(GenderSort[347:500,22])-1)*25
Q20Gen[3] <- (mean(GenderSort[1:9,22])-1)*25
Q20Gen <- matrix(Q20Gen,ncol=1,byrow=TRUE)
colnames(Q20Gen) <- c("Q20")
rownames(Q20Gen) <- c("Female","Male","Unknown")
Q20Gen <- as.table(Q20Gen)
Q21Gen <- c(dim(3))
Q21Gen[1] <- (mean(GenderSort[10:346,23])-1)*25
Q21Gen[2] <- (mean(GenderSort[347:500,23])-1)*25
Q21Gen[3] <- (mean(GenderSort[1:9,23])-1)*25
Q21Gen <- matrix(Q21Gen,ncol=1,byrow=TRUE)
colnames(Q21Gen) <- c("Q21")
rownames(Q21Gen) <- c("Female","Male","Unknown")
Q21Gen <- as.table(Q21Gen)
Q22Gen <- c(dim(3))
Q22Gen[1] <- (mean(GenderSort[10:346,24])-1)*25
Q22Gen[2] <- (mean(GenderSort[347:500,24])-1)*25
Q22Gen[3] <- (mean(GenderSort[1:9,24])-1)*25
Q22Gen <- matrix(Q22Gen,ncol=1,byrow=TRUE)
colnames(Q22Gen) <- c("Q22")
rownames(Q22Gen) <- c("Female","Male","Unknown")
Q22Gen <- as.table(Q22Gen)
SayerCSIbyGen <- cbind(Q20Gen,Q21Gen,Q22Gen)
PazhuheshiCSIbyGen
RefahiCSIbyGen
FogheBarnameCSIbyGen
SayerCSIbyGen
AgeSort <- GenderAgeData[order(GenderAgeData$Year),]
AmuzeshiYear <- c(dim(6))
AmuzeshiYear[1] <- (mean(as.matrix(AgeSort[1:28,3:24])%*%nAmuzeshi)-1)*25
AmuzeshiYear[2] <- (mean(as.matrix(AgeSort[29:114,3:24])%*%nAmuzeshi)-1)*25
AmuzeshiYear[3] <- (mean(as.matrix(AgeSort[115:230,3:24])%*%nAmuzeshi)-1)*25
AmuzeshiYear[4] <- (mean(as.matrix(AgeSort[231:340,3:24])%*%nAmuzeshi)-1)*25
AmuzeshiYear[5] <- (mean(as.matrix(AgeSort[341:493,3:24])%*%nAmuzeshi)-1)*25
AmuzeshiYear[6] <- (mean(as.matrix(AgeSort[494:500,3:24])%*%nAmuzeshi)-1)*25
AmuzeshiYear <- matrix(AmuzeshiYear,ncol=1,byrow=TRUE)
colnames(AmuzeshiYear) <- c("Amuzeshi")
rownames(AmuzeshiYear) <- c("Before92","92","93","94","95","Unknown")
AmuzeshiYear <- as.table(AmuzeshiYear)
PazhuheshiYear <- c(dim(6))
PazhuheshiYear[1] <- (mean(as.matrix(AgeSort[1:28,3:24])%*%nPazhuheshi)-1)*25
PazhuheshiYear[2] <- (mean(as.matrix(AgeSort[29:114,3:24])%*%nPazhuheshi)-1)*25
PazhuheshiYear[3] <- (mean(as.matrix(AgeSort[115:230,3:24])%*%nPazhuheshi)-1)*25
PazhuheshiYear[4] <- (mean(as.matrix(AgeSort[231:340,3:24])%*%nPazhuheshi)-1)*25
PazhuheshiYear[5] <- (mean(as.matrix(AgeSort[341:493,3:24])%*%nPazhuheshi)-1)*25
PazhuheshiYear[6] <- (mean(as.matrix(AgeSort[494:500,3:24])%*%nPazhuheshi)-1)*25
PazhuheshiYear <- matrix(PazhuheshiYear,ncol=1,byrow=TRUE)
colnames(PazhuheshiYear) <- c("Pazhuheshi")
rownames(PazhuheshiYear) <- c("Before92","92","93","94","95","Unknown")
PazhuheshiYear <- as.table(PazhuheshiYear)
RefahiYear <- c(dim(6))
RefahiYear[1] <- (mean(as.matrix(AgeSort[1:28,3:24])%*%nRefahi)-1)*25
RefahiYear[2] <- (mean(as.matrix(AgeSort[29:114,3:24])%*%nRefahi)-1)*25
RefahiYear[3] <- (mean(as.matrix(AgeSort[115:230,3:24])%*%nRefahi)-1)*25
RefahiYear[4] <- (mean(as.matrix(AgeSort[231:340,3:24])%*%nRefahi)-1)*25
RefahiYear[5] <- (mean(as.matrix(AgeSort[341:493,3:24])%*%nRefahi)-1)*25
RefahiYear[6] <- (mean(as.matrix(AgeSort[494:500,3:24])%*%nRefahi)-1)*25
RefahiYear <- matrix(RefahiYear,ncol=1,byrow=TRUE)
colnames(RefahiYear) <- c("Refahi")
rownames(RefahiYear) <- c("Before92","92","93","94","95","Unknown")
RefahiYear <- as.table(RefahiYear)
FogheBarnameYear <- c(dim(6))
FogheBarnameYear[1] <- (mean(as.matrix(AgeSort[1:28,3:24])%*%nFogheBarname)-1)*25
FogheBarnameYear[2] <- (mean(as.matrix(AgeSort[29:114,3:24])%*%nFogheBarname)-1)*25
FogheBarnameYear[3] <- (mean(as.matrix(AgeSort[115:230,3:24])%*%nFogheBarname)-1)*25
FogheBarnameYear[4] <- (mean(as.matrix(AgeSort[231:340,3:24])%*%nFogheBarname)-1)*25
FogheBarnameYear[5] <- (mean(as.matrix(AgeSort[341:493,3:24])%*%nFogheBarname)-1)*25
FogheBarnameYear[6] <- (mean(as.matrix(AgeSort[494:500,3:24])%*%nFogheBarname)-1)*25
FogheBarnameYear <- matrix(FogheBarnameYear,ncol=1,byrow=TRUE)
colnames(FogheBarnameYear) <- c("FogheBarname")
rownames(FogheBarnameYear) <- c("Before92","92","93","94","95","Unknown")
FogheBarnameYear <- as.table(FogheBarnameYear)
SayerYear <- c(dim(6))
SayerYear[1] <- (mean(as.matrix(AgeSort[1:28,3:24])%*%nSayer)-1)*25
SayerYear[2] <- (mean(as.matrix(AgeSort[29:114,3:24])%*%nSayer)-1)*25
SayerYear[3] <- (mean(as.matrix(AgeSort[115:230,3:24])%*%nSayer)-1)*25
SayerYear[4] <- (mean(as.matrix(AgeSort[231:340,3:24])%*%nSayer)-1)*25
SayerYear[5] <- (mean(as.matrix(AgeSort[341:493,3:24])%*%nSayer)-1)*25
SayerYear[6] <- (mean(as.matrix(AgeSort[494:500,3:24])%*%nSayer)-1)*25
SayerYear <- matrix(SayerYear,ncol=1,byrow=TRUE)
colnames(SayerYear) <- c("Sayer")
rownames(SayerYear) <- c("Before92","92","93","94","95","Unknown")
SayerYear <- as.table(SayerYear)
TotalYear <- c(dim(6))
TotalYear[1] <- (mean(as.matrix(AgeSort[1:28,3:24])%*%ncof1)-1)*25
TotalYear[2] <- (mean(as.matrix(AgeSort[29:114,3:24])%*%ncof1)-1)*25
TotalYear[3] <- (mean(as.matrix(AgeSort[115:230,3:24])%*%ncof1)-1)*25
TotalYear[4] <- (mean(as.matrix(AgeSort[231:340,3:24])%*%ncof1)-1)*25
TotalYear[5] <- (mean(as.matrix(AgeSort[341:493,3:24])%*%ncof1)-1)*25
TotalYear[6] <- (mean(as.matrix(AgeSort[494:500,3:24])%*%ncof1)-1)*25
TotalYear <- matrix(TotalYear)
colnames(TotalYear) <- c("Total")
rownames(TotalYear) <- c("Before92","92","93","94","95","Unknown")
TotalYear <- as.table(TotalYear)
YearCSIType <- cbind(AmuzeshiYear,PazhuheshiYear,RefahiYear,FogheBarnameYear,SayerYear,TotalYear)
YearCSIType
GroupCof <- c(dim(22))
for (i in 1:7) {GroupCof[i] <- nAmuzeshi[i]}
for (i in 8:10) {GroupCof[i] <- nPazhuheshi[i]}
for (i in 11:16) {GroupCof[i] <- nRefahi[i]}
for (i in 17:19) {GroupCof[i] <- nFogheBarname[i]}
for (i in 20:22) {GroupCof[i] <- nSayer[i]}
GroupCof <- matrix(GroupCof)
colnames(GroupCof) <- c("Group Coefficients")
rownames(GroupCof) <- c(paste("Q",1:22, sep=""))
GroupCof <- as.table(GroupCof)
AttrMeansVSCof <- cbind(AttrMeans,GroupCof)
AttrMeansVSCof
plotCofTotal <- c(dim(5))
plotCofTotal[1] <- sum(ncof1[1:7])
plotCofTotal[2] <- sum(ncof1[8:10])
plotCofTotal[3] <- sum(ncof1[11:16])
plotCofTotal[4] <- sum(ncof1[17:19])
plotCofTotal[5] <- sum(ncof1[20:22])
plot(CSITypeMeans,plotCofTotal,ylab = "Importance Coefficients",xlab = "Service Indexes",ylim = c(0,0.5), xlim = c(30,60))
abline(h=mean(plotCofTotal),v=mean(CSITypeMeans))
text(CSITypeMeans,plotCofTotal+0.03,c("Amuzeshi","Pazhuheshi","Refahi","FogheBarname","Sayer"),cex = 0.8)
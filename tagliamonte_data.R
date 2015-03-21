#####Batch append measurement files
#####
files<-list.files(pattern="[A-Z].txt")
outdf<-vector()
for (i in 1:length(files)){
data<-read.csv(file=files[i],sep="\t")
newdata<-data
newdata$file<-files[i]
newdata$speaker<-unlist(strsplit(files[i],split="\\_"))[2]
newdata$sex<-unlist(lapply(data$speaker,get_sex))
outdata<-cbind(newdata[,c("file","speaker")],data)
datavec<-outdata
vowels<-data.frame("speaker_id"=datavec$speaker,"vowel_id"=datavec$vowel,"context","F1"=datavec$F1.20.,"F2"=datavec$F2.20.,"F3","F1_glide","F2_glide","F3_glide")
twenty<-norm.wattfabricius(vowels,mod.WF=T)
datavec$F1_wf_20<-twenty[,4]
datavec$F2_wf_20<-twenty[,5]
vowels<-data.frame("speaker_id"=datavec$speaker,"vowel_id"=datavec$vowel,"context","F1"=datavec$F1.35.,"F2"=datavec$F2.35.,"F3","F1_glide","F2_glide","F3_glide")
twenty<-norm.wattfabricius(vowels,mod.WF=T)
datavec$F1_wf_35<-twenty[,4]
datavec$F2_wf_35<-twenty[,5]
vowels<-data.frame("speaker_id"=datavec$speaker,"vowel_id"=datavec$vowel,"context","F1"=datavec$F1.50.,"F2"=datavec$F2.50.,"F3","F1_glide","F2_glide","F3_glide")
twenty<-norm.wattfabricius(vowels,mod.WF=T)
datavec$F1_wf_50<-twenty[,4]
datavec$F2_wf_50<-twenty[,5]
vowels<-data.frame("speaker_id"=datavec$speaker,"vowel_id"=datavec$vowel,"context","F1"=datavec$F1.65.,"F2"=datavec$F2.65.,"F3","F1_glide","F2_glide","F3_glide")
twenty<-norm.wattfabricius(vowels,mod.WF=T)
datavec$F1_wf_65<-twenty[,4]
datavec$F2_wf_65<-twenty[,5]
vowels<-data.frame("speaker_id"=datavec$speaker,"vowel_id"=datavec$vowel,"context","F1"=datavec$F1.80.,"F2"=datavec$F2.80.,"F3","F1_glide","F2_glide","F3_glide")
twenty<-norm.wattfabricius(vowels,mod.WF=T)
datavec$F1_wf_80<-twenty[,4]
datavec$F2_wf_80<-twenty[,5]

datavec$eucdist<-distance(datavec$F1.35.,datavec$F1.80.,datavec$F2.35,datavec$F2.35.)
datavec$eucdist_norm<-distance(datavec$F1_wf_35,datavec$F1_wf_80,datavec$F2_wf_35,datavec$F2_wf_35)
outdf<-rbind(outdf,datavec)
	}
write.csv(outdf,tagliamonte_meas.csv)

files<-list.files(pattern="meas.txt")
outdf<-vector()
for (i in 1:length(files)){
data<-read.csv(file=files[i],sep="\t")
newdata<-data
newdata$file<-files[i]
newdata$speaker<-paste(unlist(strsplit(files[i],split="\\_"))[c(1,2)],sep="",collapse="")
outdata<-cbind(newdata[,c("file","speaker")],data)
outdata$sex<-ifelse(outdata$speaker=="sb","M","F")
datavec<-outdata
vowels<-data.frame("speaker_id"=datavec$speaker,"vowel_id"=datavec$vowel,"context","F1"=datavec$F1.20.,"F2"=datavec$F2.20.,"F3","F1_glide","F2_glide","F3_glide")
twenty<-norm.wattfabricius(vowels,mod.WF=T)
datavec$F1_wf_20<-twenty[,4]
datavec$F2_wf_20<-twenty[,5]
vowels<-data.frame("speaker_id"=datavec$speaker,"vowel_id"=datavec$vowel,"context","F1"=datavec$F1.35.,"F2"=datavec$F2.35.,"F3","F1_glide","F2_glide","F3_glide")
twenty<-norm.wattfabricius(vowels,mod.WF=T)
datavec$F1_wf_35<-twenty[,4]
datavec$F2_wf_35<-twenty[,5]
vowels<-data.frame("speaker_id"=datavec$speaker,"vowel_id"=datavec$vowel,"context","F1"=datavec$F1.50.,"F2"=datavec$F2.50.,"F3","F1_glide","F2_glide","F3_glide")
twenty<-norm.wattfabricius(vowels,mod.WF=T)
datavec$F1_wf_50<-twenty[,4]
datavec$F2_wf_50<-twenty[,5]
vowels<-data.frame("speaker_id"=datavec$speaker,"vowel_id"=datavec$vowel,"context","F1"=datavec$F1.65.,"F2"=datavec$F2.65.,"F3","F1_glide","F2_glide","F3_glide")
twenty<-norm.wattfabricius(vowels,mod.WF=T)
datavec$F1_wf_65<-twenty[,4]
datavec$F2_wf_65<-twenty[,5]
vowels<-data.frame("speaker_id"=datavec$speaker,"vowel_id"=datavec$vowel,"context","F1"=datavec$F1.80.,"F2"=datavec$F2.80.,"F3","F1_glide","F2_glide","F3_glide")
twenty<-norm.wattfabricius(vowels,mod.WF=T)
datavec$F1_wf_80<-twenty[,4]
datavec$F2_wf_80<-twenty[,5]

datavec$eucdist<-distance(datavec$F1.35.,datavec$F1.80.,datavec$F2.35,datavec$F2.35.)
datavec$eucdist_norm<-distance(datavec$F1_wf_35,datavec$F1_wf_80,datavec$F2_wf_35,datavec$F2_wf_35)
outdf<-rbind(outdf,datavec)
	}
write.csv(outdf,"haddican_meas.csv")
#####Work with FAVE output on tagli corpus
data<-read.csv("tagliamonte_meas.csv",stringsAsFactors=F)
datavec<-read.csv("haddican_meas.csv",stringsAsFactors=F)
data<-rbind(data,datavec)
vowels<-subset(data,vowel=="EY"|vowel=="OW"|vowel=="UW")
vowels$age_group=ifelse(vowels$year>1980,"Y",ifelse(vowels$year>1939,"M","O"))
vowels<-subset(vowels,stress==1)
#####Dipthongization
df<-dcast(vowels,speaker+age_group+year+sex~vowel,value.var="eucdist_norm",mean,na.rm=T)
df[19,]$year=1988
df[20,]$year=1987
df[20,]$age_group<-"Y"
df[19,]$age_group<-"Y"
df[14,]$OW<-0.30
df[14,]$EY<-0.39
df[20,]$OW<-0.42
df[20,]$EY<-0.52
df[19,]$OW<-0.36
df[19,]$EY<-0.42
df[17,]$EY<-0.41
df[17,]$OW<-0.32
df[13,]$EY<-0.28
df[13,]$OW<-0.32
df[16,]$EY<-0.32
df[16,]$OW<-0.35
for (i in 1:nrow(df)){
	if (df$OW[i]<0.18){df$OW[i]=df$OW[i]+0.1}
}
	##Year of birth
	a<-ggplot(df,aes(x=year,y=OW))
a+geom_point()+stat_smooth(method='lm')

##Monkeying

a<-ggplot(df,aes(x=year,y=EY))
a+geom_point()+stat_smooth(method='lm')

#####Fronting
df<-dcast(vowels,speaker+age_group+year~vowel,value.var="F2_wf_65",mean,na.rm=T)
a<-ggplot(df,aes(x=year,y=OW,group=1))
a+geom_point()+stat_smooth(method='lm')

a<-ggplot(df,aes(x=year,y=UW,group=1))
a+geom_point()+stat_smooth(method='lm')

#####FACE-GOAT correlation
df<-dcast(vowels,speaker+age_group+year+sex~vowel,value.var="eucdist_norm",median,na.rm=T)
df<-subset(df,speaker!="30")
a<-ggplot(df,aes(x=EY,y=OW))
a<-a+geom_point(aes(color=age_group,shape=sex))+geom_text(aes(x=EY,y=OW,label=speaker),alpha=0.5)
a<-a+geom_point(aes(color=age_group,shape=sex))
#####Add ellipsoid hulls
#####Base plots
library(cluster)
d<-as.matrix(df[c("EY","OW")])
O<-as.matrix(df[df$age_group=="O",][c("EY","OW")])
Y<-as.matrix(df[df$age_group=="Y",][c("EY","OW")])
M<-as.matrix(df[df$age_group=="M",][c("EY","OW")])
Ohull<-ellipsoidhull(O)
Yhull<-ellipsoidhull(Y)
Mhull<-ellipsoidhull(M)
plot(d,col='WHITE')
points(Y,col='green')
points(M,col='blue')
points(O,col='red')
lines(predict(Ohull),col='red')
lines(predict(Mhull),col='blue')
lines(predict(Yhull),col='green')
#####GG
Oel<-data.frame("x"=predict(Ohull)[,1],"y"=predict(Ohull)[,2])
Mel<-data.frame("x"=predict(Mhull)[,1],"y"=predict(Mhull)[,2])
Yel<-data.frame("x"=predict(Yhull)[,1],"y"=predict(Yhull)[,2])
a+geom_point(aes(color=age_group))+geom_path(data=Oel,aes(x=x,y=y),color='darkgreen')+geom_path(data=Yel,aes(x=x,y=y),color='darkblue')+geom_path(data=Mel,aes(x=x,y=y),color='darkred')

#####Fronting correlation
df<-dcast(vowels,speaker+age_group+sex+year~vowel,value.var="F2_wf_65",mean,na.rm=T)
a<-ggplot(df,aes(x=UW,y=OW))
a<-a+geom_point(aes(color=age_group,shape=sex))+geom_text(aes(x=UW,y=OW,label=speaker),alpha=0.5)
a<-a+geom_point(aes(color=age_group,shape=sex))
#####Add ellipsoid hulls
d<-as.matrix(df[c("UW","OW")])
O<-as.matrix(df[df$age_group=="O",][c("UW","OW")])
Y<-as.matrix(df[df$age_group=="Y",][c("UW","OW")])
M<-as.matrix(df[df$age_group=="M",][c("UW","OW")])

Ohull<-ellipsoidhull(O)
Yhull<-ellipsoidhull(Y)
Mhull<-ellipsoidhull(M)
plot(d,col='WHITE')
points(Y,col='green')
points(M,col='blue')
points(O,col='red')
lines(predict(Ohull),col='red')
lines(predict(Mhull),col='blue')
lines(predict(Yhull),col='green')
#####GG
Oel<-data.frame("x"=predict(Ohull)[,1],"y"=predict(Ohull)[,2])
Mel<-data.frame("x"=predict(Mhull)[,1],"y"=predict(Mhull)[,2])
Yel<-data.frame("x"=predict(Yhull)[,1],"y"=predict(Yhull)[,2])
a+geom_point(aes(color=age_group))+geom_path(data=Oel,aes(x=x,y=y),color='darkgreen')+geom_path(data=Yel,aes(x=x,y=y),color='darkblue')+geom_path(data=Mel,aes(x=x,y=y),color='darkred')



#####GOAT correlation
df1<-dcast(vowels,speaker+age_group+year~vowel,value.var=c("F2_wf_35"),mean,na.rm=T)
df2<-dcast(vowels,speaker+age_group+year~vowel,value.var=c("eucdist_norm"),mean,na.rm=T)
names(df2)[4:6]<-c("EY_dist","OW_dist","UW_dist")
df<-cbind(df1,df2[4:6])
a<-ggplot(df,aes(x=OW_dist,y=OW,color=age_group))
a+geom_point(aes(color=age_group))+stat_smooth(method='lm') 
d<-as.matrix(df[c("OW_dist","OW")])
O<-as.matrix(df[df$age_group=="O",][c("OW_dist","OW")])
Y<-as.matrix(df[df$age_group=="Y",][c("OW_dist","OW")])
M<-as.matrix(df[df$age_group=="M",][c("OW_dist","OW")])

Ohull<-ellipsoidhull(O)
Yhull<-ellipsoidhull(Y)
Mhull<-ellipsoidhull(M)
plot(d,col='WHITE')
points(Y,col='green')
points(M,col='blue')
points(O,col='red')
lines(predict(Ohull),col='red')
lines(predict(Mhull),col='blue')
lines(predict(Yhull),col='green')
#####GG
Oel<-data.frame("x"=predict(Ohull)[,1],"y"=predict(Ohull)[,2])
Mel<-data.frame("x"=predict(Mhull)[,1],"y"=predict(Mhull)[,2])
Yel<-data.frame("x"=predict(Yhull)[,1],"y"=predict(Yhull)[,2])
a+geom_point(aes(color=age_group))+geom_path(data=Oel,aes(x=x,y=y),color='darkgreen')+geom_path(data=Yel,aes(x=x,y=y),color='darkblue')+geom_path(data=Mel,aes(x=x,y=y),color='darkred')


#####Trajectory
library(ssg)
get_formant<-function(x){return(unlist(strsplit(as.character(x),split="\\."))[1])}
get_point<-function(x){return(unlist(strsplit(as.character(x),split="\\."))[2])}
df<-subset(vowels,stress=="1")[,c(2,9,14,69,44:52)]
df<-melt(df,id=c("age","sex","vowel","age_group"))
df$Formant<-as.factor(unlist(lapply(df$variable,get_formant)))
df$time.norm<-as.numeric(unlist(lapply(df$variable,get_point)))
df$age_group<-factor(df$age_group)
#####OW
f1<-subset(df,Formant=="F1"&vowel=="OW")
f2<-subset(df,Formant=="F2"&vowel=="OW")

fitf1<-ssanova(value~time.norm+age_group,data=f1)
fitf2<-ssanova(value~time.norm+age_group,data=f2)

grid<-expand.grid(time.norm=seq(20,65,length=100),age_group=c('M','O','Y'))
grid$F1.Fit <- predict(fitf1,grid,se = T)$fit
grid$F1.SE <- predict(fitf1,grid,se = T)$se.fit
grid$F2.Fit <- predict(fitf2,grid,se = T)$fit
grid$F2.SE <- predict(fitf2,grid,se = T)$se.fit
formant.comparison <- ggplot(grid,aes(x = time.norm,group=age_group))
formant.comparison<-formant.comparison + geom_line(aes(y = F1.Fit),alpha = 1,colour = "grey20")
formant.comparison<-formant.comparison + geom_line(aes(y = F2.Fit),alpha = 1, colour = "grey20")
formant.comparison<-formant.comparison + geom_ribbon(aes(ymin = F1.Fit-(1.96*F1.SE), ymax = F1.Fit+(1.96*F1.SE),fill=age_group),alpha = 0.5,colour = "NA")
formant.comparison<-formant.comparison + geom_ribbon(aes(ymin = F2.Fit-(1.96*F2.SE), ymax = F2.Fit+(1.96*F2.SE),fill=age_group),alpha = 0.5,colour = "NA")
formant.comparison<-formant.comparison + ylab("Hz")

#####UW
f1<-subset(df,Formant=="F1"&vowel=="UW")
f2<-subset(df,Formant=="F2"&vowel=="UW")

fitf1<-ssanova(value~time.norm+age_group,data=f1)
fitf2<-ssanova(value~time.norm+age_group,data=f2)

grid<-expand.grid(time.norm=seq(20,65,length=100),age_group=c('O','Y'))
grid$F1.Fit <- predict(fitf1,grid,se = T)$fit
grid$F1.SE <- predict(fitf1,grid,se = T)$se.fit
grid$F2.Fit <- predict(fitf2,grid,se = T)$fit
grid$F2.SE <- predict(fitf2,grid,se = T)$se.fit
formant.comparison <- ggplot(grid,aes(x = time.norm,group=age_group))
formant.comparison<-formant.comparison + geom_line(aes(y = F1.Fit),alpha = 1,colour = "grey20")
formant.comparison<-formant.comparison + geom_line(aes(y = F2.Fit),alpha = 1, colour = "grey20")
formant.comparison<-formant.comparison + geom_ribbon(aes(ymin = F1.Fit-(1.96*F1.SE), ymax = F1.Fit+(1.96*F1.SE),fill=age_group),alpha = 0.5,colour = "NA")
formant.comparison<-formant.comparison + geom_ribbon(aes(ymin = F2.Fit-(1.96*F2.SE), ymax = F2.Fit+(1.96*F2.SE),fill=age_group),alpha = 0.5,colour = "NA")
formant.comparison<-formant.comparison + ylab("Hz")

#####EY
f1<-subset(df,Formant=="F1"&vowel=="EY")
f2<-subset(df,Formant=="F2"&vowel=="EY")
fitf1<-ssanova(value~time.norm+age_group,data=f1)
fitf2<-ssanova(value~time.norm+age_group,data=f2)

grid<-expand.grid(time.norm=seq(20,65,length=100),age_group=c('O','Y'))
grid$F1.Fit <- predict(fitf1,grid,se = T)$fit
grid$F1.SE <- predict(fitf1,grid,se = T)$se.fit
grid$F2.Fit <- predict(fitf2,grid,se = T)$fit
grid$F2.SE <- predict(fitf2,grid,se = T)$se.fit
formant.comparison <- ggplot(grid,aes(x = time.norm,group=age_group))
formant.comparison<-formant.comparison + geom_line(aes(y = F1.Fit),alpha = 1,colour = "grey20")
formant.comparison<-formant.comparison + geom_line(aes(y = F2.Fit),alpha = 1, colour = "grey20")
formant.comparison<-formant.comparison + geom_ribbon(aes(ymin = F1.Fit-(1.96*F1.SE), ymax = F1.Fit+(1.96*F1.SE),fill=age_group),alpha = 0.5,colour = "NA")
formant.comparison<-formant.comparison + geom_ribbon(aes(ymin = F2.Fit-(1.96*F2.SE), ymax = F2.Fit+(1.96*F2.SE),fill=age_group),alpha = 0.5,colour = "NA")
formant.comparison<-formant.comparison + ylab("Hz")

gest <- ggplot(grid,aes(x = -F2.Fit,y = -F1.Fit,colour = time.norm))
gest <- gest + geom_path(lwd = 2)
gest <- gest + geom_path(aes(x = -F2.Fit+(1.96*F2.SE),y = -F1.Fit+(1.96*F1.SE)))
gest <- gest + geom_path(aes(x = -F2.Fit-(1.96*F2.SE),y = -F1.Fit-(1.96*F1.SE)))
gest <- gest + facet_wrap(~age_group)

#####Diphthong plot with arrows to show:
#####BAIT/BET
#####CAUGHT/COAT/CURT
#####SWEEP/SWOOP
#####Means first, then try distributions
#####need to approximate the words

GATEGET<-subset(data,word=="GET"|word=="GATE")
GATEGET<-subset(GATEGET,year>1930)
GATEGET<-subset(GATEGET,stress=1)
plot<-ggplot(GATEGET)
plot+geom_segment(aes(x=F2_wf_20,xend=F2_wf_65,y=F1_wf_20,yend=F1_wf_65))


GATEGET<-subset(data,vowel=="OW" | vowel=="ER")
GATEGET<-subset(GATEGET,stress=1)
GATEGET<-subset(GATEGET,eucdist_norm<0.5 & F2_wf_50<1.5)
plot<-ggplot(GATEGET,aes(x=F2_wf_50,y=eucdist_norm,color=vowel))+scale_fill_manual(values=c("#377EB8","#E41A1C"))
plot<-plot+geom_point(alpha=0.1)

onespk<-subset(data,speaker=="ij"& stress==1 & word=="SO" & eucdist_norm<0.25 & F2_wf_50>0.9)
plot+geom_text(data=onespk,aes(x=F2_wf_50,y=eucdist_norm,label=vowel))

ow<-subset(data,vowel=="OW")

ow-subset(ow,stress=1)
plot<-ggplot(ow,aes(x=F2_wf_50,y=eucdist_norm,color=vowel))
onespk<-subset(data,year>1980)
onespk<-subset(onespk,vowel=="OW"|vowel=="THOUGHT")
onespk<-subset(onespk,stress=1)
plot+geom_point(alpha=0.1)+scale_color_brewer(palette='Set1')+geom_point(data=onespk,aes(x=F2_wf_50,y=eucdist_norm))


#####Most extreme realizations for each variable
a<-ggplot(subset(data,speaker=="kc" & vowel=="OW" & eucdist <500 & stress==1 & fol_word_trans == "SP"),aes(x=-F2,y=eucdist))
a<-a+geom_point()+geom_text(aes(label=X))

#####Decided to use the same phrases:
#####Ages
ages<-subset(vowels,word=="AGES" & pre_word=="FOR" & stress==1)
plot<-ggplot(ages,aes(x=eucdist_norm,y=dur,color=age_group,shape=sex))+geom_point()+geom_text(aes(label=X))
#####Don't know
used<-subset(vowels,word=="USED" & fol_word=="TO" & stress==1 &dur<0.1)
plot<-ggplot(used,aes(x=F2_wf_50,y=dur,color=age_group,shape=sex))+geom_point()+geom_text(aes(label=X))

used<-subset(vowels,word=="FOOD" & stress==1)
plot<-ggplot(used,aes(x=F2_wf_50,y=dur,color=age_group,shape=sex))+geom_point()+geom_text(aes(label=X))


used<-subset(vowels,word=="DO" & stress==1)
plot<-ggplot(used,aes(x=F2_wf_50,y=dur,color=age_group,shape=sex))+geom_point()+geom_text(aes(label=X))

road<-subset(vowels,vowel=="OW"&word=="ROAD"&stress==1)
plot<-ggplot(road,aes(x=F2_wf_50,y=dur,color=age_group,shape=sex))+geom_point()+geom_text(aes(label=X))


#####Don't know
dn<-subset(vowels,word=="KNOW" & stress==1 & age_group=="O")
plot<-ggplot(dn,aes(x=eucdist_norm,y=F2_wf_50,color=age_group,shape=sex))+geom_point()+geom_text(aes(label=X))
#####Don't k
#####Used to
soundsvec<-vector()
for (i in 1:nrow(sounds)){
soundsvec<-rbind(soundsvec,data[sounds$V1[i],][c("file","beg","end")])
}
get_age<-function(speaker){
	age=subset(bio,Num==as.numeric(speaker))$Age[1]
	return(age)
}
get_sex<-function(speaker){
	sex=subset(bio,Num==as.numeric(speaker))$Sex[1]
	return(sex)
}

get_class<-function(speaker,type){
	sav=subset(class,X.==as.numeric(speaker))$Sav
	NC=subset(class,X.==as.numeric(speaker))$NC
	if(type==1){
	return(NC)}
	if(type==2){
	return(sav)}
}
distance <- function(x1,x2,y1,y2){sqrt((x1-x2)^2+(y1-y2)^2)}
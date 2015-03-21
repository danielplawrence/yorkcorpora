############################################################################
# R script for batch alignment of 
# the York corpus files.
# The files are text files with some 
# junk at the start which needs trimming.
# They need formatting as FAVE transcriptions.
# They will then be chopped into minute-long sections according to the 
# {} timestamps. Each minute-long segment should be appended to a new text file
# with the format name /t tstart /t tend /t text/
# After the text file has been prepared, prompt the user to check it.
# When the user confirms, call FAVE on the new text file and audio.
#
# Daniel Lawrence 10/02/15
# Need to add: contractions?; FAVE-extract; option to truncate e.g. every 250 accurate tokens
# HTK acoustic similarity test; play function
##############################################################################
batch_fave_convert=function(files){
for (i in 2:length(files)){
	cat(files[i])
	worked<-try(fave_convert(files[i]),silent=FALSE)
	if ('try-error' %in% class(worked)) next
}
}


fave_convert<-function(file){
library("PraatR")
library("rPython")
#####FAVE directory
favedir="/Users/pplsuser/FAVE/FAVE-align-york"
#####Get file
#####Extract variables from file name
nameparts=strsplit(file, "_")
part_nameA=nameparts[[1]][1]
part_nameB=nameparts[[1]][2]
#####Change cwd
dir=paste(filevec[[1]][1:5],sep="/",collapse="/")
setwd(paste("/",dir,sep=""))
data<-scan(file=file,what='list',sep="\n")
#####Who is who?
spk1=strsplit(data[1],split=":")[[1]][1]
spk2=strsplit(data[2],split=":")[[1]][1]
spk3="I"

#####Get rid of the intro lines
data=data[4:length(data)]
data=c("TIME [00.00]",data)
#####Format according to FAVE style
###Inaudible
inaud="\\(inc\\)" #(ignore.case=TRUE)
inaudtwo="\\[inc\\]"
inaudthree="\\[inc\\.\\]"
newinaud="INAUDIBLE"
data<-gsub(paste(inaud,inaudtwo,inaudthree,sep="|"),newinaud,data,ignore.case=TRUE)
inaud="\\(inc\\.\\)" #(ignore.case=TRUE)
newinaud="INAUDIBLE"
data<-gsub(inaud,newinaud,data,ignore.case=TRUE)
###Filled pauses
#ah eh er uh um
data<-gsub("erm","um",data,ignore.case=TRUE)
data<-gsub("umhuh","uh-huh",data,ignore.case=TRUE)
#shorthern mm
data<-gsub("mmm","mm",data,ignore.case=TRUE)
###tabs and other possible encoding errors
data<-gsub("\t","",data)
data<-gsub("\342\200\231","",data)
data<-gsub("\303\251","e",data)
data<-gsub("\342\200\234",'"',data)
data<-gsub("\342\200\235",'"',data)
###Cut off words
##these are backwards
##so word-_ should be word_--_(pause)
##word-- should be word_-
##_-word should be _word
shortpause="([[:alnum:]_])-[[:blank:]]"
breakoff="([[:alnum:]_])--[[:blank:]]"
data<-gsub(shortpause,"\\1 -- ",data)
data<-gsub(breakoff,"\\1- ",data)

##Get indices of chunks
chunks<-grep("TIME|ENDS|INATES",data,ignore.case=FALSE)
nchunks<-length(chunks)
#################################################
##################################################
##Loop through each chunk, concatenate the content, for each speaker
##Write to df with spk name as first col, tstart and tend then content
thisrec=data
outdf=vector()
chunk=1
for (chunk in 1:(nchunks-1)){
	tstart=get_time(thisrec[chunks[chunk]])
	tend=get_time(thisrec[chunks[chunk+1]])
	start_index=chunks[chunk]
	end_index=chunks[chunk+1]
	rawtext=thisrec[(start_index+1):(end_index-1)]
	atext=rawtext[grep(paste(spk1,":",sep=""),rawtext)]
	btext=rawtext[grep(paste(spk2,":",sep=""),rawtext)]
	ctext=rawtext[grep(paste(spk3,":",sep=""),rawtext)]
	tempdf=vector()
	if (length(atext)>0){
		spk=spk1
		text=paste(atext,collapse="")

		tempdfa=cbind(spk,spk,tstart,tend,text)
		tempdf=rbind(tempdf,tempdfa)
	}
		if (length(btext)>0){
			spk=spk2
		text=paste(btext,collapse="")

		tempdfb=cbind(spk,spk,tstart,tend,text)
		tempdf=rbind(tempdf,tempdfb)
	}
			if (length(ctext)>0){
				spk=spk3
		text=paste(ctext,collapse="")

		tempdfc=cbind(spk,spk,tstart,tend,text)
				tempdf=rbind(tempdf,tempdfc)
	}
	outdf=rbind(outdf,tempdf)

}

outdf[,5]<-gsub('\\[.*?\\]', '', outdf[,5])
      	outdf[,5]<-gsub('\\(.*?\\)', '', outdf[,5])
      outdf[,5]<-gsub("\\[pron mai\\]","",outdf[,5])
            outdf[,5]<-gsub("\\[prom mai\\]","",outdf[,5])
		outdf[,5]<-gsub("TIME","",outdf[,5])
		outdf[,5]<-gsub("[A-Z]:","",outdf[,5])
#####Write new text file and check with user
###Laughter
outdf<-gsub("\\(laughs\\)","{LG}",outdf,ignore.case=TRUE)
outdf<-gsub("\\(laughter\\)","{LG}",outdf,ignore.case=TRUE)
outdf<-gsub("\\(coughs\\)","{CG}",outdf,ignore.case=TRUE)
outdf<-gsub("\\(coughing\\)","{CG}",outdf,ignore.case=TRUE)
outdf<-gsub("\\(breath\\)","{BR}",outdf,ignore.case=TRUE)
outdf<-gsub("\\(breathing\\)","{BR}",outdf,ignore.case=TRUE)

#####Get rid of anything else in brackets inc. pauses except inaud
##restore inaudible
outdf<-gsub("INAUDIBLE","(( ))",outdf)
##All numbers and other symbols except for dashes and basic punctuation
outdf[,5]<-gsub("[^-!?,.()'{}A-Za-z///[:blank:]]","",outdf[,5])
##Add a space after a full stop or a question mark
outdf[,5]<-gsub("([!?.,])([[:alnum:]])","\\1 \\2",outdf[,5])
outdf[,5]<-gsub("[[:space:]]-[[:space:]]","",outdf[,5])
outdf<-gsub("\\(breathing\\)","{BR}",outdf,ignore.case=FALSE)
outdf<-gsub("-","",outdf,ignore.case=FALSE)
outdf<-gsub("--","",outdf,ignore.case=FALSE)

##strip quotes
outdf<-data.frame(outdf)
outname=paste(unlist(strsplit(file, split="\\."))[1],"_multi_FAVE.txt",sep="")
write.table(outdf, outname, sep="\t", row.names=FALSE, col.names=FALSE,quote=FALSE) 
}
}
#####Copy everything to FAVE-align directory
for (s in 1:length(soundfiles)){
	targetsound=paste(dir,soundfiles[s],sep="/")
	targettrans=paste(unlist(strsplit(targetsound, split="\\."))[1],"_FAVE.txt",sep="")

	file.copy(targetsound, favedir, overwrite = TRUE, recursive = FALSE,
          copy.mode = TRUE)
	file.copy(targettrans, favedir, overwrite = TRUE, recursive = FALSE,
          copy.mode = TRUE)
}
}
#####Call FAVE routines
batch_align<-function(soundfiles,favedir){
setwd(favedir)
for (s in 1:length(soundfiles)){
	targetsound=soundfiles[s]
	baresound=unlist(strsplit(targetsound,split="\\."))[1]
	targettrans=paste(unlist(strsplit(targetsound, split="\\."))[1],"_FAVE",sep="")
system("cd /Users/pplsuser/FAVE/FAVE-align-york")
#####Dictionary check
com=paste("python FAAValign.py -v -n -i")
dict="york.txt"
trans=paste(targettrans,".txt",sep="")
unc=paste(targettrans,"_unc.txt",sep="")
call=paste(com,dict,"-c",unc,trans,sep=" ")
system(call)
#####prompt user to check unknown and press enter to continue
cat ("Check unknown file, and press [enter] to continue")
line <- readline()
#####Add new words to York dict
yorkdict<-read.csv(file=paste(favedir,"york.txt",sep="/"),sep="\t",header=F,fill=TRUE)
newdict<-read.csv(paste(favedir,unc,sep="/"),sep="\t",header=F,fill=TRUE)
outdict<-rbind(yorkdict,newdict)
write.table(outdict,"york.txt",sep="\t", row.names=FALSE, col.names=FALSE,quote=FALSE) 
#####Now run in aligning mode
com=paste("python FAAValign.py -v -n -i")
trans=paste(targettrans,".txt",sep="")
call=paste(com,"york.txt",targetsound,trans,sep=" ")
system(call)
}
}
htk_check<-function(sound,textgrid,thresh){
#####Loop through textgrid
#####If the word is not 'sp'
#####Extract the sound file, resample
#####Call get_sim
#####If output = 1, reject
#####If n (output[2]) is greater than 7, reject
#####If sim (output[1]) is below the threshold, reject
#####Rejected tokens are marked on the third tier and the word
#####is changed to 'sp'
#####Calculate summary stats and write a log file.
dir=getwd()
	#####Copy and rename
	old_grid=textgrid
	file.copy(textgrid,"temp_textgrid",overwrite=TRUE)
	textgrid=paste(dir,"temp_textgrid",sep="/")
	sound=paste(dir,sound,sep="/")
#####Checking happens here
#####Get number of tiers and remove tier 3
ntiers<-as.numeric(praat("Get number of tiers", input=textgrid))
if (ntiers<3){
	praat("Duplicate tier...",input=textgrid,arguments=list(2,3,"accepted"))
start_interval=1
}
if (ntiers>=3){
	praat("Remove tier...",input=textgrid,arguments=list(3))
	praat("Duplicate tier...",input=textgrid,arguments=list(2,3,"accepted"))
start_interval=1
}
nwords<-as.numeric(praat("Get number of intervals...", input=textgrid,arguments=list(3)))
#####Main loop
logdf=c("Interval","Word","Start","End","Sim","Decision")
	for (i in start_interval:nwords){
			text=praat("Get label of interval...", input=textgrid,arguments=list(2,i))
			tstart=as.numeric(strsplit((praat("Get start point...", input=textgrid,arguments=list(2,i))),split=" ")[[1]][1])
			tend=as.numeric(strsplit((praat("Get end point...", input=textgrid,arguments=list(2,i))),split=" ")[[1]][1])
				if (grepl("\\([[:print:]]*\\)",text)==TRUE){text="sp"}
				if (text=="sp"){praat("Set interval text...",input=textgrid,arguments=list(3,i,"sp"))}
				
				if (text!="sp"){
						praat("Extract part...",input=sound,output=paste(dir,"temp.wav",sep="/"),arguments= list(tstart, tend, "rectangular", 1, "yes"),overwrite=TRUE,filetype="WAV")
						praat("Resample...", input=paste(dir,"temp.wav",sep="/"),output=paste(dir,"temp_resamp.wav",sep="/"),arguments=list(16000,50),overwrite=TRUE,filetype="WAV")
						score<-get_sim("temp_resamp.wav",text)
							
							if (length(score ==1)){
								praat("Set interval text...",input=textgrid,arguments=list(2,i,"sp"))
								praat("Set interval text...",input=textgrid,arguments=list(3,i,"r"))
								decision="reject"
							} else if (score[1]<threshold){
								praat("Set interval text...",input=textgrid,arguments=list(2,i,"sp"))
								praat("Set interval text...",input=textgrid,arguments=list(3,i,"r"))
								decision="reject"
							} 
							if (score[1]>=threshold) {
								praat("Set interval text...",input=textgrid,arguments=list(3,i,"a"))
								decision="accept"
							}
							out=c(i,text,tstart,tend,score[1],decision)
							cat(paste(out,"\n",sep=""))
							logdf=rbind(logdf,out)

				}
}
total_n=nwords
total_a=as.numeric(strsplit(praat("Count labels...",input=textgrid,arguments=list(3,"a")),split=" ")[[1]][1])
total_r=as.numeric(strsplit(praat("Count labels...",input=textgrid,arguments=list(3,"r")),split=" ")[[1]][1])
rate=total_a/total_n*100
cat(paste("Total words checked:",total_n,"\n"))
cat(paste("Total accepted:",total_a,"\n"))
cat(paste("Total rejected:",total_r,"\n"))
cat(paste("Accuracy:",rate,"%","\n"))
labout=c("Total.N","Total.A","Total.R","Accuracy","","")
rbind(logdf,labout)
sumout=c(total_n,total_a,total_r,rate)
logdf<-rbind(logdf,sumout)
filename=strsplit(sound,split="/")[[1]]
name=filename[length(filename)]
root=strsplit(name,split="\\.")[[1]][1]
sumname=paste(root,"HtkCheck_log.txt",sep="_")
pathname=paste(dir,sumname,sep="/")
write.table(logdf,pathname,sep="\t", row.names=FALSE, col.names=FALSE,quote=FALSE)
#####Strip third tier and rename
praat("Remove tier...",input=textgrid,arguments=list(3))
file.rename("temp_textgrid",paste(root,"HtkCheck.TextGrid",sep="_"))
}

get_sim<-function(sound,word){
	#####Sample of alignments -- some correct incorrect
#####See if acoustic similarity reflects accuracy
#####Work out names
root=unlist(strsplit(sound,split="\\."))[1]
plp=paste(root,".plp",sep="")
write(plp,"test.scp")
#####Create a wordlist and wordnet
write(word,"list.txt")
system("HBuild list.txt wordnet")
#####Create plp object
call=paste("HCopy -C config",sound,plp,sep=" ")
system(call)
#####CAll Hvite
call="Hvite -a -H macros -H hmmdefs -S test.scp -i results.mlf -w wordnet dict monophones"
system(call)
res <- unlist(strsplit(paste(readLines("results.mlf"), collapse=" "),split=" "))
sim=as.numeric(res[length(res)-1])
n=length(res)
return(c(sim,n))
}

praat_check<-function(sound,textgrid){
	dir=getwd()
	#####Copy and rename
#####Checking happens here
#####Get number of tiers and remove tier 3
ntiers<-as.numeric(praat("Get number of tiers", input=textgrid))
if (ntiers<3){
	praat("Duplicate tier...",input=textgrid,arguments=list(2,3,"accepted"))
start_interval=1
}

nwords<-as.numeric(praat("Get number of intervals...", input=textgrid,arguments=list(3)))

if (ntiers==3){
	cat("It looks like we've worked on this before. Where shall we start from?\n")
	start_interval=readline()
		if (start_interval==""){start_interval=readline()}
}

#####Main loop
logdf=c("Start","End","Decision","","")
	for (i in start_interval:nwords){
			text=praat("Get label of interval...", input=textgrid,arguments=list(2,i))
			tstart=as.numeric(strsplit((praat("Get start point...", input=textgrid,arguments=list(2,i))),split=" ")[[1]][1])
			tend=as.numeric(strsplit((praat("Get end point...", input=textgrid,arguments=list(2,i))),split=" ")[[1]][1])
				if (text=="sp"){praat("Set interval text...",input=textgrid,arguments=list(3,i,"sp"))}
				if (text!="sp"){
						praat("Extract part...",input=sound,output=paste(dir,"temp.wav",sep="/"),arguments= list(tstart, tend, "rectangular", 1, "yes"),overwrite=TRUE)
						cat(paste("Word:",text,"\n"))
						cat ("Decision: 4=accept; 6=reject; 5=unsure; 0=quit; other=play again\n")
						praat("Play",input=paste(dir,"temp.wav",sep="/"))
						decision<-readline()
						answers<-c(4,5,6,0)

							while(!decision %in% answers){
								praat("Play",input=paste(dir,"temp.wav",sep="/"))
								cat(paste("Word:",text,"\n"))
								cat ("Decision: 4=accept; 6=reject; 5=unsure; 0=quit; other=play again\n")
								decision<-readline()
							}
							if (decision == 4){
								praat("Set interval text...",input=textgrid,arguments=list(3,i,"a"))
							}
							if (decision == 6){
								praat("Set interval text...",input=textgrid,arguments=list(2,i,"sp"))
								praat("Set interval text...",input=textgrid,arguments=list(3,i,"r"))
							}
							if (decision == 5){
								praat("Set interval text...",input=textgrid,arguments=list(2,i,"sp"))
								praat("Set interval text...",input=textgrid,arguments=list(3,i,"u"))
							}
							if (decision == 0){break}
							out=c(tstart,tend,decision,"","")
							logdf=rbind(logdf,out)
							file.remove(paste(dir,"temp.wav",sep="/"))

				}

	}
#####Calculate accuracy 
total_n=nwords
total_a=as.numeric(strsplit(praat("Count labels...",input=textgrid,arguments=list(3,"a")),split=" ")[[1]][1])
total_r=as.numeric(strsplit(praat("Count labels...",input=textgrid,arguments=list(3,"r")),split=" ")[[1]][1])
total_u=as.numeric(strsplit(praat("Count labels...",input=textgrid,arguments=list(3,"u")),split=" ")[[1]][1])
rate=total_a/total_n*100
cat(paste("Total words checked:",total_n,"\n"))
cat(paste("Total accepted:",total_a,"\n"))
cat(paste("Total rejected:",total_r,"\n"))
cat(paste("Total unsure:",total_u,"\n"))
cat(paste("Accuracy:",rate,"%","\n"))
labout=c("Total.N","Total.A","Total.R","Total.U","Accuracy")
rbind(logdf,labout)
sumout=c(total_n,total_a,total_r,total_u,rate)
logdf<-rbind(logdf,sumout)
filename=strsplit(sound,split="/")[[1]]
name=filename[length(filename)]
root=strsplit(name,split="\\.")[[1]][1]
sumname=paste(root,"PraatCheck_log.txt",sep="_")
pathname=paste(dir,sumname,sep="/")
write.table(logdf,pathname,sep="\t", row.names=FALSE, col.names=FALSE,quote=FALSE) 
#####Copy again,remove third tier,rename

}

batch_speaker_files()<-function{
for (i in 1:nrow(bio_single)){
	make_speaker_file(bio_single$Root[i],bio_single$Name[i],bio_single$Age[i],bio_single$Sex[i],"York",bio_single$Year[i],"1")
}

}
make_speaker_file<-function(root,name,age,sex,location,year,speakernum){
#####Create speaker file for this participant
content<-paste("--name\n",name,"\n","--age\n",age,"\n","--sex\n",sex,"\n","--location\n",location,"\n","--year\n",year,"\n","--speakernum\n",speakernum,sep="")
dir=getwd()
file=paste(root,".info",sep="")
path=paste(dir,file,sep="/")
write(content,path)
	#####Create config file for this participant
content<-paste("--speaker\n",file,sep="")
file=paste(root,".config",sep="")
path=paste(dir,file,sep="/")
write(content,path)
}

batch_extract<-function(sounds){
	library(ggplot2)
for (s in 1:length(sounds)){
	#####Figure out name of file and textgrid
dir=getwd()
sound=sounds[s]
root=paste(strsplit(sound,split="_")[[1]][1],"_",strsplit(sound,split="_")[[1]][2],sep="")
configfile=paste("+",root,".config",sep="")
name=strsplit(sound,split="\\.")[[1]][1]
grid=paste(name,".TextGrid",sep="")
out=paste(name,".meas",sep="")
	#####Call FAVE extract
	call=paste("python bin/extractFormants.py",configfile,sound,grid,out,sep=" ")
	system(call)
	data<-read.csv(paste(name,".txt",sep=""),sep="\t")
	mono=c("AA","AE","AH","AO","EH","ER","IH","IY","UH","UW")
	data_mono<-subset(data,vowel %in% mono)
	vowels<-ggplot(data_mono,aes(x=-F2.50.,y=-F1.50.,fill=vowel))
	vowels+geom_text(aes(color=vowel,label=vowel))
}
}

move_alignment_files<-function(sound,textgrid,targetdir){
	dir=getwd()
	sound=sound
	grid=textgrid
	root=unlist(strsplit(sound,split="\\."))[1]
	unc=paste(root,"_FAVE_unc.txt",sep="")
	trans=paste(root,"_FAVE.txt",sep="")
	log=paste(root,".FAAVlog",sep="")
	##checklog=paste(root,"PraatCheck_log.txt",sep="_")
#####Create folder for this participant
dir.create(file.path(targetdir, paste(root,"_Alignment",sep="")), showWarnings = FALSE)
#####Copy everything over
newpath=paste(targetdir,paste(root,"_Alignment",sep=""),sep="/")
file.copy(grid, newpath, overwrite = TRUE, recursive = FALSE,
          copy.mode = TRUE)
file.copy(unc, newpath, overwrite = TRUE, recursive = FALSE,
          copy.mode = TRUE)
file.copy(trans, newpath, overwrite = TRUE, recursive = FALSE,
          copy.mode = TRUE)
file.copy(log, newpath, overwrite = TRUE, recursive = FALSE,
          copy.mode = TRUE)
##file.copy(checklog, newpath, overwrite = TRUE, recursive = FALSE,
 ##         copy.mode = TRUE)
#####Need to add extraction files to this
#####Delete files from FAVE folder
file.remove(sound)
file.remove(grid)
file.remove(trans)
file.remove(log)
}
#####Function for stripping brackets etc. and converting
#####to seconds.

function<-getfiles(dir,targetdir){
bashdir="/Volumes/lang/Corpus/York\ Corpus"
files=list.files()
for (file in 30:31){
	setwd(dir)
 ## wavs
wavs=list.files(path=paste(dir,files[file],sep="/"),pattern="*.wav")
cat(paste("Found:",wavs))
wavsdir=paste(files[file],wavs,sep="/")
	for (s in 1:length(wavsdir)){
		call=paste("rsync --progress",wavsdir[s],targetdir)
		system(call)
	}


 ## doc
doc=list.files(path=paste(dir,files[file],sep="/"),pattern="*trans2")
cat(paste("Found:",doc))
docdir=paste(files[file],doc,sep="/")
if (file.copy(docdir,targetdir)==TRUE){
	setwd(targetdir)
	cat(paste("Copied:",doc))
	call=paste("textutil -convert txt ",doc)
	system(call)
}

}
}

get_biodata<-function(){

df=c("File","Name","Age","Code")
for (i in 1:length(files)){
	file=files[i]
	data<-scan(file=file,what='list',sep="\n")
	info=data[1]
	infoparts=strsplit(info, ",")
	name=infoparts[[1]][2]
	age=infoparts[[1]][3]
	code=infoparts[[1]][4]
	outdf=c(file,name,age,code)
	df=rbind(df,outdf)
}

}
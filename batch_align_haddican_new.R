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
filevec=strsplit(file, "/")
filename=filevec[[1]][length(filevec[[1]])]
nameparts=strsplit(filename, "_")
part_nameA=nameparts[[1]][1]
part_nameB=nameparts[[1]][2]
#####Change cwd
dir=paste(filevec[[1]][1:5],sep="/",collapse="/")
setwd(paste("/",dir,sep=""))
#####Load the file
data<-scan(file=file,what='list',sep="\n")
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

###Split by participant name
data<-gsub("[A-Z]:","",data)
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
##Loop through each chunk, concatenate the content,
##Write to df with name as first col, tstart and tend then content
thisrec=data
outdf=vector()
chunk=1
for (chunk in 1:(nchunks-1)){
	tstart=get_time(thisrec[chunks[chunk]])
	tend=get_time(thisrec[chunks[chunk+1]])
	start_index=chunks[chunk]
	end_index=chunks[chunk+1]
	rawtext=thisrec[(start_index+1):(end_index-1)]
	print(cat(rawtext))
	text=paste(rawtext,collapse="")
	tempdf=cbind(part_nameA,part_nameB,tstart,tend,text)
	outdf=rbind(outdf,tempdf)
}
          	outdf<-gsub('\\[.*?\\]', '', outdf)
      	outdf<-gsub('\\(.*?\\)', '', outdf)
      outdf<-gsub("\\[pron mai\\]","",outdf)
            outdf<-gsub("\\[prom mai\\]","",outdf)
		outdf<-gsub("TIME","",outdf)
#####Write new text file and check with user
###Laughter
outdf<-gsub("\\(laughs\\)","{LG}",outdf,ignore.case=TRUE)
outdf<-gsub("\\(laughter\\)","{LG}",outdf,ignore.case=TRUE)
outdf<-gsub("\\(coughs\\)","{CG}",outdf,ignore.case=TRUE)
outdf<-gsub("\\(coughing\\)","{CG}",outdf,ignore.case=TRUE)
outdf<-gsub("\\(breath\\)","{BR}",outdf,ignore.case=TRUE)
outdf<-gsub("\\(breathing\\)","{BR}",outdf,ignore.case=TRUE)


outdf<-gsub("INAUDIBLE","(( ))",outdf)
##All numbers and other symbols except for dashes and basic punctuation
outdf[,5]<-gsub("[^-!?,.()'{}A-Za-z///[:blank:]]","",outdf[,5])
##Add a space after a full stop or a question mark
outdf[,5]<-gsub("([!?.,])([[:alnum:]])","\\1 \\2",outdf[,5])
outdf<-outdf<-gsub("\\(breathing\\)","{BR}",outdf,ignore.case=FALSE)
##strip quotes
outdf<-data.frame(outdf)
outname=paste(unlist(strsplit(file, split="\\."))[1],"_FAVE.txt",sep="")
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
get_time<-function(string){
	stripped_string<-gsub("[[:punct:]]","",string)
	string_content=strsplit(stripped_string,split=" ")
	type=string_content[[1]][1]
	time=string_content[[1]][length(string_content[[1]])]
	min=paste(unlist(strsplit(time,split=""))[1:2],collapse="")
	sec=paste(unlist(strsplit(time,split=""))[3:4],collapse="")
	target_time<-paste("2087","8","11","0",min,sec,sep=",")
	target<-unlist(strsplit(target_time, split=","))
	time<- as.numeric(difftime(do.call(ISOdatetime, as.list(target)),ISOdatetime(2087,8,11,0,0,0), unit="secs"))
	return (time)
}

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
 ## play a sound given the filename and time
 play_sound<-function(data,line,minus,plus){
 target=data[data$X==line,]
name=as.character(target$file)
tstart=target$beg-minus
 tend=target$end+plus
 name<-paste(unlist(strsplit(name,split="\\."))[1],".wav",sep="")
 name<-gsub("_meas","",name)
 sound<-paste(getwd(),name,sep="/")
praat("Extract part...",input=sound,output=paste(getwd(),"temp.wav",sep="/"),arguments= list(tstart, tend, "rectangular", 1, "yes"),overwrite=TRUE)
						praat("Play",input=paste(getwd(),"temp.wav",sep="/"))
					}

					## extract a sound given the filename and time
extract_sound<-function(data,line,minus,plus,outname){
 target=data[data$X==line,]
name=as.character(target$file)
tstart=target$beg-minus
 tend=target$end+plus
 name<-paste(unlist(strsplit(name,split="\\."))[1],".wav",sep="")
 name<-gsub("_meas","",name)
 sound<-paste(getwd(),name,sep="/")
 outname<-paste(outname,".wav",sep="")
praat("Extract part...",input=sound,output=paste(getwd(),outname,sep="/"),arguments= list(tstart, tend, "rectangular", 1, "yes"),overwrite=TRUE)
					praat("Play",input=paste(getwd(),outname,sep="/"))
					}

 ## using the original transcriptions, loop through 
 ## each word and remove it if it is likely
 ## to not be the target speaker
get_bigrams<-function(file){
	
##Get indices of chunks
data<-scan(file=file,what='list',sep="\n")
#####Who is who?
spk1=strsplit(data[1],split=":")[[1]][1]
spk2=strsplit(data[2],split=":")[[1]][1]
spk3="IN"

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
outdf[,5]<-gsub(paste(spk1,":",sep=""),"",outdf[,5])
outdf[,5]<-gsub(paste(spk2,":",sep=""),"",outdf[,5])
outdf[,5]<-gsub(paste(spk3,":",sep=""),"",outdf[,5])
outdf<-gsub("\\(laughs\\)","{LG}",outdf,ignore.case=TRUE)
outdf<-gsub("\\(laughter\\)","{LG}",outdf,ignore.case=TRUE)
outdf<-gsub("\\(coughs\\)","{CG}",outdf,ignore.case=TRUE)
outdf<-gsub("\\(coughing\\)","{CG}",outdf,ignore.case=TRUE)
outdf<-gsub("\\(breath\\)","{BR}",outdf,ignore.case=TRUE)
outdf<-gsub("\\(breathing\\)","{BR}",outdf,ignore.case=TRUE)
outdf<-gsub("\\[laughs\\]","{LG}",outdf,ignore.case=TRUE)
outdf<-gsub("\\[laughter\\]","{LG}",outdf,ignore.case=TRUE)
outdf<-gsub("\\[coughs\\]","{CG}",outdf,ignore.case=TRUE)
outdf<-gsub("\\[coughing\\]","{CG}",outdf,ignore.case=TRUE)
outdf<-gsub("\\[breath\\]","{BR}",outdf,ignore.case=TRUE)
outdf<-gsub("\\[breathing\\]","{BR}",outdf,ignore.case=TRUE)
outdf[,5]<-gsub('\\[.*?\\]', '', outdf[,5])
      	outdf[,5]<-gsub('\\(.*?\\)', '', outdf[,5])
      outdf[,5]<-gsub("\\[pron mai\\]","",outdf[,5])
            outdf[,5]<-gsub("\\[prom mai\\]","",outdf[,5])
		outdf[,5]<-gsub("TIME","",outdf[,5])
		outdf[,5]<-gsub("[A-Z]:","",outdf[,5])
#####Write new text file and check with user
###Laughter
#####Get rid o anything else in brackets inc. pauses except inaud
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
## extract bigram probabilities for each minute for each speaker
bigrams_out=data.frame()
trigrams_out=data.frame()
for (chunk in 1:(nchunks-1)){
	ts=as.character(get_time(thisrec[chunks[chunk]]))
	te=as.character(get_time(thisrec[chunks[chunk+1]]))
	thischunk=outdf[outdf[,3]==ts&outdf[,4]==te,]

pri<-unlist(strsplit(thischunk,split=" "))
pri<-pri[pri != ""]
pri<-tolower(pri)
pri<-gsub("[[:punct:]]","",pri)
pri2<-c(pri[-1],".")
bigrams<-paste(pri,pri2)
bigram_priors<-table(bigrams)/length(pri)
pri3<-c(pri2[-1],".")
trigrams<-paste(pri,pri2,pri3)
trigram_priors<-table(trigrams)/length(pri)
bigram_priors<-data.frame(bigram_priors)
trigram_priors<-data.frame(trigram_priors)
bigram_priors$speaker="All"
trigram_priors$speaker="All"

triA<-unlist(strsplit(thischunk[thischunk[,1]==spk1,5],split=" "))
triA<-triA[triA != ""]
triA<-tolower(triA)
triA<-gsub("[[:punct:]]","",triA)
triA2<-c(triA[-1],".")
bigramsA<-paste(triA,triA2)
bigrams_A<-table(bigramsA)/length(triA)
triA3<-c(triA2[-1],".")
trigramsA<-paste(triA,triA2,triA3)
trigrams_A<-table(trigramsA)/length(triA)
bigrams_A<-data.frame(bigrams_A)
trigrams_A<-data.frame(trigrams_A)
if(length(bigrams_A)>0){bigrams_A$speaker=spk1}
if(length(trigrams_A)>0){trigrams_A$speaker=spk1}


triB<-unlist(strsplit(thischunk[thischunk[,1]==spk2,5],split=" "))
triB<-triB[triB != ""]
triB<-tolower(triB)
triB<-gsub("[[:punct:]]","",triB)
triB2<-c(triB[-1],".")
bigramsB<-paste(triB,triB2)
bigrams_B<-table(bigramsB)/length(triB)
triB3<-c(triB2[-1],".")
trigramsB<-paste(triB,triB2,triB3)
trigrams_B<-table(trigramsB)/length(triB)
bigrams_B<-data.frame(bigrams_B)
trigrams_B<-data.frame(trigrams_B)
if(length(bigrams_B)>0){bigrams_B$speaker=spk2}
if(length(trigrams_B)>0){trigrams_B$speaker=spk2}

triC<-unlist(strsplit(thischunk[thischunk[,1]==spk3,5],split=" "))
triC<-triC[triC != ""]
triC<-tolower(triC)
triC<-gsub("[[:punct:]]","",triC)
triC2<-c(triB[-1],".")
bigramsC<-paste(triC,triC2)
bigrams_C<-table(bigramsC)/length(triC)
triC3<-c(triC2[-1],".")
trigramsC<-paste(triC,triC2,triC3)
trigrams_C<-table(trigramsC)/length(triC)
bigrams_C<-data.frame(bigrams_C)
trigrams_C<-data.frame(trigrams_C)
if(length(bigrams_C)>0){bigrams_C$speaker=spk3}
if(length(trigrams_C)>0){trigrams_C$speaker=spk3}

bipriorA<-nrow(bigrams_A)/nrow(bigram_priors)
bipriorB<-nrow(bigrams_B)/nrow(bigram_priors)
bipriorC<-nrow(bigrams_C)/nrow(bigram_priors)
bispkpriors<-c(bipriorA,bipriorB,bipriorC)

tripriorA<-nrow(trigrams_A)/nrow(trigram_priors)
tripriorB<-nrow(trigrams_B)/nrow(trigram_priors)
tripriorC<-nrow(trigrams_C)/nrow(trigram_priors)
trispkpriors<-c(tripriorA,tripriorB,tripriorC)

spkrs=c(spk1,spk2,spk3)
bi_speaker_priors<-data.frame("ngram"="All","Freq"=bispkpriors,"speaker"=spkrs)
tri_speaker_priors<-data.frame("ngram"="All","Freq"=trispkpriors,"speaker"=spkrs)

names(bigram_priors)<-c("ngram","Freq","speaker")
names(trigram_priors)<-c("ngram","Freq","speaker")

names(bigrams_A)<-c("ngram","Freq","speaker")
names(bigrams_B)<-c("ngram","Freq","speaker")
names(bigrams_C)<-c("ngram","Freq","speaker")

names(trigrams_A)<-c("ngram","Freq","speaker")
names(trigrams_B)<-c("ngram","Freq","speaker")
names(trigrams_C)<-c("ngram","Freq","speaker")

biout<-rbind(bi_speaker_priors,bigram_priors,bigrams_A,bigrams_B,bigrams_C)
triout<-rbind(tri_speaker_priors,trigram_priors,trigrams_A,trigrams_B,trigrams_C)

biout$tstart=ts
biout$tend=te
biout$chunk=chunk

triout$tstart=ts
triout$tend=te
triout$chunk=chunk

bigrams_out<-rbind(bigrams_out,biout)
trigrams_out<-rbind(trigrams_out,triout)
bigrams_out<-subset(bigrams_out,Freq!=0)
trigrams_out<-subset(trigrams_out,Freq!=0)
}

return(list("trigrams"=trigrams_out,"bigrams"=bigrams_out))
}

get_probs<-function(worda,wordb,wordc="none",ts,te,bigrams,spk1,spk2,spk3,speaker){
	mode="bi"
	worda<-tolower(worda)
	wordb<-tolower(wordb)
	bigrams$tstart<-as.numeric(bigrams$tstart)
	bigrams$tend<-as.numeric(bigrams$tend)
	target<-bigrams[bigrams[,5]<=as.numeric(ts)&bigrams[,6]>=as.numeric(te),]
	which.chunk=Mode(target$chunk)
	priorString<-max(target[target[,1]==worda&target[,2]==wordb&target[,4]=="All",]$Freq)
	#sometimes the chunk is wrong -- this will find the right one
	if(priorString<0){
		#nearest chunk
			newtarget=bigrams[bigrams[,1]==worda&bigrams[,2]==wordb,]
				ch=newtarget[which.min(abs(newtarget[,7]-which.chunk)),7]
				newtarget_close=bigrams[bigrams[,7]==ch,]
				target<-newtarget_close
			priorString<-max(target[target[,1]==worda&target[,2]==wordb&target[,4]=="All",]$Freq)
			mode="bi"
	}
	if(priorString<0){
		#unigram probs
			newtarget=bigrams[bigrams[,1]==worda,]
				ch=newtarget[which.min(abs(newtarget[,7]-which.chunk)),7]
				newtarget_close=bigrams[bigrams[,7]==ch,]
				target<-newtarget_close
			priorString<-max(target[target[,1]==worda&target[,4]=="All",]$Freq)
			mode="uni"
	}
	speakerA<-target[target[,4]==spk1,]
	speakerB<-target[target[,4]==spk2,]
	speakerC<-target[target[,4]==spk3,]
	priorA<-max(speakerA[speakerA[,1]=="All",]$Freq,na.rm=TRUE)
	priorB<-max(speakerB[speakerB[,1]=="All",]$Freq,na.rm=TRUE)
	priorC<-max(speakerC[speakerC[,1]=="All",]$Freq,na.rm=TRUE)
	
	if (mode=="bi"){
	targetfreqA=speakerA[speakerA[,1]==worda&speakerA[,2]==wordb,]$Freq
	targetfreqB=speakerB[speakerB[,1]==worda&speakerB[,2]==wordb,]$Freq
	targetfreqC=speakerC[speakerC[,1]==worda&speakerC[,2]==wordb,]$Freq}

	if (mode=="uni"){
	targetfreqA=speakerA[speakerA[,1]==worda,]$Freq
	targetfreqB=speakerB[speakerB[,1]==worda,]$Freq
	targetfreqC=speakerC[speakerC[,1]==worda,]$Freq}
	
	if (length(targetfreqA)<1){targetfreqA=0}
		if (length(targetfreqB)<1){targetfreqB=0}
			if (length(targetfreqC)<1){targetfreqC=0}
	pA<-max(targetfreqA,na.rm=TRUE)
	pB<-max(targetfreqB,na.rm=TRUE)
	pC<-max(targetfreqC,na.rm=TRUE)
	pAA<-pA*priorA
	pBB<-pB*priorB
	pCC<-pC*priorC
	all<-c(pAA,pBB,pCC)
	probs=all/priorString
	spks=c(spk1,spk2,spk3)
	probs<-data.frame("Speaker"=spks,"P"=probs)
	return(subset(probs,Speaker==speaker)$P)
	}
get_spk<-function(worda,wordb,ts,te,bigrams,spk1,spk2,spk3){
		speakers=c(spk1,spk2,spk3)
		a<-get_probs(worda,wordb,ts,te,bigrams,spk1,spk2,spk3,spk1)
		b<-get_probs(worda,wordb,ts,te,bigrams,spk1,spk2,spk3,spk2)
		c<-get_probs(worda,wordb,ts,te,bigrams,spk1,spk2,spk3,spk3)
		candidates=c(a,b,c)
return(c(speakers[which.max(candidates)],candidates[which.max(candidates)]))
	}

	label<-function(data,bigrams,spk1,spk2,spk3){
		for (i in 1:nrow(data)){
			data$which<-get_spk(data$pre_word[i],data$word[i],data$beg[i],data$end[i],bigrams,spk1,spk2,spk3)[1]
			data$prob<-get_spk(data$pre_word[i],data$word[i],data$beg[i],data$end[i],bigrams,spk1,spk2,spk3)[2]
		}

	}
	
	speaker_check<-function(textgrid,bigrams){
#####Loop through textgrid
#####If the word is sp, treat it as the previous speaker
#####For each word, get the next none-sp word
#####Compute a bigram probability
#####Assign the word to a new tier for the target speaker
#####Go back and delete the originals
	#####Copy and rename
	dir=getwd()
	old_grid=textgrid
	file.copy(textgrid,"temp_textgrid",overwrite=TRUE)
	textgrid=paste(dir,"temp_textgrid",sep="/")
#####Get speakers
speakers=levels(bigrams$speaker)
speakers=subset(speakers,speakers!="All")
#####Get number of tiers and remove tier 3
ntiers<-as.numeric(praat("Get number of tiers", input=textgrid))
if (ntiers<3){
	praat("Insert interval tier...",input=textgrid,arguments=list(3,"speaker"))
start_interval=1
}

nwords<-as.numeric(praat("Get number of intervals...", input=textgrid,arguments=list(2)))
praat("Down to Table...",input=textgrid,arguments=list("no", 6, "yes", "no"),output=paste(dir,"temp_table",sep="/"),filetype="comma-separated",overwrite="true")
grid<-read.csv("temp_table")
grid<-grid[grep("word",grid$tier),]
grid$text<-as.character(grid$text)
grid<-as.matrix(grid)
#####Main loop

matlen<-nrow(grid)-length(grep("sp",grid[,3]))
mat<-matrix(data=NA,nrow=matlen+1,ncol=6)
mat[1,]<-c("Interval","Word","Start","End","Decision","Prob")
l=1
clean_grid<-grid[grep("[^sp]",grid[,3]),]
	grid_1<-clean_grid[-1,]
	grid_prev<-rbind(c("0","0","0","0"),clean_grid)
	for (i in 1:(matlen-1)){
	text=clean_grid[i,3]
	nexttext=grid_1[i,3]
	if (i<nrow(grid))
						tstart=as.numeric(clean_grid[i,1])
						tend=as.numeric(clean_grid[i,4])
						check=get_spk(text,nexttext,tstart,tend,bigrams,speakers[1],speakers[2],speakers[3])
						decision=check[1]
						prob=check[2]
						#if we can't decide, try another bigram
						if (is.na(prob)){
						check=get_spk(nexttext,text,tstart,tend,bigrams,speakers[1],speakers[2],speakers[3])
						decision=check[1]
						prob=check[2]	
						}
						#if we still can't decide, use the unigram probability
						if (is.na(prob)){
							nexttext=grid_prev[i,3]
						check=get_spk(nexttext,text,tstart,tend,bigrams,speakers[1],speakers[2],speakers[3])
						decision=check[1]
						prob=check[2]	
						}
						row=rownames(clean_grid)[i]
							out=c(row,text,tstart,tend,decision,prob)
							mat[l+1,]<-out
							cat(out,"\n")
							l=l+1
				}
				return(mat)
}


	Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }

  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

get_speaker_tier(speaker,table){
	outvec=c("tstart","tend")
#####Identify start times of utterance-length chunks from table
#####Add these chunks to a textgrid!
speakers=levels(bigrams$speaker)
speakers=subset(speakers,speakers!="All")
#####Get number of tiers and remove tier 3
ntiers<-as.numeric(praat("Get number of tiers", input=textgrid))
if (ntiers>=3){
	praat("Remove tier...",input=textgrid,arguments=list(3))}

	praat("Insert interval tier...",input=textgrid,arguments=list(3,"speaker"))

#####Get the times for a given speaker
spk<-mat[2:nrow(mat),]
indices<-spk[,5]
plusone<-c(indices[2:length(indices)],NA)
change<-cbind(indices,plusone)
change<-as.data.frame(change)
change$change<-change$indices!=change$plusone
change$change[1]=TRUE
bound<-spk[grep(TRUE,change$change),]
bound[,3]<-as.numeric(bound[,3])
bound[,4]<-as.numeric(bound[,4])
starts<-bound[seq(1, nrow(bound), by = 2),c(3,5)]
ends<-bound[seq(2, nrow(bound), by = 2),c(4,5)]

ends<-rbind(ends,bound[nrow(bound),c(4,5)])
int<-cbind(starts,ends)
int<-data.frame(int)
names(int)<-c("starts","spk","ends","spk2")
}
new <- int
for (t in 1:50){
	praat("Insert boundary...",input=textgrid,arguments=list(3,new$starts[t]))
	praat("Insert boundary...",input=textgrid,arguments=list(3,new$ends[t]))
	if (t %% 2 == 0 ){s="JC"}
	if (t %% 2 != 0 ){s="JG"}
	praat("Set interval text...",input=textgrid,arguments=list(3,t+1,s))
}
}

get_speaker_tier(speaker,table){
speakers=levels(bigrams$speaker)
speakers=subset(speakers,speakers!="All")
#####Get number of tiers and remove tier 3
ntiers<-as.numeric(praat("Get number of tiers", input=textgrid))
if (ntiers<3){
	praat("Insert interval tier...",input=textgrid,arguments=list(3,"speaker"))
start_interval=1
}



}
spk<-mat[2:nrow(mat),]
indices<-spk[,5]
plusone<-c(indices[2:length(indices)],NA)
change<-cbind(indices,plusone)
change<-as.data.frame(change)
change$change<-change$indices!=change$plusone
change$change[1]=TRUE
spk[grep(TRUE,change$change),]
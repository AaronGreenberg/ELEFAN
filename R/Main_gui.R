#' Minimal doc
#' @import gWidgets MASS Rcpp RGtk2 cairoDevice
#' @description minimal documentation for roxygen purposes and could be added later 
#' @export

options("guiToolkit"="RGtk2")


#create the Main window.


ELEFAN_gui <- function(){
my_path <- paste(.libPaths()[1],"/ELEFAN",sep="")
size=1000
window = gwindow("ELEFAN in R",height=size,width=1.618*size,visible=TRUE)
biggroup <- ggroup(container=window,expand=TRUE,horizontal=FALSE)
welcome<- gtext("Welcome to ELEFAN in R; please load a file!", container =biggroup,expand=FALSE,horizontal=FALSE,font.attr=c(size=13))
#make big note book!
nb <- gnotebook(container=biggroup,expand=FALSE,horizontal=FALSE)




#%############################################################
#Make the Entry page!
YieldProbs<<-NA#place to store prob.
YieldAges <<-NA#place to store ages
lengthunits<<-c("mt")
datetmp <- "  "
datatmp <- "  "
#visible(nb[1]) <- TRUE
Entry <- ggroup(container = nb,label="Tab. data", expand=TRUE,horizontal=TRUE)#make entry gr
addSpace(biggroup,300,horizontal=FALSE)
Entrylittle <- ggroup(container=Entry,expand=FALSE,horizontal=FALSE,height=30,width=330)# make little entry group

#Add ELEFAN in R logo at the top of the page
gimage("png/logo1.png",dirname=my_path,container=Entrylittle)
Entrypic<- gnotebook(container=Entry,expand=TRUE)#create the Entry pic.
Datatable<- gtable(datatmp,container=Entrypic,label="LF data")
names(Datatable) <- "ML" #lengthunist
#Load Data
#I need handlers.
datefileinh <- function(h,...){
  rm(list=ls())# clear everything out
  dispose(Entrypic)
  Datatable<- gtable(datatmp,container=Entrypic,label="LF data")
  names(Datatable) <- "ML" #lengthunits
  
  YieldProbs<<-NA#place to store prob.
  YieldAges <<-NA#place to store ages
  lengthunits<<-c("mt")
  datefilein()
  svalue(welcome) <-paste("You are studying: ", basename(sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\2",fname1)),".",sep="")
  #print("datain")
  #print(datain)
  datetmp <- datein
  options(digits=3)
  stdate[] <- c(-1,1:(length(datein[,2])-1))
  stdate1[] <- c(-1,1:(length(datein[,2])-1))
  lengthprob[] <- c(-1,1:length(datain$ML))
  stdatek[] <- 1:(length(datein[,2])-1)
  temptable <-datain 
  temptable <-datain
  height<- length(temptable[,1])
  len<- length(temptable[1,])
  temptable[height+1,1] <- as.character(lengthunits)
  temptable[height+1,2:len] <- as.character(" ")
  dispose(Entrypic)
  Datatable<- gtable(temptable,container=Entrypic,label="LF data")
  visible(Datatable) <- TRUE
  visible(Datatable) <- TRUE
  Linfslide[] <- seq(0,1.5*max(datain$ML),length.out=1000)
  Linfslidek[] <- seq(0,1.5*max(datain$ML),length.out=1000)
  Linfslidec[] <- seq(0.8*max(datain$ML),1.5*max(datain$ML),length.out=1000)
  Linfslidec2[] <- seq(0.8*max(datain$ML),1.5*max(datain$ML),length.out=1000)
  #Linfslider[] <- seq(0.8*max(datain$ML),1.5*max(datain$ML),length.out=1000)
  Linfypr[] <- seq(0.8*max(datain$ML),1.5*max(datain$ML),length.out=1000)
  Linfypriso[] <- seq(0.8*max(datain$ML),1.5*max(datain$ML),length.out=1000)
  Pointslide[] <- 2:length(datain$ML)
  Pointslideuc[] <- 1:length(datain$ML)
  Pointslidelc[] <- 1:length(datain$ML)
  Pointslideuc2[] <- 1:length(datain$ML)
  Pointslidelc2[] <- 1:length(datain$ML)
  midlength[] <-   datain$ML
  midlength1[] <- datain$ML#make 
  hline[] <- datain$ML
  midlengthk[] <- datain$ML
  YieldtmpProbs <- cbind(datain$ML,YieldProbs)
  visible(Datatable) <- TRUE
  visible(Datatable) <- TRUE
  }


#adding buttons



readdatefile=gbutton("Load file",handler=datefileinh)
tmp = gframe("Data", container = Entrylittle)
add(tmp, readdatefile, expand=FALSE)

#Add sponsors logo at the bottom of the page
addSpace(Entrylittle,438,horizontal=FALSE)# spacing needs to be tuned for each slide
#addSpring(Entrylittle)
Entrylogo <- ggroup(container=Entrylittle,expand=TRUE,horizontal=FALSE,width=330)# make little entry group
gimage("png/logo2.png",dirname=my_path,container=Entrylogo)


## ## ## ## Make Length Frequency plot window.

LFplot <- ggroup(container = nb,label="L/F", expand=TRUE,horizontal=TRUE)#make entry group
LFplotlittle <- ggroup(container=LFplot,expand=FALSE,horizontal=FALSE,width=330)# make little entry group  
LFpic<- gnotebook(container=LFplot,expand=TRUE)#create the Entry pic.
histgraphic<- ggraphics(container = LFpic,width=700,height=500,label="Orig. L/F")
refactorgraphic<- ggraphics(container = LFpic,width=700,height=500,label="Restruct. L/F")




#Add ELEFAN in R logo at the top of the page
gimage("png/logo1.png",dirname=my_path,container=LFplotlittle)


Linfslide = gslider(from=1,to=10,length.out=1000,value=2)
tmp = gframe("Linf", container = LFplotlittle)
add(tmp, Linfslide, expand=TRUE)


Kslide = gslider(from=0,to=10,by=.01,value=0)
tmp = gframe("K", container = LFplotlittle)
add(tmp, Kslide, expand=TRUE)


Cslide= gslider(from=0,to=1.2,by=.01,value=0)
tmp = gframe("C", container = LFplotlittle)
add(tmp, Cslide, expand=TRUE)


twslide=gslider(from=0,to=1,by=.01,value=0)
tmp = gframe("WP", container = LFplotlittle)
add(tmp, twslide, expand=TRUE)


scaleslide= gslider(from=0,to=4,by=.01,value=1)
tmp = gframe("Scale", container = LFplotlittle)
add(tmp, scaleslide, expand=TRUE)


hline= gslider(from=0,to=4,by=.01,value=1)
tmp = gframe("Ruler", container = LFplotlittle)
add(tmp, hline, expand=TRUE)


#gimage("png/usaid.png",dirname=my_path,container=LFplotlogo)
plotlf <- function(h,...){
  sdate <- as.numeric(svalue(stdate))
  sdate <- sdate+1#converting to real sample number.
  ML <- as.numeric(svalue(midlength))
  sdate1 <- as.numeric(svalue(stdate1))
  sdate1 <- sdate1+1#converting to real sample number.
  ML1 <- as.numeric(svalue(midlength1))
  hline <- as.numeric(svalue(hline))
 visible(histgraphic) <- TRUE #make correct picture  
 plotpeak(svalue(Linfslide),svalue(Kslide),svalue(Cslide),svalue(twslide),ptype="LF",sdate,ML,svalue(scaleslide),sdate1,ML1,hline)
 visible(refactorgraphic) <- TRUE #make correct picture  
 plotpeak(svalue(Linfslide),svalue(Kslide),svalue(Cslide),svalue(twslide),ptype="Peaks",sdate,ML,svalue(scaleslide),sdate1,ML1,hline)
 }

 
LFplotlittlebutton1<-ggroup(container=LFplotlittle,expand=FALSE,horizontal=TRUE,width=330)
#gtext(text="Main cohort",container=LFplotlittlebutton1,height=10,width=200,font.attr=c(size=3))
LFplotlittlecohort1<-ggroup(container=LFplotlittle,expand=FALSE,horizontal=TRUE,width=330)
stdate <- gdroplist(list(1:10*0-1))
tmp <- gframe("SS-1",container=LFplotlittlecohort1)
add(tmp,stdate, expand=FALSE)
midlength <- gdroplist(list(1:20*0))
tmp <- gframe("SL-1",container=LFplotlittlecohort1)
add(tmp,midlength, expand=FALSE)
LFplotlittlebutton2<-ggroup(container=LFplotlittle,expand=FALSE,horizontal=TRUE,width=330)
#gtext(text="2nd cohort",container=LFplotlittlebutton2,height=10,width=200,font.attr=c(size=3))
LFplotlittlecohort2<-ggroup(container=LFplotlittle,expand=FALSE,horizontal=TRUE,width=330)
stdate1 <- gdroplist(list(1:10*0-1))
tmp <- gframe("SS-2",container=LFplotlittlecohort2)
add(tmp,stdate1, expand=FALSE)
midlength1 <- gdroplist(list(1:20*0))
tmp <- gframe("SL-2",container=LFplotlittlecohort2)
add(tmp,midlength1, expand=FALSE)
plot=gbutton("Make plot",handler=plotlf)
tmp=gframe("Plot",container=LFplotlittlecohort2)
add(tmp, plot, expand=FALSE)


#Add sponsors logo at the bottom of the page
#addSpace(LFplotlittle,76,horizontal=FALSE)# spacing needs to be tuned for each slide
LFplotlogo <- ggroup(container=LFplotlittle,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
gimage("png/logo2.png",dirname=my_path,container=LFplotlogo)





## ## ## ## Make Wetherall plot Window

Wetherallplot <- ggroup(container = nb,label="W. plot", expand=TRUE,horizontal=TRUE)#make entry group
Wetherallplotlittle <- ggroup(container=Wetherallplot,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
Wetherallpic<- gnotebook(container=Wetherallplot,expand=TRUE)#create the Entry pic.
Wetherallgraphic<- ggraphics(container = Wetherallpic,width=700,height=500,label="Wetherall plot")


#Add ELEFAN in R logo at the top of the page
gimage("png/logo1.png",dirname=my_path,container=Wetherallplotlittle)


Pointslide = gslider(from=1,to=50,by=1,value=4)
tmp = gframe("Points in plot", container = Wetherallplotlittle)
add(tmp, Pointslide, expand=TRUE)


plotweth <- function(h,...){ 
visible(Wetherallgraphic) <- TRUE #make correct picture  
 wetherall(datain,svalue(Pointslide))
 }

#addSpace(Wetherallplotlittle,40,horizontal=FALSE)# Daniel wants the "plot" buttons spaced farther from the other buttons
plot=gbutton("Make plot",handler=plotweth)
tmp=gframe("Plot",container=Wetherallplotlittle)
add(tmp, plot, expand=FALSE)


#Add sponsors logo at the bottom of the page
addSpace(Wetherallplotlittle,378,horizontal=FALSE)# spacing needs to be tuned for each slide
Wetherallplotlogo <- ggroup(container=Wetherallplotlittle,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
gimage("png/logo2.png",dirname=my_path,container=Wetherallplotlogo)



## ## ## ## Make Kscan Window

Kscanplot <- ggroup(container = nb,label="K-scan", expand=TRUE,horizontal=TRUE)#make entry group
Kscanplotlittle <- ggroup(container=Kscanplot,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
Kscanplotpic<- gnotebook(container=Kscanplot,expand=TRUE)#create the Entry pic.
Kscangraphic<- ggraphics(container = Kscanplotpic,width=700,height=500,label="K-scan")


#Add ELEFAN in R logo at the top of the page
gimage("png/logo1.png",dirname=my_path,container=Kscanplotlittle)


Linfslidek = gslider(from=1,to=10,length.out=1000,value=2)
tmp = gframe("Linf", container = Kscanplotlittle)
add(tmp, Linfslidek, expand=TRUE)


Cslidek= gslider(from=0,to=1.2,by=.01,value=0)
tmp = gframe("C", container = Kscanplotlittle)
add(tmp, Cslidek, expand=TRUE)


twslidek=gslider(from=0,to=1,by=.01,value=0)
tmp = gframe("WP", container = Kscanplotlittle)
add(tmp, twslidek, expand=TRUE)


movingaveragek=gslider(from=0,to=100,by=1,value=1)
tmp = gframe("Moving average strength", container = Kscanplotlittle)
add(tmp, movingaveragek, expand=TRUE)


stdatek <- gdroplist(list(1:10*0))
tmp <- gframe("SS",container=Kscanplotlittle)
add(tmp,stdatek, expand=FALSE) 


midlengthk <- gdroplist(list(1:20*0))
tmp <- gframe("SL",container=Kscanplotlittle)
add(tmp,midlengthk, expand=FALSE) 


computefullkscan <- function(h,...){
ckscan(Linf=svalue(Linfslidek),c=svalue(Cslidek),tw=svalue(twslidek))
}
computefixedkscan <- function(h,...){

cfixedkscan(sdate=svalue(stdatek),ML=svalue(midlengthk),Linf=svalue(Linfslidek),C=svalue(Cslidek),tw=svalue(twslidek))
}
plotfullkscan <- function(h,...){ 
visible(Kscangraphic) <- TRUE #make correct picture

kscanplot(window=svalue(movingaveragek))
visible(Kscangraphic) <- FALSE  #make correct picture  
visible(Recruitgraphic) <- TRUE #make correct picture
temp<- recruitment(max(zkscan[which.max(zkscan[,1]),1]),svalue(Linfslidek),svalue(Cslidek),svalue(twslidek))
visible(Recruitgraphic) <- TRUE #make correct picture
visible(Recruitgraphic) <- TRUE #make correct picture
visible(Kscangraphic) <- TRUE #make correct picture  


 }
plotfixedkscan <- function(h,...){ 
visible(Kscangraphic) <- TRUE #make correct picture  
fixedkscanplot(window=svalue(movingaveragek))
visible(Kscangraphic) <- FALSE #make correct picture  
visible(Recruitgraphic) <- TRUE #make correct picture
temp<- recruitment(max(fixzkscan[which.max(fixzkscan[,1]),1]),svalue(Linfslidek),svalue(Cslidek),svalue(twslidek))
visible(Recruitgraphic) <- TRUE #make correct picture
visible(Recruitgraphic) <- TRUE #make correct picture
visible(Kscangraphic) <- TRUE #make correct picture  

 }


Kscanplotlittlebutton<-ggroup(container=Kscanplotlittle,expand=FALSE,horizontal=TRUE,width=330)
compute=gbutton("Start full scan",handler=computefullkscan)
tmp=gframe("K-scan",container=Kscanplotlittlebutton)
add(tmp, compute, expand=FALSE)
plot=gbutton("Make plot",handler=plotfullkscan)
tmp=gframe("Plot",container=Kscanplotlittlebutton)
add(tmp, plot, expand=FALSE)

Kscanplotlittlebutton1<-ggroup(container=Kscanplotlittle,expand=FALSE,horizontal=TRUE,width=330)
compute=gbutton("Start fixed scan",handler=computefixedkscan)
tmp=gframe("K-scan",container=Kscanplotlittlebutton1)
add(tmp, compute, expand=FALSE)
plot=gbutton("Make plot",handler=plotfixedkscan)
tmp=gframe("Plot",container=Kscanplotlittlebutton1)
add(tmp, plot, expand=FALSE)


#Add sponsors logo at the bottom of the page
addSpace(Kscanplotlittle,65,horizontal=FALSE)# spacing needs to be tuned for each slide
Kscanplotlogo <- ggroup(container=Kscanplotlittle,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
gimage("png/logo2.png",dirname=my_path,container=Kscanplotlogo)



## ## ## ## Make Catch curve and Corr.L/F wind-points-pointsow

Catchcurveplot <- ggroup(container = nb,label="C.C. I", expand=TRUE,horizontal=TRUE)#make entry group
Catchcurvelittle <- ggroup(container=Catchcurveplot,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
Catchcurvepic<- gnotebook(container=Catchcurveplot,expand=TRUE)#create the Entry pic.
Catchcurvegraphic<- ggraphics(container = Catchcurvepic,width=700,height=500,label="C.C. non seasonal")


#Add ELEFAN in R logo at the top of the page
gimage("png/logo1.png",dirname=my_path,container=Catchcurvelittle)


Linfslidec=gslider(from=1,to=10,length.out=1000,value=2)
tmp = gframe("Linf", container = Catchcurvelittle)
add(tmp, Linfslidec, expand=TRUE)


Klocslidec=gslider(from=0,to=10,by=.01,value=0)
tmp = gframe("K", container = Catchcurvelittle)
add(tmp, Klocslidec, expand=TRUE)


Pointslidelc=gslider(from=0,to=10,by=1,value=0)
tmp = gframe("First point", container = Catchcurvelittle)
add(tmp, Pointslidelc, expand=TRUE)


Pointslideuc=gslider(from=0,to=10,by=1,value=0)
tmp = gframe("Last point", container = Catchcurvelittle)
add(tmp, Pointslideuc, expand=TRUE)


plotnonseacatch <- function(h,...){
visible(Catchcurvegraphic) <- TRUE #make correct picture
temp<- plotnonseacatchcurve(svalue(Klocslidec),svalue(Linfslidec),svalue(Pointslideuc),svalue(Pointslidelc))
YieldProbs <<- temp$prob
temp$data <- correctedlf(temp$prob)
temptable2<- (signif(temp$data,3))
temptable2 <- (signif(temp$data,3))
height<- length(temptable2[,1])
len<- length(temptable2[1,])
temptable2[height+1,1] <- as.character(lengthunits)
temptable2[height+1,2:len] <- as.character(" ")
dispose(CorrLFpic)
Datatablemodified<- gtable(temptable2,container=CorrLFpic,label="Modified data")
Datatablemodified[] <- temptable2
visible(Datatablemodified) <- TRUE
visible(Datatablemodified) <- TRUE
YieldProbs <<- temp$prob
YieldAges<<-temp$ages
filename <- (paste(substr(fname1,start=1,stop=(nchar(fname1)-4)),"corrected.csv",sep="_"))
if(file.exists(filename)){file.remove(filename)}#remove file
#print(temp$data)
#print(lengthunits)
#write("hey",file=filename)
write.table(round(temp$data,4),file=filename,row.names=FALSE,col.names=TRUE,append=TRUE,quote=FALSE,sep=",")
write.table(lengthunits,file=filename,quote=FALSE,append=TRUE,row.names=FALSE,col.names=FALSE)
visible(Catchcurvegraphic) <- TRUE #make correct picture

 }

 #addSpace(Catchcurvelittle,40,horizontal=FALSE)# Daniel wants the "plot" buttons spaced farther from the other buttons
 plot=gbutton("Make plot",handler=plotnonseacatch)
 tmp=gframe("Plot",container=Catchcurvelittle)
 add(tmp, plot, expand=FALSE)


#Add sponsors logo at the bottom of the page
addSpace(Catchcurvelittle,198,horizontal=FALSE)# spacing needs to be tuned for each slide
Catchcurvelogo <- ggroup(container=Catchcurvelittle,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
gimage("png/logo2.png",dirname=my_path,container=Catchcurvelogo)



# Make the  Probability fixing page


YieldtmpProbs <- cbind(" "," ")
probplot <- ggroup(container = nb,label="Prob", expand=TRUE,horizontal=TRUE)#make entry group
problittle <- ggroup(container=probplot,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
probpic <- gnotebook(container=probplot,expand=TRUE)#create the Entry pic.
Probabilitymodified<- gtable(YieldtmpProbs,container=probpic,label="Probabilities")
colnames(Probabilitymodified) <- c("ML","Probabilities")
probgraphic<- ggraphics(container = probpic,width=700,height=500,label="Prob. plot")  

#Add ELEFAN in R logo at the top of the page
gimage("png/logo1.png",dirname=my_path,container=problittle)


lengthprob <- gdroplist(list(1:200-2))
tmp <- gframe("Which prob",container=problittle)
add(tmp,lengthprob, expand=FALSE)

probadjust <- gslider(from=0,to=100,by=.5,value=50)
tmp <- gframe("Probability",container=problittle)
add(tmp,probadjust, expand=TRUE)

fn <- function(length,prob){

     if(svalue(length)>0){
     YieldProbs[length] <- prob/100
    }
     YieldProbs<<-YieldProbs
     return(YieldProbs)
    }
plotprobs <- function(h,...){
  YieldProbs<<-fn(svalue(lengthprob),svalue(probadjust))
  temprob<- cbind(datain$ML,(signif(YieldProbs,3)))
  dispose(probpic)
  dispose(probpic)
  
  temprobs <- cbind(datain$ML,(signif(YieldProbs,3)))
  Probabilitymodified<- gtable(temprobs,container=probpic,label="Probabilities")
  colnames(Probabilitymodified) <- c("ML","Probabilities")
  visible(Probabilitymodified) <- FALSE #make correct picture
  visible(Probabilitymodified) <- TRUE #make correct picture
  probgraphic<- ggraphics(container = probpic,width=700,height=500,label="Prob. plot")
  visible(probgraphic) <- TRUE #make correct picture  
  probsplot(YieldProbs,datain$ML)
  visible(probgraphic) <- FALSE #make correct picture  
  visible(probgraphic) <- TRUE #make correct picture
  visible(probgraphic) <- TRUE #make correct picture
temper <- correctedlf(YieldProbs)
temptable3<- (signif(temper,3))
temptable3 <- (signif(temper,3))
height<- length(temptable3[,1])
len<- length(temptable3[1,])
temptable3[height+1,1] <- as.character(lengthunits)
temptable3[height+1,2:len] <- as.character(" ")


dispose(CorrLFpic)
Datatablemodified<- gtable(temptable3,container=CorrLFpic,label="Modified data")
Datatablemodified[] <- temptable3
visible(Datatablemodified) <- TRUE
visible(Datatablemodified) <- TRUE
filename <- (paste(substr(fname1,start=1,stop=(nchar(fname1)-4)),"corrected.csv",sep="_"))
if(file.exists(filename)){file.remove(filename)}#remove file
write.table(round(temper,4),file=filename,row.names=FALSE,col.names=TRUE,append=TRUE,quote=FALSE,sep=",")
write.table(lengthunits,file=filename,quote=FALSE,append=TRUE,row.names=FALSE,col.names=FALSE)
visible(Catchcurvegraphic) <- TRUE #make correct picture
visible(Catchcurvegraphic) <- TRUE #make correct picture
}



 plot=gbutton("Make plot",handler=plotprobs)
 tmp=gframe("Plot",container=problittle)
 add(tmp, plot, expand=FALSE)


#Add sponsors logo at the bottom of the page
addSpace(problittle,248,horizontal=FALSE)# spacing needs to be tuned for each slide
problogo <- ggroup(container=problittle,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
gimage("png/logo2.png",dirname=my_path,container=problogo)




CorrLFplot <- ggroup(container = nb,label="Corr. L/F", expand=TRUE,horizontal=TRUE)#make entry group
CorrLFlittle <- ggroup(container=CorrLFplot,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
CorrLFpic <- gnotebook(container=CorrLFplot,expand=TRUE)#create the Entry pic.
Datatablemodified<- gtable(datatmp,container=CorrLFpic,label="Modified data")
names(Datatablemodified) <- "ML" #lengthunits

#Add ELEFAN in R logo at the top of the page
gimage("png/logo1.png",dirname=my_path,container=CorrLFlittle)



#Add sponsors logo at the bottom of the page
addSpace(CorrLFlittle,485,horizontal=FALSE)# spacing needs to be tuned for each slide
CorrLFlogo <- ggroup(container=CorrLFlittle,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
gimage("png/logo2.png",dirname=my_path,container=CorrLFlogo)


## ## ## ## Seasonal Catch curve tab

SeasonalCatch <- ggroup(container = nb,label="C.C. II", expand=TRUE,horizontal=TRUE)#make entry gr
SeasonalCatchlittle <- ggroup(container=SeasonalCatch,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
SeasonalCatchpic<- gnotebook(container=SeasonalCatch,expand=TRUE)#create the Entry pic.
SeasonalCatchgraphic<- ggraphics(container = SeasonalCatchpic,width=700,height=500,label="C.C. seasonal")


#Add ELEFAN in R logo at the top of the page
gimage("png/logo1.png",dirname=my_path,container=SeasonalCatchlittle)


Linfslidec2=gslider(from=1,to=10,length.out=1000,value=2)
tmp = gframe("Linf", container = SeasonalCatchlittle)
add(tmp, Linfslidec2, expand=TRUE)


Klocslidec2=gslider(from=0,to=10,by=.01,value=0)
tmp = gframe("K", container = SeasonalCatchlittle)
add(tmp, Klocslidec2, expand=TRUE)


Cslidec2=gslider(from=0,to=1.2,by=.01,value=0)
tmp = gframe("C", container = SeasonalCatchlittle)
add(tmp, Cslidec2, expand=TRUE)


TWslidec2=gslider(from=0,to=1,by=.01,value=0)
tmp = gframe("WP", container = SeasonalCatchlittle)
add(tmp, TWslidec2, expand=TRUE)


Pointslidelc2=gslider(from=0,to=10,by=1,value=0)
tmp = gframe("First point", container = SeasonalCatchlittle)
add(tmp, Pointslidelc2, expand=TRUE)


Pointslideuc2=gslider(from=0,to=10,by=1,value=0)
tmp = gframe("Last point", container = SeasonalCatchlittle)
add(tmp, Pointslideuc2, expand=TRUE)


plotseacatch <- function(h,...){ 
  visible(SeasonalCatchgraphic) <- TRUE #make correct picture
  temp<- plotseacatchcurve(svalue(Klocslidec2),svalue(Linfslidec2),svalue(Cslidec2),svalue(TWslidec2),svalue(Pointslideuc2),svalue(Pointslidelc2))
  visible(SeasonalCatchgraphic) <- TRUE #make correct picture
  visible(SeasonalCatchgraphic) <- TRUE #make correct picture
 }


 plot=gbutton("Make plot",handler=plotseacatch)
 tmp=gframe("Plot",container=SeasonalCatchlittle)
 add(tmp, plot, expand=FALSE)


addSpace(SeasonalCatchlittle,78,horizontal=FALSE)# spacing needs to be tuned for each slide
SeasonalCatchlogo <- ggroup(container=SeasonalCatchlittle,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
gimage("png/logo2.png",dirname=my_path,container=SeasonalCatchlogo)



## ## ## ## Recruitment tab

Recruit <- ggroup(container = nb,label="Recr.", expand=TRUE,horizontal=TRUE)#make entry gr
Recruitlittle <- ggroup(container=Recruit,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
Recruitpic<- gnotebook(container=Recruit,expand=TRUE)#create the Entry pic.
Recruitgraphic<- ggraphics(container = Recruitpic,width=700,height=500,label="Recr.")


#Add ELEFAN in R logo at the top of the page
gimage("png/logo1.png",dirname=my_path,container=Recruitlittle)


addSpace(Recruitlittle,600,horizontal=FALSE)# spacing needs to be tuned for each slide
Recruitlogo <- ggroup(container=Recruitlittle,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
gimage("png/logo2.png",dirname=my_path,container=Recruitlogo)



## ## ## ## Yield per recruit tab

YieldperRecruit <- ggroup(container = nb,label="Y/R", expand=TRUE,horizontal=TRUE)#make entry gr
YieldperRecruitlittle <- ggroup(container=YieldperRecruit,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
YieldperRecruitpic<- gnotebook(container=YieldperRecruit,expand=TRUE)#create the Entry pic.
YieldperRecruitgraphic<- ggraphics(container = YieldperRecruitpic,width=700,height=500,label="YPR-E")
YieldperRecruitgraphic2<- ggraphics(container = YieldperRecruitpic,width=700,height=500,label="YPR-F")


#Add ELEFAN in R logo at the top of the page
gimage("png/logo1.png",dirname=my_path,container=YieldperRecruitlittle)


Linfypr=gslider(from=1,to=10,length.out=1000,value=2)
tmp = gframe("Linf", container = YieldperRecruitlittle)
add(tmp, Linfypr, expand=TRUE)


Kypr=gslider(from=0,to=10,by=.01,value=0)
tmp = gframe("K", container = YieldperRecruitlittle)
add(tmp, Kypr, expand=TRUE)


Lc=gslider(from=0.1,to=0.9,by=.01,value=0)
tmp = gframe("Lc/Linf", container = YieldperRecruitlittle)
add(tmp, Lc, expand=TRUE)


M=gslider(from=0.5,to=3,by=.01,value=0)
tmp = gframe("M/K", container = YieldperRecruitlittle)
add(tmp, M, expand=TRUE)


plotyperr <- function(h,...){ 
visible(YieldperRecruitgraphic) <- TRUE #make correct picture
Yieldbiomass <- yield_biomass_per_recruit(svalue(M),svalue(Kypr),svalue(Lc),svalue(Linfypr),Pi=(YieldProbs),pas=NULL)
plot_yield_biomass_per_recruit(Yieldbiomass,YieldProbs,datain$ML)
visible(YieldperRecruitgraphic) <- TRUE #make correct picture
visible(YieldperRecruitgraphic2) <- TRUE #make correct picture
Yieldbiomass <- yield_biomass_per_recruitf(svalue(M),svalue(Kypr),svalue(Lc),svalue(Linfypr),Pi=(YieldProbs),pas=NULL)
plot_yield_biomass_per_recruitf(Yieldbiomass,YieldProbs,datain$ML)
visible(YieldperRecruitgraphic2) <- TRUE #make correct picture
visible(YieldperRecruitgraphic) <- TRUE #make correct picture
#print("Yieldprobs")
#print(YieldProbs)
 }


 plot=gbutton("Make plot",handler=plotyperr)
 tmp=gframe("Plot",container=YieldperRecruitlittle)
 add(tmp, plot, expand=FALSE)


#Add sponsors logo at the bottom of the page
addSpace(YieldperRecruitlittle,198,horizontal=FALSE)# spacing needs to be tuned for each slide
YieldperRecruitlogo <- ggroup(container=YieldperRecruitlittle,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
gimage("png/logo2.png",dirname=my_path,container=YieldperRecruitlogo)



## ## ## ## Yield per recruit tab isoplath

YieldperRecruitiso <- ggroup(container = nb,label="Y/R iso", expand=TRUE,horizontal=TRUE)#make entry gr
YieldperRecruitisolittle <- ggroup(container=YieldperRecruitiso,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
YieldperRecruitisopic<- gnotebook(container=YieldperRecruitiso,expand=TRUE)#create the Entry pic.
YieldperRecruitisographic<- ggraphics(container = YieldperRecruitisopic,width=700,height=500,label="Isopleth-E")
YieldperRecruitisographic2<- ggraphics(container = YieldperRecruitisopic,width=700,height=500,label="Isopleth-F")


#Add ELEFAN in R logo at the top of the page
gimage("png/logo1.png",dirname=my_path,container=YieldperRecruitisolittle)


Linfypriso=gslider(from=1,to=10,length.out=1000,value=2)
tmp = gframe("Linf", container = YieldperRecruitisolittle)
add(tmp, Linfypriso, expand=TRUE)


Kypriso=gslider(from=0,to=10,by=.01,value=0)
tmp = gframe("K", container = YieldperRecruitisolittle)
add(tmp, Kypriso, expand=TRUE)


Miso=gslider(from=0.5,to=3,by=.01,value=0)
tmp = gframe("M/K", container = YieldperRecruitisolittle)
add(tmp, Miso, expand=TRUE)


hlineiso= gslider(from=0,to=1,by=.01,value=.5)
tmp = gframe("Ruler Lc/Linf", container = YieldperRecruitisolittle)
add(tmp, hlineiso, expand=TRUE)


vlineiso= gslider(from=0,to=1,by=.01,value=.5)
tmp = gframe("Ruler E", container = YieldperRecruitisolittle)
add(tmp, vlineiso, expand=TRUE)


vlineisof= gslider(from=0,to=5,by=.01,value=.5)
tmp = gframe("Ruler F", container = YieldperRecruitisolittle)
add(tmp, vlineisof, expand=TRUE)

nlevels= gslider(from=10,to=50 ,by=1,value=10)
tmp = gframe("Contour Scale", container = YieldperRecruitisolittle)
add(tmp, nlevels, expand=TRUE)


plotyperriso <- function(h,...){ 
visible(YieldperRecruitisographic) <- TRUE #make correct picture
isoplath(svalue(Miso),svalue(Kypriso),svalue(Linfypriso),svalue(vlineiso),svalue(hlineiso),svalue(nlevels),Pi=(YieldProbs),pas=NULL)
visible(YieldperRecruitisographic) <- TRUE #make correct picture  
visible(YieldperRecruitisographic2) <- TRUE #make correct picture
isoplathf(svalue(Miso),svalue(Kypriso),svalue(Linfypriso),svalue(vlineisof),svalue(hlineiso),svalue(nlevels),Pi=(YieldProbs),pas=NULL)
visible(YieldperRecruitisographic2) <- TRUE #make correct picture  

}


 plot=gbutton("Make plot",handler=plotyperriso)
 tmp=gframe("Plot",container=YieldperRecruitisolittle)
 add(tmp, plot, expand=FALSE)


#Add sponsors logo at the bottom of the page
addSpace(YieldperRecruitisolittle,128,horizontal=FALSE)# spacing needs to be tuned for each slide
YieldperRecruitisologo <- ggroup(container=YieldperRecruitisolittle,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
gimage("png/logo2.png",dirname=my_path,container=YieldperRecruitisologo)






## ## ## ## L/F manip. tab


reslicedtemp <- " "
LFmanipplot <- ggroup(container = nb,label="L/F manip", expand=TRUE,horizontal=TRUE)#make entry group
LFmanipplotlittle <- ggroup(container=LFmanipplot,expand=FALSE,horizontal=FALSE,width=330)# make little entry group  
LFmanippic<- gnotebook(container=LFmanipplot,expand=TRUE)#create the Entry pic.
LFmaniphistgraphic<- gtable(reslicedtemp,container=LFmanippic,label="Resliced Data")
names(LFmaniphistgraphic) <- "ML" #lengthunits

#Add ELEFAN in R logo at the top of the page
gimage("png/logo1.png",dirname=my_path,container=LFmanipplotlittle)



slices= gslider(from=.25,to=2, by=.25,value=.25)
tmp = gframe("Slice Scale", container = LFmanipplotlittle)
add(tmp, slices, expand=TRUE)


#gimage("png/usaid.png",dirname=my_path,container=LFmanipplotlogo)
plotlfmanip <- function(h,...){
 hline <- seq(0,ceiling(max(datain$ML))+.5*(datain$ML[2]-datain$ML[1])+svalue(slices),by=svalue(slices))
 #print(hline)
 lftemp<-lfmanipplot(hline)
 filename <- (paste(substr(fname1,start=1,stop=(nchar(fname1)-4)),"resliced.csv",sep="_"))
if(file.exists(filename)){file.remove(filename)}#remove file
 write.table(lftemp,file=filename,row.names=FALSE,col.names=TRUE,append=TRUE,quote=FALSE,sep=",")
 write.table(lengthunits,file=filename,quote=FALSE,append=TRUE,row.names=FALSE,col.names=FALSE)
 
 height<- length(lftemp[,1])
 len<- length(lftemp[1,])
 lftemp[height+1,1] <- as.character(lengthunits)
 lftemp[height+1,2:len] <- as.character(" ")
 dispose(LFmanippic)
 LFmaniphistgraphic<- gtable(lftemp,container=LFmanippic,label="Sliced L/F")
 visible(LFmaniphistgraphic) <- TRUE #make correct picture
 visible(LFmaniphistgraphic) <- TRUE #make correct picture  
 
 }

plot=gbutton("Make plot",handler=plotlfmanip)
tmp=gframe("Plot",container=LFmanipplotlittle)
add(tmp, plot, expand=FALSE)


#Add sponsors logo at the bottom of the page
addSpace(LFmanipplotlittle,396,horizontal=FALSE)# spacing needs to be tuned for each slide
LFmanipplotlogo <- ggroup(container=LFmanipplotlittle,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
gimage("png/logo2.png",dirname=my_path,container=LFmanipplotlogo) 
svalue(nb)=1#change to correct starting window.
visible(window) <- TRUE


}

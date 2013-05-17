### Be sure to set the working directory to a mylocation/ELEFAN 
## Just showing Danielle how pull requests work
## for relative paths to work.
### Change below:
setwd("../")
library(gWidgets)
library(MASS)
library(Rcpp) # connects to c++ programs
options("guiToolkit"="RGtk2")
source("R/main.R")
sourceCpp("src/growth_curve.cpp") #compiles and sources. 
#create the Main window.

size=1000
window = gwindow("ELEFAN in R",height=size,width=1.618*size,visible=TRUE)

biggroup <- ggroup(container=window,expand=TRUE,horizontal=FALSE)

#make big note book!
nb <- gnotebook(container=biggroup,expand=TRUE,horizontal=TRUE)

 

#%############################################################
#Make the Entry page!
datetmp <- NA
data <- NA
#visible(nb[1]) <- TRUE
Entry <- ggroup(container = nb,label="Tab. data", expand=TRUE,horizontal=TRUE)#make entry gr
addSpace(biggroup,300,horizontal=FALSE)
Entrylittle <- ggroup(container=Entry,expand=FALSE,horizontal=FALSE,height=30)# make little entry group

#Add ELEFAN in R logo at the top of the page
gimage("png/logo1.png",dirname=getwd(),container=Entrylittle)

Entrypic<- gnotebook(container=Entry,expand=TRUE)#create the Entry pic.
Datetable<- gtable(datetmp,container=Entrypic,label="Date")
Datatable<- gtable(data,container=Entrypic,label="Data")

#Load Data
#I need handlers.
datefileinh <- function(h,...){
  datefilein()
  datetmp <- date
  datetmp[,1] <- as.character(date[,1])
  datetmp[,1] <- as.character(date[,1])
  visible(Datetable) <- FALSE
  Datetable[] <- datetmp
  Datetable[] <- datetmp
  stdate[] <- 1:length(date[,2])
  stdatek[] <- 1:length(date[,2])
  visible(Datetable) <- TRUE
  }
lffileinh <- function(h,...){
  lffilein()
  visible(Datatable) <- FALSE
  Datatable[] <- data
  Datatable[] <- data
  visible(Datatable) <- TRUE
  Linfslide[] <- seq(0,1.2*max(data$ML),length.out=100)
  Linfslidek[] <- seq(0,1.2*max(data$ML),length.out=100)
  Linfslidec[] <- seq(0,1.22*max(data$ML),length.out=100)
  Pointslide[] <- 2:length(data$ML)
  Pointslideuc[] <- 1:length(data$ML)
  Pointslidelc[] <- 1:length(data$ML)
  midlength[] <- data$ML
  midlengthk[] <- data$ML
  }


#adding buttons



readdatefile=gbutton("Date file",handler=datefileinh)
readlengthfile=gbutton("Length file",handler=lffileinh)
tmp = gframe("Read in date file", container = Entrylittle)
add(tmp, readdatefile, expand=TRUE)
tmp = gframe("Read in length file", container = Entrylittle)
add(tmp, readlengthfile, expand=TRUE)

#Add sponsors logo at the bottom of the page
addSpace(Entrylittle,460,horizontal=FALSE)# spacing needs to be tuned for each slide
#addSpring(Entrylittle)
Entrylogo <- ggroup(container=Entrylittle,expand=TRUE,horizontal=FALSE,width=200)# make little entry group
gimage("png/logo2.png",dirname=getwd(),container=Entrylogo)


## ## #Make Length Frequency plot window.

LFplot <- ggroup(container = nb,label="L/F", expand=TRUE,horizontal=TRUE)#make entry group
LFplotlittle <- ggroup(container=LFplot,expand=FALSE,horizontal=FALSE,width=200)# make little entry group  
LFpic<- gnotebook(container=LFplot,expand=TRUE)#create the Entry pic.

#Add ELEFAN in R logo at the top of the page
gimage("png/logo1.png",dirname=getwd(),container=LFplotlittle)

histgraphic<- ggraphics(container = LFpic,width=700,height=500,label="L/F")
refactorgraphic<- ggraphics(container = LFpic,width=700,height=500,label="Restruct. L/F")


Linfslide = gslider(from=1,to=10,length.out=100,value=2)
tmp = gframe("L infinity", container = LFplotlittle)
add(tmp, Linfslide, expand=TRUE)

Kslide = gslider(from=0,to=10,by=.001,value=0)
tmp = gframe("K", container = LFplotlittle)
add(tmp, Kslide, expand=TRUE)

Cslide= gslider(from=0,to=2,by=.01,value=0)
tmp = gframe("C", container = LFplotlittle)
add(tmp, Cslide, expand=TRUE)

twslide=gslider(from=0,to=1,by=.01,value=0)
tmp = gframe("WP", container = LFplotlittle)
add(tmp, twslide, expand=TRUE)

scaleslide= gslider(from=0,to=4,by=.01,value=1)
tmp = gframe("Scale", container = LFplotlittle)
add(tmp, scaleslide, expand=TRUE)

  stdate <- gdroplist(list(1:10*0))
  tmp <- gframe("SS",container=LFplotlittle)
  add(tmp,stdate, expand=FALSE) 

  midlength <- gdroplist(list(1:20*0))
  tmp <- gframe("ML",container=LFplotlittle)
  add(tmp,midlength, expand=FALSE) 

#gimage("png/usaid.png",dirname=getwd(),container=LFplotlogo)
plotlf <- function(h,...){
  sdate <- as.numeric(svalue(stdate))
  sdate <- sdate+1#converting to real sample number.
  ML <- as.numeric(svalue(midlength))
 visible(histgraphic) <- TRUE #make correct picture  
 plotpeak(svalue(Linfslide),svalue(Kslide),svalue(Cslide),svalue(twslide),ptype="LF",sdate,ML,svalue(scaleslide))
 visible(refactorgraphic) <- TRUE #make correct picture  
 plotpeak(svalue(Linfslide),svalue(Kslide),svalue(Cslide),svalue(twslide),ptype="Peaks",sdate,ML,svalue(scaleslide))
 }


 addSpace(LFplotlittle,40,horizontal=FALSE)# Daniel wants the "plot" buttons spaced farther from the other buttons
 plot=gbutton("Make plot",handler=plotlf)
 tmp=gframe("Plot",container=LFplotlittle)
 add(tmp, plot, expand=FALSE)

#Add sponsors logo at the bottom of the page
addSpace(LFplotlittle,76,horizontal=FALSE)# spacing needs to be tuned for each slide
LFplotlogo <- ggroup(container=LFplotlittle,expand=FALSE,horizontal=FALSE,width=200)# make little entry group
gimage("png/logo2.png",dirname=getwd(),container=LFplotlogo)

## ## ## ## #Make Wetherall plot Window
Wetherallplot <- ggroup(container = nb,label="W. plot", expand=TRUE,horizontal=TRUE)#make entry group
Wetherallplotlittle <- ggroup(container=Wetherallplot,expand=FALSE,horizontal=FALSE,width=200)# make little entry group
Wetherallpic<- gnotebook(container=Wetherallplot,expand=TRUE)#create the Entry pic.
Wetherallgraphic<- ggraphics(container = Wetherallpic,width=700,height=500,label="Wetherall plot")

#Add ELEFAN in R logo at the top of the page
gimage("png/logo1.png",dirname=getwd(),container=Wetherallplotlittle)


Pointslide = gslider(from=1,to=50,by=1,value=4)
tmp = gframe("Points in plot", container = Wetherallplotlittle)
add(tmp, Pointslide, expand=TRUE)
 
plotweth <- function(h,...){ 
visible(Wetherallgraphic) <- TRUE #make correct picture  
 wetherall(data,svalue(Pointslide))
 }


addSpace(Wetherallplotlittle,40,horizontal=FALSE)# Daniel wants the "plot" buttons spaced farther from the other buttons
plot=gbutton("Make plot",handler=plotweth)
tmp=gframe("Plot",container=Wetherallplotlittle)
add(tmp, plot, expand=FALSE)


#Add sponsors logo at the bottom of the page
addSpace(Wetherallplotlittle,402,horizontal=FALSE)# spacing needs to be tuned for each slide
Wetherallplotlogo <- ggroup(container=Wetherallplotlittle,expand=FALSE,horizontal=FALSE,width=200)# make little entry group
gimage("png/logo2.png",dirname=getwd(),container=Wetherallplotlogo)

## ## ## ## # Make Kscan Window
Kscanplot <- ggroup(container = nb,label="K-scan", expand=TRUE,horizontal=TRUE)#make entry group
Kscanplotlittle <- ggroup(container=Kscanplot,expand=FALSE,horizontal=FALSE,width=200)# make little entry group
Kscanplotpic<- gnotebook(container=Kscanplot,expand=TRUE)#create the Entry pic.
Kscangraphic<- ggraphics(container = Kscanplotpic,width=700,height=500,label="K-scan")

#Add ELEFAN in R logo at the top of the page
gimage("png/logo1.png",dirname=getwd(),container=Kscanplotlittle)


Linfslidek = gslider(from=1,to=10,length.out=100,value=2)
tmp = gframe("L infinity", container = Kscanplotlittle)
add(tmp, Linfslidek, expand=TRUE)
Cslidek= gslider(from=0,to=2,by=.01,value=0)
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
tmp <- gframe("ML",container=Kscanplotlittle)
add(tmp,midlengthk, expand=FALSE) 
computefullkscan <- function(h,...){
ckscan(Linf=svalue(Linfslidek),c=svalue(Cslidek),tw=svalue(twslidek))
}
computefixedkscan <- function(h,...){
print(svalue(stdatek))
cfixedkscan(sdate=svalue(stdatek),ML=svalue(midlengthk),Linf=svalue(Linfslidek),C=svalue(Cslidek),tw=svalue(twslidek))
}
plotfullkscan <- function(h,...){ 
visible(Kscangraphic) <- TRUE #make correct picture  
kscanplot(window=svalue(movingaveragek))
 }
plotfixedkscan <- function(h,...){ 
visible(Kscangraphic) <- TRUE #make correct picture  
fixedkscanplot(window=svalue(movingaveragek))
 }

addSpace(Kscanplotlittle,40,horizontal=FALSE)# Daniel wants the "plot" buttons spaced farther from the other buttons
compute=gbutton("Start full scan",handler=computefullkscan)
tmp=gframe("K-scan",container=Kscanplotlittle)
add(tmp, compute, expand=FALSE)
plot=gbutton("Make plot",handler=plotfullkscan)
tmp=gframe("Plot",container=Kscanplotlittle)
add(tmp, plot, expand=FALSE)

compute=gbutton("Start fixed scan",handler=computefixedkscan)
tmp=gframe("K-scan",container=Kscanplotlittle)
add(tmp, compute, expand=FALSE)

plot=gbutton("Make plot",handler=plotfixedkscan)
tmp=gframe("Plot",container=Kscanplotlittle)
add(tmp, plot, expand=FALSE)

#Add sponsors logo at the bottom of the page
#addSpace(Kscanplotlittle,19,horizontal=FALSE)# spacing needs to be tuned for each slide
Kscanplotlogo <- ggroup(container=Kscanplotlittle,expand=FALSE,horizontal=FALSE,width=200)# make little entry group
gimage("png/logo2.png",dirname=getwd(),container=Kscanplotlogo)



## #Make Catch curve wind-points-pointsow

Catchcurveplot <- ggroup(container = nb,label="C.C. I", expand=TRUE,horizontal=TRUE)#make entry group

Catchcurvelittle <- ggroup(container=Catchcurveplot,expand=FALSE,horizontal=FALSE,with=200)# make little entry group
Catchcurvepic<- gnotebook(container=Catchcurveplot,expand=TRUE)#create the Entry pic.
Catchcurvegraphic<- ggraphics(container = Catchcurvepic,width=700,height=500,label="C.C. non seasonal")
Datatablemodified<- gtable(data,container=Catchcurvepic,label="Modified data")

#Add ELEFAN in R logo at the top of the page
gimage("png/logo1.png",dirname=getwd(),container=Catchcurvelittle)


Klocslidec=gslider(from=0,to=1,by=.01,value=0)
tmp = gframe("K", container = Catchcurvelittle)
add(tmp, Klocslidec, expand=TRUE)


Linfslidec=gslider(from=0,to=100,by=.01,value=0)
tmp = gframe("L infinity", container = Catchcurvelittle)
add(tmp, Linfslidec, expand=TRUE)



Cslidec=gslider(from=0,to=100,by=.01,value=0)
tmp = gframe("C", container = Catchcurvelittle)
add(tmp, Cslidec, expand=TRUE)


TWslidec=gslider(from=0,to=100,by=.01,value=0)
tmp = gframe("WP", container = Catchcurvelittle)
add(tmp, TWslidec, expand=TRUE)



Pointslidelc=gslider(from=0,to=10,by=1,value=0)
tmp = gframe("First point", container = Catchcurvelittle)
add(tmp, Pointslidelc, expand=TRUE)

Pointslideuc=gslider(from=0,to=10,by=1,value=0)
tmp = gframe("Last point", container = Catchcurvelittle)
add(tmp, Pointslideuc, expand=TRUE)


plotnonseacatch <- function(h,...){ 
visible(Catchcurvegraphic) <- TRUE #make correct picture
temp<- plotnonseacatchcurve(svalue(Klocslidec),svalue(Linfslidec),svalue(Pointslideuc),svalue(Pointslidelc))
Datatablemodified[] <- temp
Datatablemodified[] <- temp
filename <- (paste(fname2,"corrected.dat",sep="_"))
print(temp)
if(file.exists(filename)){file.remove(filename)}#remove file
write.matrix(temp,file=filename)

#lapply(t(temp), write, filename, append=TRUE, ncolumns=1000)#write to file
visible(Catchcurvegraphic) <- TRUE #make correct picture  
 }

 addSpace(Catchcurvelittle,40,horizontal=FALSE)# Daniel wants the "plot" buttons spaced farther from the other buttons
 plot=gbutton("Make plot",handler=plotnonseacatch)
 tmp=gframe("Plot",container=Catchcurvelittle)
 add(tmp, plot, expand=FALSE)






plotseacatch <- function(h,...){ 
visible(SeasonalCatchgraphic) <- TRUE #make correct picture
temp<- plotseacatchcurve(svalue(Klocslidec),svalue(Linfslidec),svalue(Cslidec),svalue(TWslidec))
Datatablemodified[] <- temp
Datatablemodified[] <- temp
visible(Catchcurvegraphic) <- TRUE #make correct picture
 }



plotrecruit <- function(h,...){ 
visible(RecruitmentPatterngraphic) <- TRUE #make correct picture
temp<- plotseacatchcurve(svalue(Klocslidec),svalue(Linfslidec),svalue(Cslidec),svalue(TWslidec))
Datatablemodified[] <- temp
Datatablemodified[] <- temp
visible(RecruitmentPatterngraphic) <- TRUE #make correct picture
 }


 plot=gbutton("Make plot",handler=plotseacatch)
 tmp=gframe("Plot",container=Catchcurvelittle)
 add(tmp, plot, expand=FALSE)



datetmp <- NA
data <- NA

#visible(nb[1]) <- TRUE

#Add sponsors logo at the bottom of the page
addSpace(Catchcurvelittle,55,horizontal=FALSE)# spacing needs to be tuned for each slide
Catchcurvelogo <- ggroup(container=Catchcurvelittle,expand=FALSE,horizontal=FALSE,width=200)# make little entry group
gimage("png/logo2.png",dirname=getwd(),container=Catchcurvelogo)


SeasonalCatch <- ggroup(container = nb,label="C.C. II", expand=TRUE,horizontal=TRUE)#make entry gr
SeasonalCatchlittle <- ggroup(container=SeasonalCatch,expand=FALSE,horizontal=FALSE)# make little entry group
SeasonalCatchpic<- gnotebook(container=SeasonalCatch,expand=TRUE)#create the Entry pic.
SeasonalCatchgraphic<- ggraphics(container = SeasonalCatchpic,width=700,height=500,label="Catch Curve Plot Seasonal")

 plot=gbutton("Make the plots",handler=plotseacatch)
 tmp=gframe("Plot",container=SeasonalCatchlittle)
 add(tmp, plot, expand=TRUE)



RecruitmentPattern <- ggroup(container = nb,label="Recr.", expand=TRUE,horizontal=TRUE)#make entry gr

RecruitmentPatternlittle <- ggroup(container=RecruitmentPattern,expand=FALSE,horizontal=FALSE)# make little entry group
RecruitmentPatternpic<- gnotebook(container=RecruitmentPattern,expand=TRUE)#create the Entry pic.
RecruitmentPatterngraphic<- ggraphics(container = RecruitmentPatternpic,width=700,height=500,label="Catch Curve Plot Seasonal")

 plot=gbutton("Make the plots",handler=plotrecruit)
 tmp=gframe("Plot",container=RecruitmentPatternlittle)
 add(tmp, plot, expand=TRUE)





YeildperRecruit <- ggroup(container = nb,label="Y/R", expand=TRUE,horizontal=TRUE)#make entry gr
YeildperRecruitlittle <- ggroup(container=YeildperRecruit,expand=FALSE,horizontal=FALSE)# make little entry group


BiomassperRecruit <- ggroup(container = nb,label="B/R", expand=TRUE,horizontal=TRUE)#make entry gr
BiomassperRecruit <- ggroup(container=BiomassperRecruit,expand=FALSE,horizontal=FALSE)# make little entry group

svalue(nb)=1
visible(window) <- TRUE




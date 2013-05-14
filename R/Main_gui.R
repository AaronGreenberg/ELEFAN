### Be sure to set the working directory to a mylocation/ELEFAN 
## Just showing Danielle how pull requests work
## for relative paths to work.
### Change below:
setwd("~/ELEFAN")
library(gWidgets)
library(MASS)
library(Rcpp) # connects to c++ programs
options("guiToolkit"="RGtk2")
source("R/main.R")
sourceCpp("src/growth_curve.cpp") #compiles and sources. 
#create the Main window.

size=1000
window = gwindow("New ELEFAN",height=size,width=1.618*size,visible=TRUE)
biggroup <- ggroup(container=window,expand=TRUE,horizontal=FALSE)

#make big note book!
nb <- gnotebook(container=biggroup,expand=TRUE,horizontal=TRUE)

 

#%############################################################
#Make the Entry page!
datetmp <- NA
data <- NA
#visible(nb[1]) <- TRUE
Entry <- ggroup(container = nb,label="Data Display", expand=TRUE,horizontal=TRUE)#make entry gr
addSpace(biggroup,300,horizontal=FALSE)
Entrylittle <- ggroup(container=Entry,expand=FALSE,horizontal=FALSE,height=30)# make little entry group

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
addSpace(Entrylittle,700,horizontal=FALSE)# needs to be tuned for each slide
#addSpring(Entrylittle)
Entrylogo <- ggroup(container=Entrylittle,expand=TRUE,horizontal=FALSE,width=200)# make little entry group
gimage("png/usaid.png",dirname=getwd(),container=Entrylogo)


## ## #Make Length Frequency plot window.

LFplot <- ggroup(container = nb,label="Length Frequency Plots", expand=TRUE,horizontal=TRUE)#make entry group
LFplotlittle <- ggroup(container=LFplot,expand=FALSE,horizontal=FALSE,width=200)# make little entry group  
LFpic<- gnotebook(container=LFplot,expand=TRUE)#create the Entry pic.

histgraphic<- ggraphics(container = LFpic,width=700,height=500,label="LF Plot")
refactorgraphic<- ggraphics(container = LFpic,width=700,height=500,label="Restructured Plot")
LFplotlogo <- ggroup(container=LFplotlittle,expand=FALSE,horizontal=FALSE,width=200)# make little entry group
gimage("png/usaid.png",dirname=getwd(),container=LFplotlogo)

Linfslide = gslider(from=1,to=10,length.out=100,value=2)
tmp = gframe("Linf", container = LFplotlittle)
add(tmp, Linfslide, expand=TRUE)

Kslide = gslider(from=0,to=10,by=.001,value=0)
tmp = gframe("K", container = LFplotlittle)
add(tmp, Kslide, expand=TRUE)

Cslide= gslider(from=0,to=2,by=.01,value=0)
tmp = gframe("C", container = LFplotlittle)
add(tmp, Cslide, expand=TRUE)

twslide=gslider(from=0,to=1,by=.01,value=0)
tmp = gframe("Wp", container = LFplotlittle)
add(tmp, twslide, expand=TRUE)

scaleslide= gslider(from=0,to=4,by=.01,value=1)
tmp = gframe("Scale", container = LFplotlittle)
add(tmp, scaleslide, expand=TRUE)

  stdate <- gdroplist(list(1:10*0))
  tmp <- gframe("Start Sample",container=LFplotlittle)
  add(tmp,stdate, expand=TRUE) 

  midlength <- gdroplist(list(1:20*0))
  tmp <- gframe("Mid Length",container=LFplotlittle)
  add(tmp,midlength, expand=TRUE) 
 
plotlf <- function(h,...){
  sdate <- as.numeric(svalue(stdate))
  sdate <- sdate+1#converting to real sample number.
  ML <- as.numeric(svalue(midlength))
 visible(histgraphic) <- TRUE #make correct picture  
 plotpeak(svalue(Linfslide),svalue(Kslide),svalue(Cslide),svalue(twslide),ptype="LF",sdate,ML,svalue(scaleslide))
 visible(refactorgraphic) <- TRUE #make correct picture  
 plotpeak(svalue(Linfslide),svalue(Kslide),svalue(Cslide),svalue(twslide),ptype="Peaks",sdate,ML,svalue(scaleslide))
 }


 plot=gbutton("Make the plots",handler=plotlf)
 tmp=gframe("Plot",container=LFplotlittle)
 add(tmp, plot, expand=TRUE)

## ## ## ## #Make Wetherall plot Window
Wetherallplot <- ggroup(container = nb,label="Wetherall Plot", expand=TRUE,horizontal=TRUE)#make entry group
Wetherallplotlittle <- ggroup(container=Wetherallplot,expand=FALSE,horizontal=FALSE,width=200)# make little entry group
Wetherallpic<- gnotebook(container=Wetherallplot,expand=TRUE)#create the Entry pic.
Wetherallgraphic<- ggraphics(container = Wetherallpic,width=700,height=500,label="Wetherall Plot")

Wetherallplotlogo <- ggroup(container=Wetherallplotlittle,expand=FALSE,horizontal=FALSE,width=200)# make little entry group
gimage("png/usaid.png",dirname=getwd(),container=Wetherallplotlogo)

Pointslide = gslider(from=1,to=50,by=1,value=4)
tmp = gframe("#number of points", container = Wetherallplotlittle)
add(tmp, Pointslide, expand=TRUE)
 
plotweth <- function(h,...){ 
visible(Wetherallgraphic) <- TRUE #make correct picture  
 wetherall(data,svalue(Pointslide))
 }


plot=gbutton("Make the plots",handler=plotweth)
tmp=gframe("Plot",container=Wetherallplotlittle)
add(tmp, plot, expand=TRUE)



## ## ## ## # Make Kscan Window
Kscanplot <- ggroup(container = nb,label="K scan", expand=TRUE,horizontal=TRUE)#make entry group
Kscanplotlittle <- ggroup(container=Kscanplot,expand=FALSE,horizontal=FALSE,width=200)# make little entry group
Kscanplotpic<- gnotebook(container=Kscanplot,expand=TRUE)#create the Entry pic.
Kscangraphic<- ggraphics(container = Kscanplotpic,width=700,height=500,label="K Scan")
Kscanplotlogo <- ggroup(container=Kscanplotlittle,expand=FALSE,horizontal=FALSE,width=200)# make little entry group
gimage("png/usaid.png",dirname=getwd(),container=Kscanplotlogo)
Linfslidek = gslider(from=1,to=10,length.out=100,value=2)
tmp = gframe("Linf", container = Kscanplotlittle)
add(tmp, Linfslidek, expand=TRUE)
Cslidek= gslider(from=0,to=2,by=.01,value=0)
tmp = gframe("C", container = Kscanplotlittle)
add(tmp, Cslidek, expand=TRUE)
twslidek=gslider(from=0,to=1,by=.01,value=0)
tmp = gframe("Wp", container = Kscanplotlittle)
add(tmp, twslidek, expand=TRUE)
movingaveragek=gslider(from=0,to=100,by=1,value=1)
tmp = gframe("Moving Average Window", container = Kscanplotlittle)
add(tmp, movingaveragek, expand=TRUE)
stdatek <- gdroplist(list(1:10*0))
tmp <- gframe("Start Sample",container=Kscanplotlittle)
add(tmp,stdatek, expand=TRUE) 
midlengthk <- gdroplist(list(1:20*0))
tmp <- gframe("Mid Length",container=Kscanplotlittle)
add(tmp,midlengthk, expand=TRUE) 
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
compute=gbutton("Compute Full Kscan",handler=computefullkscan)
tmp=gframe("compute",container=Kscanplotlittle)
add(tmp, compute, expand=TRUE)
plot=gbutton("Full Kscan",handler=plotfullkscan)
tmp=gframe("Plot",container=Kscanplotlittle)
add(tmp, plot, expand=TRUE)

compute=gbutton("Compute Fixed Kscan",handler=computefixedkscan)
tmp=gframe("compute",container=Kscanplotlittle)
add(tmp, compute, expand=TRUE)

plot=gbutton("Fixed Kscan",handler=plotfixedkscan)
tmp=gframe("Plot",container=Kscanplotlittle)
add(tmp, plot, expand=TRUE)




## #Make Catch curve wind-points-pointsow

Catchcurveplot <- ggroup(container = nb,label="Catch Curve", expand=TRUE,horizontal=TRUE)#make entry group

Catchcurvelittle <- ggroup(container=Catchcurveplot,expand=FALSE,horizontal=FALSE,with=200)# make little entry group
Catchcurvepic<- gnotebook(container=Catchcurveplot,expand=TRUE)#create the Entry pic.
Catchcurvegraphic<- ggraphics(container = Catchcurvepic,width=700,height=500,label="Catch Curve Plot Non Seasonal")
Catchcurvelogo <- ggroup(container=Catchcurvelittle,expand=FALSE,horizontal=FALSE,width=200)# make little entry group
Datatablemodified<- gtable(data,container=Catchcurvepic,label="Modified Data")
gimage("png/usaid.png",dirname=getwd(),container=Catchcurvelogo)

Klocslidec=gslider(from=0,to=1,by=.01,value=0)
tmp = gframe("K", container = Catchcurvelittle)
add(tmp, Klocslidec, expand=TRUE)


Linfslidec=gslider(from=0,to=100,by=.01,value=0)
tmp = gframe("Linf", container = Catchcurvelittle)
add(tmp, Linfslidec, expand=TRUE)



Cslidec=gslider(from=0,to=100,by=.01,value=0)
tmp = gframe("C", container = Catchcurvelittle)
add(tmp, Cslidec, expand=TRUE)


TWslidec=gslider(from=0,to=100,by=.01,value=0)
tmp = gframe("TW", container = Catchcurvelittle)
add(tmp, TWslidec, expand=TRUE)



Pointslidelc=gslider(from=0,to=10,by=1,value=0)
tmp = gframe("First Point", container = Catchcurvelittle)
add(tmp, Pointslidelc, expand=TRUE)

Pointslideuc=gslider(from=0,to=10,by=1,value=0)
tmp = gframe("Last Point", container = Catchcurvelittle)
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

 plot=gbutton("Make the plots",handler=plotnonseacatch)
 tmp=gframe("Plot",container=Catchcurvelittle)
 add(tmp, plot, expand=TRUE)






datetmp <- NA
data <- NA
#visible(nb[1]) <- TRUE
SeasonalCatch <- ggroup(container = nb,label="Seasonal Catch Curve", expand=TRUE,horizontal=TRUE)#make entry gr
SeasonalCatchlittle <- ggroup(container=SeasonalCatch,expand=FALSE,horizontal=FALSE)# make little entry group

plotseacatch <- function(h,...){ 
visible(SeasonalCatchgraphic) <- TRUE #make correct picture
temp<- plotseacatchcurve(svalue(Klocslidec),svalue(Linfslidec),svalue(Cslidec),svalue(TWslidec))
Datatablemodified[] <- temp
Datatablemodified[] <- temp
visible(Catchcurvegraphic) <- TRUE #make correct picture
 }

<<<<<<< HEAD

datetmp <- NA
data <- NA
#visible(nb[1]) <- TRUE
SeasonalCatch <- ggroup(container = nb,label="Seasonal Catch Curve", expand=TRUE,horizontal=TRUE)#make entry gr
SeasonalCatchlittle <- ggroup(container=SeasonalCatch,expand=FALSE,horizontal=FALSE)# make little entry group
SeasonalCatchpic<- gnotebook(container=SeasonalCatch,expand=TRUE)#create the Entry pic.
SeasonalCatchgraphic<- ggraphics(container = SeasonalCatchpic,width=700,height=500,label="Catch Curve Plot Seasonal")

 plot=gbutton("Make the plots",handler=plotseacatch)
 tmp=gframe("Plot",container=SeasonalCatchlittle)
 add(tmp, plot, expand=TRUE)



=======
 plot=gbutton("Make the plots",handler=plotseacatch)
 tmp=gframe("Plot",container=SeasonalCatchlittle)
 add(tmp, plot, expand=TRUE)


>>>>>>> 2ece7a0256cbc8403ffc4c5978cc30daf10806a8
RecruitmentPattern <- ggroup(container = nb,label="Recruitment Pattern", expand=TRUE,horizontal=TRUE)#make entry gr
RecruitmentPatternlittle <- ggroup(container=RecruitmentPattern,expand=FALSE,horizontal=FALSE)# make little entry group


YeildperRecruit <- ggroup(container = nb,label="Yield Per Recruit", expand=TRUE,horizontal=TRUE)#make entry gr
YeildperRecruitlittle <- ggroup(container=YeildperRecruit,expand=FALSE,horizontal=FALSE)# make little entry group


BiomassperRecruit <- ggroup(container = nb,label="Biomass Per Recruit", expand=TRUE,horizontal=TRUE)#make entry gr
BiomassperRecruit <- ggroup(container=BiomassperRecruit,expand=FALSE,horizontal=FALSE)# make little entry group

svalue(nb)=1
visible(window) <- TRUE




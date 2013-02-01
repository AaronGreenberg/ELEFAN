library(gWidgetsRGtk2)
library(MASS)
options("guiToolkit"="RGtk2")
source("main.R")
library("tcltk")


#create the Main window.

size=1000
window = gwindow("New ELEFAN",height=size,width=1.618*size)
biggroup <- ggroup(container=window,expand=TRUE,horizontal=FALSE,visible=TRUE)

#make big note book!
nb <- gnotebook(container=biggroup,expand=TRUE,horizontal=TRUE,visible=TRUE)

 

#%############################################################
#Make the Entry page!
datetmp <- NA
data <- NA
Entry <- ggroup(container = nb,label="Data Display", expand=TRUE,horizontal=TRUE,visible=TRUE)#make entry group
Entrylittle <- ggroup(container=Entry,expand=FALSE,horizontal=FALSE,visible=TRUE,width=200)# make little entry group

Entrypic<- gnotebook(container=Entry,expand=TRUE,visible=TRUE)#create the Entry pic.
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
  Linfslide[] <- seq(0,2*max(data$ML),length.out=100)
  Linfslidek[] <- seq(0,2*max(data$ML),length.out=100)
  Linfslidec[] <- seq(0,2*max(data$ML),length.out=100)
  Pointslide[] <- 2:length(data$ML)
  Pointslideuc[] <- 1:length(data$ML)
  Pointslidelc[] <- 1:length(data$ML)
  midlength[] <- data$ML
  midlengthk[] <- data$ML
  }


#adding buttons
Entrylogo <- ggroup(container=Entrylittle,expand=FALSE,horizontal=FALSE,visible=TRUE,width=200)# make little entry group
gimage("png/usaid.png",dirname=getwd(),container=Entrylogo)

readdatefile=gbutton("Date file",handler=datefileinh)
readlengthfile=gbutton("Length file",handler=lffileinh)
tmp = gframe("Read in date file", container = Entrylittle)
add(tmp, readdatefile, expand=TRUE)
tmp = gframe("Read in length file", container = Entrylittle)
add(tmp, readlengthfile, expand=TRUE)



#Make Length Frequency plot window.

LFplot <- ggroup(container = nb,label="Length Frequency Plots", expand=TRUE,horizontal=TRUE,visible=TRUE)#make entry group
LFplotlittle <- ggroup(container=LFplot,expand=FALSE,horizontal=FALSE,visible=TRUE,width=200)# make little entry group  
LFpic<- gnotebook(container=LFplot,expand=TRUE,visible=TRUE)#create the Entry pic.

histgraphic<- ggraphics(container = LFpic,width=700,height=500,label="LF Plot")
refactorgraphic<- ggraphics(container = LFpic,width=700,height=500,label="Peaks Plot")
LFplotlogo <- ggroup(container=LFplotlittle,expand=FALSE,horizontal=FALSE,visible=TRUE,width=200)# make little entry group
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
 plotpeak(svalue(Linfslide),svalue(Kslide),svalue(Cslide),svalue(twslide),ptype="LF",sdate,ML)
 visible(refactorgraphic) <- TRUE #make correct picture  
 plotpeak(svalue(Linfslide),svalue(Kslide),svalue(Cslide),svalue(twslide),ptype="Peaks",sdate,ML)
 }


 plot=gbutton("Make the plots",handler=plotlf)
 tmp=gframe("Plot",container=LFplotlittle)
 add(tmp, plot, expand=TRUE)

#Make Wetherall plot Window
Wetherallplot <- ggroup(container = nb,label="Wetherall Plot", expand=TRUE,horizontal=TRUE,visible=TRUE)#make entry group
Wetherallplotlittle <- ggroup(container=Wetherallplot,expand=FALSE,horizontal=FALSE,visible=TRUE,width=200)# make little entry group
Wetherallpic<- gnotebook(container=Wetherallplot,expand=TRUE,visible=TRUE)#create the Entry pic.
Wetherallgraphic<- ggraphics(container = Wetherallpic,width=700,height=500,label="Wetherall Plot")

Wetherallplotlogo <- ggroup(container=Wetherallplotlittle,expand=FALSE,horizontal=FALSE,visible=TRUE,width=200)# make little entry group
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



# Make Kscan Window

Kscanplot <- ggroup(container = nb,label="K scan", expand=TRUE,horizontal=TRUE,visible=TRUE)#make entry group
Kscanplotlittle <- ggroup(container=Kscanplot,expand=FALSE,horizontal=FALSE,visible=TRUE,width=200)# make little entry group
Kscanplotpic<- gnotebook(container=Kscanplot,expand=TRUE,visible=TRUE)#create the Entry pic.
Kscangraphic<- ggraphics(container = Kscanplotpic,width=700,height=500,label="K Scan")
Kscanplotlogo <- ggroup(container=Kscanplotlittle,expand=FALSE,horizontal=FALSE,visible=TRUE,width=200)# make little entry group
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

movingaveragek=gslider(from=0,to=10,by=1,value=1)
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




#Make Catch curve wind-points-pointsow

Catchcurveplot <- ggroup(container = nb,label="Catch Curve", expand=TRUE,horizontal=TRUE,visible=TRUE)#make entry group
Catchcurvelittle <- ggroup(container=Catchcurveplot,expand=FALSE,horizontal=FALSE,visible=TRUE,width=200)# make little entry group
Catchcurvepic<- gnotebook(container=Catchcurveplot,expand=TRUE,visible=TRUE)#create the Entry pic.
Catchcurvegraphic<- ggraphics(container = Catchcurvepic,width=700,height=500,label="Catch Curve Plot Non Seasonal")
Catchcurvelogo <- ggroup(container=Catchcurvelittle,expand=FALSE,horizontal=FALSE,visible=TRUE,width=200)# make little entry group
Datatablemodified<- gtable(data,container=Catchcurvepic,label="Modified Data")
gimage("png/usaid.png",dirname=getwd(),container=Catchcurvelogo)

Klocslidec=gslider(from=0,to=1,by=.01,value=0)
tmp = gframe("K", container = Catchcurvelittle)
add(tmp, Klocslidec, expand=TRUE)


Linfslidec=gslider(from=0,to=100,by=.01,value=0)
tmp = gframe("Linf", container = Catchcurvelittle)
add(tmp, Linfslidec, expand=TRUE)


Pointslideuc=gslider(from=0,to=10,by=1,value=0)
tmp = gframe("Points upper limit", container = Catchcurvelittle)
add(tmp, Pointslideuc, expand=TRUE)

Pointslidelc=gslider(from=0,to=10,by=1,value=0)
tmp = gframe("Points lower limit", container = Catchcurvelittle)
add(tmp, Pointslidelc, expand=TRUE)


plotcatch <- function(h,...){ 
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

 plot=gbutton("Make the plots",handler=plotcatch)
 tmp=gframe("Plot",container=Catchcurvelittle)
 add(tmp, plot, expand=TRUE)








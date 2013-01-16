library(gWidgetsRGtk2)
options("guiToolkit"="RGtk2")
source("main.R")

#create the Main window.

size=1000
window = gwindow("New ELEFAN",height=size,width=1.618*size)
biggroup <- ggroup(container=window,expand=TRUE,horizontal=FALSE,visible=TRUE)

#make big note book!
nb <- gnotebook(container=biggroup,expand=TRUE,horizontal=TRUE,visible=TRUE)
#Make Catch curve window 
Catchcurveplot <- ggroup(container = nb,label="Catch Curve", expand=TRUE,horizontal=TRUE,visible=TRUE)#make entry group
Catchcurvelittle <- ggroup(container=Catchcurveplot,expand=TRUE,horizontal=FALSE,visible=TRUE)# make little entry group
Catchcurvepic<- gnotebook(container=Catchcurveplot,expand=TRUE,visible=FALSE)#create the Entry pic.
Catchcurvegraphic<- ggraphics(container = Catchcurvepic,width=500,height=500,label="Catch Curve Plot Non Seasonal")
# Make Kscan Window

Kscanplot <- ggroup(container = nb,label="K scan", expand=TRUE,horizontal=TRUE,visible=TRUE)#make entry group
Kscanplotlittle <- ggroup(container=Kscanplot,expand=TRUE,horizontal=FALSE,visible=TRUE)# make little entry group
Kscanplotpic<- gnotebook(container=Kscanplot,expand=TRUE,visible=FALSE)#create the Entry pic.
Kscangraphic<- ggraphics(container = Kscanplotpic,width=500,height=500,label="K Scan")
#Load Data

#Make Wetherall plot Window
Wetherallplot <- ggroup(container = nb,label="Wetherall Plot", expand=TRUE,horizontal=TRUE,visible=TRUE)#make entry group
Wetherallplotlittle <- ggroup(container=Wetherallplot,expand=TRUE,horizontal=FALSE,visible=TRUE)# make little entry group
Wetherallpic<- gnotebook(container=Wetherallplot,expand=TRUE,visible=FALSE)#create the Entry pic.
Wetheralgraphic<- ggraphics(container = Wetherallpic,width=500,height=500,label="Wetherall Plot")


#Make Length Frequency plot window.

LFplot <- ggroup(container = nb,label="Length Frequency Plots", expand=TRUE,horizontal=TRUE,visible=TRUE)#make entry group
LFplotlittle <- ggroup(container=LFplot,expand=TRUE,horizontal=FALSE,visible=TRUE)# make little entry group  
LFpic<- gnotebook(container=LFplot,expand=TRUE,visible=FALSE)#create the Entry pic.

histgraphic<- ggraphics(container = LFpic,width=500,height=500,label="LF Plot")
refactorgraphic<- ggraphics(container = LFpic,width=500,height=500,label="Peaks Plot")

  Linfslide = gslider(from=1,to=10,length.out=100,value=2)
  tmp = gframe("Linf", container = LFplotlittle)
  add(tmp, Linfslide, expand = TRUE)

  Kslide = gslider(from=1,to=100,by=1,value=.2)
  tmp = gframe("K", container = LFplotlittle)
  add(tmp, Kslide, expand = TRUE)

  Cslide= gslider(from=0,to=2,by=.01,value=0)
  tmp = gframe("C", container = LFplotlittle)
  add(tmp, Cslide, expand = TRUE)

  twslide=gslider(from=0,to=1,by=.01,value=0)
  tmp = gframe("Tw", container = LFplotlittle)
  add(tmp, twslide, expand = TRUE)

plotlf <- function(h,...){
 plot(hist(rnorm(10^4)))
 plotpeak(d=days,dm=date,da=data,pd2=lfdata,svalue(Linfslide),svalue(Kslide),svalue(Cslide),svalue(twslide),ptype="peaks",sdate=2,ML=1.05)
 }
 plot=gbutton("Date file",handler=plotlf)
 tmp=gframe("Plot",container=LFplotlittle)
 add(tmp, plot, expand = TRUE)





#%############################################################
#Make the Entry page!
datetmp <- NA
data <- NA
Entry <- ggroup(container = nb,label="Data Display", expand=TRUE,horizontal=TRUE,visible=TRUE)#make entry group
Entrylittle <- ggroup(container=Entry,expand=FALSE,horizontal=FALSE,visible=TRUE,width=200)# make little entry group
Entrypic<- gnotebook(container=Entry,expand=TRUE,visible=FALSE)#create the Entry pic.
Datetable<- gtable(datetmp,container=Entrypic,label="Date")
Datatable<- gtable(data,container=Entrypic,label="Data")

#Load Data
#I need handlers.
datefileinh <- function(h,...){
  datefilein()
  datetmp <- date
  datetmp[,1] <- as.character(date[,1])
  datetmp[,1] <- as.character(date[,1])
  Datetable[] <- datetmp
  }
lffileinh <- function(h,...){
  lffilein()
  Datatable[] <- data
  Linfslide[] <- seq(0,2*max(data$ML),length.out=100)
  }


#adding buttons
readdatefile=gbutton("Date file",handler=datefileinh)
readlengthfile=gbutton("Length file",handler=lffileinh)
tmp = gframe("Read in date file", container = Entrylittle)
add(tmp, readdatefile, expand = TRUE)
tmp = gframe("Read in length file", container = Entrylittle)
add(tmp, readlengthfile, expand = TRUE)




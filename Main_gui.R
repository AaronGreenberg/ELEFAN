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
Catchcurvelittle <- ggroup(container=Catchcurveplot,expand=FALSE,horizontal=FALSE,visible=TRUE,width=200)# make little entry group
Catchcurvepic<- gnotebook(container=Catchcurveplot,expand=TRUE,visible=FALSE)#create the Entry pic.
Catchcurvegraphic<- ggraphics(container = Catchcurvepic,width=700,height=500,label="Catch Curve Plot Non Seasonal")
# Make Kscan Window

Kscanplot <- ggroup(container = nb,label="K scan", expand=TRUE,horizontal=TRUE,visible=TRUE)#make entry group
Kscanplotlittle <- ggroup(container=Kscanplot,expand=TRUE,horizontal=FALSE,visible=TRUE,width=200)# make little entry group
Kscanplotpic<- gnotebook(container=Kscanplot,expand=TRUE,visible=FALSE)#create the Entry pic.
Kscangraphic<- ggraphics(container = Kscanplotpic,width=700,height=500,label="K Scan")
#Load Data

#Make Wetherall plot Window
Wetherallplot <- ggroup(container = nb,label="Wetherall Plot", expand=TRUE,horizontal=TRUE,visible=TRUE)#make entry group
Wetherallplotlittle <- ggroup(container=Wetherallplot,expand=FALSE,horizontal=FALSE,visible=TRUE,width=200)# make little entry group
Wetherallpic<- gnotebook(container=Wetherallplot,expand=TRUE,visible=FALSE)#create the Entry pic.
Wetherallgraphic<- ggraphics(container = Wetherallpic,width=700,height=500,label="Wetherall Plot")

Pointslide = gslider(from=1,to=50,by=1,value=4)
tmp = gframe("#number of points", container = Wetherallplotlittle)
add(tmp, Pointslide, expand = TRUE)

plotweth <- function(h,...){ 
visible(Wetherallgraphic) <- TRUE #make correct picture  
 wetherall(data,svalue(Pointslide))
 }


 plot=gbutton("Make the plots",handler=plotweth)
 tmp=gframe("Plot",container=Wetherallplotlittle)
 add(tmp, plot, expand = TRUE)




#Make Length Frequency plot window.

LFplot <- ggroup(container = nb,label="Length Frequency Plots", expand=TRUE,horizontal=TRUE,visible=TRUE)#make entry group
LFplotlittle <- ggroup(container=LFplot,expand=FALSE,horizontal=FALSE,visible=TRUE,width=20)# make little entry group  
LFpic<- gnotebook(container=LFplot,expand=TRUE,visible=FALSE)#create the Entry pic.

histgraphic<- ggraphics(container = LFpic,width=700,height=500,label="LF Plot")
refactorgraphic<- ggraphics(container = LFpic,width=700,height=500,label="Peaks Plot")

  Linfslide = gslider(from=1,to=10,length.out=100,value=2)
  tmp = gframe("Linf", container = LFplotlittle)
  add(tmp, Linfslide, expand = TRUE)

  Kslide = gslider(from=.001,to=10,by=.001,value=.2)
  tmp = gframe("K", container = LFplotlittle)
  add(tmp, Kslide, expand = TRUE)

  Cslide= gslider(from=0,to=2,by=.01,value=0)
  tmp = gframe("C", container = LFplotlittle)
  add(tmp, Cslide, expand = TRUE)


  twslide=gslider(from=0,to=1,by=.01,value=0)
  tmp = gframe("Tw", container = LFplotlittle)
  add(tmp, twslide, expand = TRUE)

  stdate <- gedit()
  tmp <- gframe("Start Sample",container=LFplotlittle)
  add(tmp,stdate, expand = TRUE) 

  meanlength <- gedit()
  tmp <- gframe("Mean Length",container=LFplotlittle)
  add(tmp,meanlength, expand = TRUE) 

plotlf <- function(h,...){
  sdate <- as.numeric(svalue(stdate))
  sdate <- sdate+1#converting to real sample number.
  ML <- as.numeric(svalue(meanlength))
 visible(histgraphic) <- TRUE #make correct picture  
 plotpeak(svalue(Linfslide),svalue(Kslide),svalue(Cslide),svalue(twslide),ptype="LF",sdate,ML)
 visible(refactorgraphic) <- TRUE #make correct picture  
 plotpeak(svalue(Linfslide),svalue(Kslide),svalue(Cslide),svalue(twslide),ptype="Peaks",sdate,ML)
 }


 plot=gbutton("Make the plots",handler=plotlf)
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
  visible(Datetable) <- FALSE
  Datetable[] <- datetmp
  Datetable[] <- datetmp
  visible(Datetable) <- TRUE
  }
lffileinh <- function(h,...){
  lffilein()
  visible(Datatable) <- FALSE
  Datatable[] <- data
  Datatable[] <- data
  visible(Datatable) <- TRUE
  Linfslide[] <- seq(0,2*max(data$ML),length.out=100)
  Pointslide[] <- 1:length(data$ML)
  }


#adding buttons
readdatefile=gbutton("Date file",handler=datefileinh)
readlengthfile=gbutton("Length file",handler=lffileinh)
tmp = gframe("Read in date file", container = Entrylittle)
add(tmp, readdatefile, expand = TRUE)
tmp = gframe("Read in length file", container = Entrylittle)
add(tmp, readlengthfile, expand = TRUE)




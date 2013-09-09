#' Minimal doc
#' @import gWidgets MASS Rcpp RGtk2 cairoDevice
#' @description minimal documentation for roxygen purposes and could be added later 
#' @export
library(gWidgets)
library(PBSmodelling)
options("guiToolkit"="RGtk2")
source("main.R")
source("yield_biomass_per_recruit.r")
source("plot_yield_biomass_per_recruit.r")


#create the Main window.


YPR_gui <- function(){

size=1000
window = gwindow("ELEFAN in R",height=size,width=1.618*size,visible=TRUE)

biggroup <- ggroup(container=window,expand=TRUE,horizontal=FALSE)

#make big note book!
nb <- gnotebook(container=biggroup,expand=TRUE,horizontal=TRUE)
#Make the Entry page!
YieldProbs<<-NA  #place to store prob.
YieldAges <<- NA#place to store ages
datetmp <- NA
datatmp <- NA
#visible(nb[1]) <- TRUE
Entry <- ggroup(container = nb,label="Tab. data", expand=TRUE,horizontal=TRUE)#make entry gr
addSpace(biggroup,300,horizontal=FALSE)
Entrylittle <- ggroup(container=Entry,expand=FALSE,horizontal=FALSE,height=30,width=330)# make little entry group



Entrypic<- gnotebook(container=Entry,expand=TRUE)#create the Entry pic.
Datetable<- gtable(datetmp,container=NULL,label="Dates")
Datatable<- gtable(datatmp,container=Entrypic,label="Lengths")
names(Datatable) <- "ML"

#Load Data
#I need handlers.
datefileinh <- function(h,...){
  #this function sets correct limits for sliders.
  datefilein()
  datetmp <- datein
  options(digits=3)
  datetmp[,1] <- as.character(datein[,1])
  datetmp[,1] <- as.character(datein[,1])
  visible(Datetable) <- FALSE
  Datetable[] <- datetmp
  Datetable[] <- datetmp
  visible(Datetable) <- TRUE
  Datatable[] <- datain
  Datatable[] <- datain
  visible(Datatable) <- TRUE
 }


#adding buttons



readdatefile=gbutton("Load file",handler=datefileinh)
tmp = gframe("Data", container = Entrylittle)
add(tmp, readdatefile, expand=FALSE)

## ## ## ## Yield per recruit tab

YieldperRecruit <- ggroup(container = nb,label="Y/R", expand=TRUE,horizontal=TRUE)#make entry gr
YieldperRecruitlittle <- ggroup(container=YieldperRecruit,expand=FALSE,horizontal=FALSE,width=330)# make little entry group
YieldperRecruitpic<- gnotebook(container=YieldperRecruit,expand=TRUE)#create the Entry pic.
YieldperRecruitgraphic<- ggraphics(container = YieldperRecruitpic,width=700,height=500,label="Y/R")


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
#This is where you set the dummy probabilities.

YieldProbs <- seq(1:13)*0+.10

plotyperr <- function(h,...){ 
visible(YieldperRecruitgraphic) <- TRUE #make correct picture
Yieldbiomass <- yield_biomass_per_recruit(svalue(M),svalue(Kypr),svalue(Lc),svalue(Linfypr),Pi=(YieldProbs),pas=NULL)
plot_yield_biomass_per_recruit(Yieldbiomass,YieldProbs,datain$ML)
visible(YieldperRecruitgraphic) <- TRUE #make correct picture  
 }


plot=gbutton("Make plot",handler=plotyperr)
tmp=gframe("Plot",container=YieldperRecruitlittle)
add(tmp, plot, expand=FALSE)



}
YPR_gui()

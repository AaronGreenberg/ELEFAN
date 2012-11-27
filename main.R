## % This file organizes several other files to make ELEFAN a reality
## %############################################################
## %############################################################
#%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Preamble load required packages,source files and make gui.
#%############################################################
require(compiler)
require(PBSmodelling);                  #nice software that makes making gui easier
source("ASP.R")                         #load routines to compute peaks and available sum of peaks
source("ESP.R")                         #load routines to compute Explained Sum of Peakz
source("catchcurves.R")
source("LF_plots.R")                    #load routines to make the special plots in particular rqFreqplot
source("other_plots.R")                 #load routines that make other plots
source("low_level.R")                   #load misc routines really mostly data proccessing     
createWin("main_gui.txt")



## % Read in the data 
## %############################################################
## %############################################################
## %

datefilein <- function(){
fname1 <- selectFile()
date<-read.table(fname1,head=TRUE,as.is=TRUE)  #read in date key for the length frequency data This should probably be documented
date$Date=(as.Date(date$Date,format="%d/%m/%y"))       #convert dates into the date class.
datelength <- length(date$Date)                                      #get number of days data was collected
days= as.numeric(julian(date$Date[datelength])-julian(date$Date[1])) #set default number of days
moddays=(365-days%%365)+days                                         #compute width of plot window in years... 
assign("date", date, envir = .GlobalEnv)
assign("days", moddays, envir = .GlobalEnv)
}


lffilein <- function(){
fname2 <- selectFile()
data<-read.table(fname2,head=TRUE,as.is=TRUE)  #read in date key for the length frequency data This should probably be documented
assign("data", data, envir = .GlobalEnv)
assign("lfbin",length(data$ML),envir=.GlobalEnv)                   #get number of bins
}




## buildgui <- function()
##   {                                     #This just sets initial bounds on the GUI
##     setWinVal(list(Linf.min=0,Linf.max=2*max(data$ML),Linf=max(data$ML)))
##     
##   }






lfgui <- function()
{
  createWin("lf_gui.txt")
  getWinVal(scope="G")
  setWinVal(list(Linf.min=0,Linf.max=2*max(data$ML),Linf=max(data$ML)))
 
}



Kscangui <- function()
{
  createWin("kscan_gui.txt")
  
  print("This is still dummy")
}

Weatherallgui<- function()
{
  createWin("wetherall_gui.txt")
  setWinVal(list(points.min=2,points.max=length(data$ML),points=3))
}


Catchcurvegui<- function()
{
  createWin("catch_gui.txt")
  
}

return2main <- function(){createWin("main_gui.txt")}

## #buildgui()

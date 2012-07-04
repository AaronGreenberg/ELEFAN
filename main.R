## % This file organizes several other files to make ELEFAN a reality
## %############################################################
## %############################################################
#%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Preamble load required packages,source files and make gui.
#%############################################################
require(compiler)
require(PBSmodelling);                  #nice software that makes making gui easier
source("ASP.R")                         #load routines to compute peaks and available sum of peaks
source("LF_plots.R")                    #load routines to make the special plots in particular rqFreqplot
source("ESP.R")                         #load routines to compute Explained Sum of Peakz
source("MISC.R")                        #load misc routines really mostly data proccessing
createWin("ELEFAN.txt");                #Make gui
buildgui <- function()
  {                                     #This just sets initial bounds on the GUI
    setWinVal(list(Linf.min=0,Linf.max=2*max(data$ML),Linf=max(data$ML)))
    setWinVal(list(points.min=1,points.max=length(data$ML),points=2))
  }

## % Read in the data 
## %############################################################
## %############################################################
## %

data<-read.table("test.dat",head=TRUE,as.is=TRUE)      #read in the length frequency data
date<-read.table("datetest.dat",head=TRUE,as.is=TRUE)  #read in date key for the length frequency data This should probably be documented
date$Date=(as.Date(date$Date,format="%d/%m/%y"))       #convert dates into the date class.

datelength <- length(date$Date)                                      #get number of days data was collected
days= as.numeric(julian(date$Date[datelength])-julian(date$Date[1])) #set default number of days
moddays=(365-days%%365)+days                                         #compute width of plot window in years... 
days=moddays                                                         #reset days
lfbin=length(data$ML)                   #get number of bins




buildgui()

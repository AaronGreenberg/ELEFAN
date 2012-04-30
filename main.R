## % This file organizes several other files to make ELEFAN a reality
## %############################################################
## %############################################################
#%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Preamble load required packages,source files and make gui.
#%############################################################
require(compiler)
require(PBSmodelling);                  #nice software that makes making gui easier
#require(TTR) #some timeseries stuff may not be needed
createWin("ELEFAN.txt");                #Make gui
source("ASP.R")                         #load routines to compute peaks and available sum of peaks
source("LF_plots.R")                    #load routines to make the special plots in particular rqFreqplot
source("ESP.R")                         #load routines to compute Explained Sum of Peakz
source("MISC.R")                        #load misc routines really mostly data proccessing


## % Read in the data 
## %############################################################
## %############################################################
## %

data<-read.table("test.dat",head=TRUE,as.is=TRUE)      #read in the length frequency data
date<-read.table("datetest.dat",head=TRUE,as.is=TRUE)  #read in date key for the length frequency data This should probably be documented
date$Date=(as.Date(date$Date,format="%d/%m/%y"))       #convert dates into the date class.
print(date)
## %
## %############################################################
## %############################################################
## %
datelength <- length(date$Date)
days= as.numeric(julian(date$Date[datelength])-julian(date$Date[1]))                               #set default number of days
#print(days)
moddays=(365-days%%365)+days
#print(moddays)
days=moddays
lfbin=length(data$ML)                   #get number of bins
growthdata <- matrix(0,ncol=days,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)


## % MAIN routines
## %############################################################
## %############################################################
## %


lfdata<- fillgrowthdata(date,data,growthdata)
#print(lfdata)
datafreq<-main(data,date$Date)## timecurves <- 1:12
peaks <- fillgrowthdata(date,datafreq$out,growthdata)
## print(peaks)
## test<- ESP(peaks,date,40,.95,1,.2)
## print("TEST")
## print(test)
## out <- goodfit(test,datafreq$asp)
## print(out)

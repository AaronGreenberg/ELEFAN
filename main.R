## % This file organizes several other files to make ELEFAN a reality
## %############################################################
## %############################################################
#%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Preamble load required packages,source files and make gui.
#%############################################################
require(compiler)
require(PBSmodelling);                  #nice software that makes making gui easier
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

datelength <- length(date$Date)                                      #get number of days data was collected
days= as.numeric(julian(date$Date[datelength])-julian(date$Date[1])) #set default number of days
moddays=(365-days%%365)+days                                         #compute width of plot window in years... 
days=moddays                                                         #reset days
lfbin=length(data$ML)                   #get number of bins



## % MAIN routines
## %############################################################
## %############################################################
## %

goodfit <- NULL
getWinVal(scope="L")
growthdata <- matrix(0,ncol=days,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
lfdata<- fillgrowthdata(date,data,growthdata) #make data structure with length frequency data
peaks <- lfrestruc(lfdata)                    #create restructure lfdata into peaks and valleys.
gcurve <- curves(Linf,c,tw,K,data$ML,days,lfdata)      # compute growth curve this has index, day in growthcurve and properbin.
asp <- aspcompute(peaks)                      #compute asp
esp <- espcompute(gcurve,peaks$out,days,data$ML)               #compute esp
gf <- gfcompute(asp,esp)



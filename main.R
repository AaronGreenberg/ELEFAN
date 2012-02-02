## % This file organizes several other files to make ELEFAN a reality
## %############################################################
## %############################################################
## %
#%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Preamble load required packages,source files and make gui.
#%############################################################
require(PBSmodelling);                  #nice software that makes making gui easier
#require(TTR) #some timeseries stuff may not be needed
createWin("ELEFAN.txt");                #Make gui
source("ASP.R")                         #load routines to compute peaks and available sum of peaks
source("LF_plots.R")                    #load routines to make the special plots

## % Read in the data 
## %############################################################
## %############################################################
## %

data<-read.table("test.dat",head=TRUE,as.is=TRUE)     #read in the length frequency data
date<-read.table("datetest.dat",head=TRUE,as.is=TRUE)  #read in date key for the length frequency data This should probably be documented
date$Date=(as.Date(date$Date,format="%d/%m/%y")) #convert dates into the date class.
print(date)
## %
## %############################################################
## %############################################################
## %
days=365                                #set default number of days
lfbin=length(data$ML)                   #get number of bins
growthdata <- matrix(0,ncol=days,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)


## % MAIN routines
## %############################################################
## %############################################################
## %


fillgrowthdata <- function(date,data,growthdata){ 
  ##this function fills in the growth data with the data that is being read in.
  ##It is important to realize that we need to have a data structure that keeps track of time.
  ##This is a really sparse structure, but it allows us keep time proportional.
  interval <- vector()
  
  for(i in 1:(length(date$Date)-1)){
    # compute intervals between dates so that dates are stored correctly. 
    interval[i]=date$Date[i+1]-date$Date[1] 
  }
  
  for(i in 1:(length(date$Date)-1)){#assign lf data to big array of date lf data.
    growthdata[,interval[i]]=data[,i+1]
  }
  return(growthdata)
}





curves <- function(dm=date,ti=1:5*365,Linf,c,tw,K){
  #computes growth curve for optimization
  K <- K/365                            #converts growth parameter from years to days
  xlab1 <-as.Date(dm$Date[1]+1:365)     #creates a vector of dates... this may not be best way to do this
  #print(xlab1)
  c <- Linf*(1-exp(-K*(ti-tw)-(c*K)/(2*pi)*sin(2*pi*(ti-tw)))) #computes growth curve. I am fairly sure this is right, but 
return(list(c=c,xlab=xlab1))
}


curves2 <- function(dm=date,ti=1:5*365,Linf,c,tw,K){
  #computes growth curve for plot
  getWinVal(scope="L");                 #reads in from gui
  K <- K/365                            #converts growth parameter from years to days
  xlab1 <-as.Date(dm$Date[1]+1:365)     #creates a vector of dates... this may not be best way to do this
  print(xlab1)
  c <- Linf*(1-exp(-K*(ti-tw)-(c*K)/(2*pi)*sin(2*pi*(ti-tw)))) #computes growth curve. I am fairly sure this is right, but 
return(list(c=c,xlab=xlab1))
}


plotlf <- function(dm=date,da=data,pd=lfdata,Linf,c,tw,K){
  c1 <- curves2(dm,1:(10*365),Linf,c,tw,K) 
rqFreqPlot(1:365,da$ML,pd,c1)
}

plotpeak <- function(dm=date,da=data,pd=peaks,Linf,c,tw,K){
  c1 <- curves2(dm,1:(10*365),Linf,c,tw,K) 
rqFreqPlot(1:365,da$ML,pd,c1,barscale=10)
}



ESP <- function(data2=data,dm=date){ #this needs rework.
  getWinVal(scope="L");
  count=0;
  ctest <- (curves(dm,1:5*365,Linf,c,tw,K))
  for(i in 2:length(ctest$c)){
   z=(data$ML-ctest$c[i])^2
   k=which.min(z)
   
   if(data[k]<0){
     count=count-data[k]
   }else if(data[k]>0){ 
     count=count+data[k]
     data[k]=0
   }
 }
  print("ESP")
  print(count)
  return(count)
}



lfdata<- fillgrowthdata(date,data,growthdata)
datafreq<-main(data,date$Date)## timecurves <- 1:12
peaks <- fillgrowthdata(date,datafreq$out,growthdata)
explainedpeaks <- ESP(peaks,date)#compute esp

goodfit <-function(esp=explainedpeaks,asp=datafreq$asp){
  gf <- exp(explainedpeaks/sum(datafreq$asp))
print(gf)
}

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
source("LF_plots.R")                    #load routines to make the special plots
source("ESP.R")
source("MISC.R")


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
time=1:(10*365)
lfbin=length(data$ML)                   #get number of bins
growthdata <- matrix(0,ncol=days,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)


## % MAIN routines
## %############################################################
## %############################################################
## %




lfdata<- fillgrowthdata(date,data,growthdata)
datafreq<-main(data,date$Date)## timecurves <- 1:12
peaks <- fillgrowthdata(date,datafreq$out,growthdata)
cESP <- cmpfun(ESP)
cgoodfit <- cmpfun(goodfit)
#test<- cESP(peaks,date,time,40,.95,1,.2)
#out <- cgoodfit(test,datafreq$asp)
#print(out)

## #make plausible ranges of parameter values.
## rLinf <- 49.5 #seq(30,60,1)
## rK <- seq(0,10,.05)
## rc <- seq(0,5,.25)
## rtw <- .95#seq(-1,1,.25)
## explainedpeaks <- gf<- vector()
## fitcrit <- NULL
## #god this is the scaryest loop I have ever written in R... there may be a more flash way of doing this.
## count <- 0
## len <- prod(c(length(rLinf),length(rK),length(rc),length(rtw)))
## for(l in rLinf){
##     for(k in rK){
##       for(c in rc){
##         for(t in rtw){
##           count=count+1
##           explainedpeaks[count] <- cESP(peaks,date,time,l,c,t,k)#compute esp
##           gf[count] <- cgoodfit(explainedpeaks[count],datafreq$asp)
##           fitcrit$gf[count] <- gf[count]
##           fitcrit$esp <- explainedpeaks[count]
##           fitcrit$t[count] <- t
##           fitcrit$c[count] <- c
##           fitcrit$k[count] <- k
##           fitcrit$l[count] <- l
##          # print(explainedpeaks[count])
##           print(gf[count]) 
##           #print(count/len)
##      }
##     }
##   }
## }

## OPTIMIZE <- function(fit=fitcrit){
##   z <- which.max(fit$gf)
##   print("K")
##   print(fitcrit$k[z])
##   print("L")
##   print(fitcrit$l[z])
##   print("c")
##   print(fitcrit$c[z])
##   print("tw")
##   print(fitcrit$t[z])
## }

## OPTIMIZE(fitcrit)

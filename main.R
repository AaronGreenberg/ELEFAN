## Preamble!
##
require(PBSmodelling);
createWin("ELEFAN.txt");
source("ASP.R")
source("LF_plots.R")
##Read Data
data<-read.table("test.dat",head=TRUE,as.is=TRUE)  #read in data
date<-read.table("datetest.dat",head=TRUE,as.is=TRUE)  #read in data
##
date$Date=(as.Date(date$Date,format="%d/%m/%y"))
print(data)
print(date)
date
days=365
lfbin=length(data$ML)
growthdata <- matrix(0,ncol=days,nrow=lfbin)

fillgrowthdata <- function(date,data,growthdata){
  #this function fills in the growth data with the data that is being read in.
  interval <- vector()
  
  for(i in 1:(length(date$Date)-1)){
    # compute intervals between dates so that dates are stored correctly. 
    interval[i]=date$Date[i+1]-date$Date[1] 
  }

  
  for(i in 1:(length(date$Date)-1)){#assign lf data to big array of date lf data.
    growthdata[,interval[i]]=data[,i+1]
  }
}
fillgrowthdata(date,data,growthdata)





## curves <- function(ti=1:12){#plots
##   getWinVal(scope="L");
##   c <- Linf*(1-exp(-K*(ti-tw)-sin(2*pi*(ti-tw))))
## return(c)
## }


## curves2 <- function(ti=1:12,Linf,tw,K){#plots #for optimization
  
##   c <- Linf*(1-exp(-K*(ti-tw)-sin(2*pi*(ti-tw))))
## return(c)
## }

 datafreq<-main(data,date$Date)## timecurves <- 1:12
## plotdata <- as.data.frame(cbind(data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F))
## plotdata2 <- as.data.frame(cbind(data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS))



## plotlf <- function(da=data,pd=plotdata2){
##   c1 <- curves(1:12) 
## rqFreqPlot(1:12,da$ML,pd,c1)
## }

## plotpeak <- function(da=data,pd2=plotdata){
##   c2 <- curves(seq(1,100,.01)) 
## rqFreqPlot(1:12,da$ML,pd2,c2,barcol="red")
## }

## ESP <- function(data2=data){
##   getWinVal(scope="L");
##   count=0;
##   ctest <- (curves(1:12))
##   for(i in 1:12){
##    z=(data$ML-ctest[i])^2
##    k=which.min(z)
##    if(data$F[k]<0)
##      count=count-data$F[k]
##    else
##      count=count+data$F[k]
##      data$F[k]=0
##    }
##   print("ESP")
##   print(count)
## }



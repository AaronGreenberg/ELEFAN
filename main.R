## Preamble!
##
require(PBSmodelling);
require(TTR)
createWin("ELEFAN.txt");
source("ASP.R")
source("LF_plots.R")
##Read Data
data<-read.table("test.dat",head=TRUE,as.is=TRUE)  #read in data
date<-read.table("datetest.dat",head=TRUE,as.is=TRUE)  #read in data
##
date$Date=(as.Date(date$Date,format="%d/%m/%y"))
#print(data)
#print(date)
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
  return(growthdata)
}
 lfdata<- fillgrowthdata(date,data,growthdata)



curves2 <- function(dm=date,ti=1:5*365,Linf,c,tw,K){#plots #for optimization
  getWinVal(scope="L");
  K <- K/365
  xlab1 <-as.Date(dm$Date[1]+1:365)
  c <- Linf*(1-exp(-K*(ti-tw)-(c*K)/(2*pi)*sin(2*pi*(ti-tw))))
return(list(c=c,xlab=xlab1))
}

 datafreq<-main(data,date$Date)## timecurves <- 1:12
  peaks <- fillgrowthdata(date,datafreq$out,growthdata)
## plotdata <- as.data.frame(cbind(data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F))
## plotdata2 <- as.data.frame(cbind(data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS))
#print("testing data freq")
#print(datafreq)


plotlf <- function(dm=date,da=data,pd=lfdata,Linf,c,tw,K){
  c1 <- curves2(dm,1:(10*365),Linf,c,tw,K) 
rqFreqPlot(1:365,da$ML,pd,c1)
}

plotpeak <- function(dm=date,da=data,pd=peaks,Linf,c,tw,K){
  c1 <- curves2(dm,1:(10*365),Linf,c,tw,K) 
rqFreqPlot(1:365,da$ML,pd,c1,barscale=10)
}


 ## plotpeak <- function(da=data[,1],pd2=lfdata){
 ##   c2 <- (1:365)*0# curves(seq(1,100,.01))
 ##   print(pd2)
 ##   rqFreqPlot(1:365,da,pd2,c2,barcol="red")
 ##  }

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



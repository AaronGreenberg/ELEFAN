
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





curves <- function(dm=date,ti=time,Linf,c,tw,K){
  #computes growth curve for optimization
  K <- K/365                            #converts growth parameter from years to days
  xlab1 <-as.Date(dm$Date[1]+1:365)     #creates a vector of dates... this may not be best way to do this
  #print(xlab1)
  cur <- Linf*(1-exp(-K*(ti-tw)))
  #cur <- Linf*(1-exp(-K*(ti-tw)-(c*K)/(2*pi)*sin(2*pi*(ti-tw)))) #computes growth curve. I am fairly sure this is right, but 
return(list(c=cur,xlab=xlab1))
}


curves2 <- function(dm=date,ti=time,Linf,c,tw,K){
  #computes growth curve for plot
  getWinVal(scope="L");                 #reads in from gui
  K <- K/365                            #converts growth parameter from years to days
  tw <- tw/365                          #converts winter point from years to days I think winter point has dimenstion t/year ?
  print(length(ti))
  xlab1 <-as.Date(dm$Date[1]+1:365)     #creates a vector of dates... this may not be best way to do this
  #print(xlab1)
  cur <- Linf*(1-exp(-K*(ti-tw)))
  #cur <- Linf*(1-exp(-K*(ti-tw)-(c*K)/(2*pi)*(sin(2*pi*(ti-tw))-sin(2*pi*(0-tw))))) #computes growth curve. I am fairly sure this is right, but 
return(list(c=cur,xlab=xlab1))
}


plotlf <- function(dm=date,da=data,pd=lfdata,ti=time,Linf,c,tw,K){
  c1 <- curves2(dm,ti,Linf,c,tw,K) 
rqFreqPlot(1:365,da$ML,pd,c1)
}

plotpeak <- function(dm=date,da=data,pd=peaks,ti=time,Linf,c,tw,K){
  c1 <- curves2(dm,ti,Linf,c,tw,K) 
rqFreqPlot(1:365,da$ML,pd,c1,barscale=10)
}

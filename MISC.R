
fillgrowthdata <- function(date,data,growthdata){ 
  ##this function fills in the growth data with the data that is being read in.
  ##It is important to realize that we need to have a data structure that keeps track of time.
  ##This is a really sparse structure, but it allows us keep time proportional.
  interval <- vector()
  
  for(i in 1:(length(date$Date)-1)){
    # compute intervals between dates so that dates are stored correctly. 
    interval[i]=date$Date[i+1]-date$Date[1] 
  }
  
  for(i in 1:(length(date$Date)-1)){#assign lf data to big array of date lf da
    growthdata[,interval[i]]=data[,i+1]
  }
  return(growthdata)
}



curves <- function(dm=date,Linf,c,tw,K){
  #computes growth curve for optimization
  #getWinVal(scope="L");                 #reads in from gui
  K <- K/365                            #converts growth parameter from years to days
  tw <- tw/365                          #converts winter point from years to days I think winter point has dimenstion t/year ?
  xlab1 <-as.Date(dm$Date[1]+1:365)     #creates a vector of dates... this may not be best way to do this
  #print(xlab1)
  cur <- NULL
  cur[1] <- tw
  time=1
  #print((cur[time]<=.95*Linf))
  while((cur[time]<=.95*Linf)|(time%%365!=0)){

    #print((cur[time]<=.95*Linf))
   cur <- c(cur,Linf*(1-exp(-K*(time-tw)-(c*K)/(2*pi)*sin(2*pi*(time-tw))))) #computes growth curve. I am fairly sure this is right, but
   time=time+1
   #print(c(time,(cur[time]<=.95*Linf),(time%%365!=0),cur[time]))
      }
  #print(c(time%%365,length(cur) ))
return(list(c=cur,xlab=xlab1))
}





curves2 <- function(dm=date,Linf,c,tw,K){
  #computes growth curve for optimization
  getWinVal(scope="L");                 #reads in from gui
  K <- K/365                            #converts growth parameter from years to days
  tw <- tw/365                          #converts winter point from years to days I think winter point has dimenstion t/year ?
  xlab1 <-as.Date(dm$Date[1]+1:365)     #creates a vector of dates... this may not be best way to do this
  #print(xlab1)
  cur <- NULL
  cur[1] <- tw
  time=1
  print((cur[time]<=.95*Linf))
  while((cur[time]<=.95*Linf)|(time%%365!=0)){

    #print((cur[time]<=.95*Linf))
   cur <- c(cur,Linf*(1-exp(-K*(time-tw)-(c*K)/(2*pi)*sin(2*pi*(time-tw))))) #computes growth curve. I am fairly sure this is right, but
   time=time+1
   #print(c(time,(cur[time]<=.95*Linf),(time%%365!=0),cur[time]))
      }
  #print(c(time%%365,length(cur) ))
return(list(c=cur,xlab=xlab1))
}



plotlf <- function(d=days,dm=date,da=data,pd=lfdata,Linf,c,tw,K){
  c1 <- curves2(dm,Linf,c,tw,K)
  #print(d)
rqFreqPlot(1:d,da$ML,pd,c1,dm)
}

plotpeak <- function(d=days,dm=date,da=data,pd=peaks,Linf,c,tw,K){
  c1 <- curves2(dm,Linf,c,tw,K)

  #print(pd)
rqFreqPlot(1:d,da$ML,pd,c1,dm,barscale=10)
}

plotwetherall <- function(da=data){
  wetherall(data)
}
wetherall <- function(da=data){
  data2 <- data
  data2$ML <- data$ML*0
  z <- rowSums(data2)
  print(z)
  Li=Liprime=z*0
  for(i in 1:length(data2$ML)){
    Li[i]=data$ML[i]
    Liprime[i]=mean(z[i:length(z)])
  }
  print(Li)
  print(Liprime)
  plot(Li,Liprime)
}


catch <- function(da=data){
  data2 <- data
  data2$ML <- data$ML*0
  z <- rowSums(data2)
  print(z)
  plot(data$ML,z)
}

# Sample Session
report <- function(d=days,dm=date,da=data,pd=lfdata,pd2=peaks,Linf,c,tw,K){
getWinVal(scope="L");                 #reads in from gui
print(dm)
print(da)
library(R2HTML)
HTMLStart(outdir="~/html", file="myreport",extension="html", echo=FALSE, HTMLframe=TRUE)


HTML.title("Dates", HR=1)
HTML(print(dm))

HTML.title("Length Freq Data", HR=1)
HTML(print(da))

HTML.title("Wetherall Plot", HR=1)
wetherall(data)
HTMLplot()
rnorm(10^6)

HTML.title("K scan",HR=1)

HTML.title("Catch Plot", HR=1)
catch(data)
HTMLplot()
rnorm(10^6)
HTML.title("Length Freq plots", HR=1)
plotlf(d=days,dm=date,da=data,pd=lfdata,Linf,c,tw,K)
HTMLplot()
rnorm(10^6)

HTML.title("Peak plots", HR=1)
plotpeak(d=days,dm=date,da=data,pd=peaks,Linf,c,tw,K)
HTMLplot()

HTMLStop()
}

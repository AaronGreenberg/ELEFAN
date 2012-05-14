
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



curves <- function(Linf,c,tw,K,ML,modday,lfdata){
  #computes growth curve for optimization
  
  #getWinVal(scope="L");                 #reads in from gui
  K <- K/365                            #converts growth parameter from years to days
  tw <- tw/365                          #converts winter point from years to days I think winter point has dimenstion t/year ?
  cur <- matrix(0,nrow=1,ncol=4)

  bin <- function(ML,x,lf=lfdata,z=z){
    if(sum(lf[,time%%modday])==0){z <- z}
    else{
      z <- ML[which.min(((ML)-x)^2)]
  }
    return(z)
  }
 time=1
  z=0
  cur[1,] <- c(1,1,tw,bin(ML,tw,lfdata,z))
  
  while((cur[time,3]<=.95*Linf)|(time%%modday!=0)){
   g <- Linf*(1-exp(-K*(time-tw)-(c*K)/(2*pi)*sin(2*pi*(time-tw)))) #computes growth curve. I am fairly sure this is right, but
   #print(time%%modday+1)
   cur <- rbind(cur,c(time,time%%modday,g,bin(ML,g,lfdata,cur[time,4])))
   time=time+1
      }
return(list(c=cur))
}


aspcompute <- function(peaks){sum(peaks$asp[2:length(peaks$asp)])}

espcompute <- function(gcurve,p=peaks$out,modday,ML)
{
  esp=0
  for(time in 1:(length(gcurve$c[,1]))){
    if(sum(p[,time%%modday])==0) {esp=esp}
    else{
      if(gcurve$c[time,4]>0){   
        jim <- which(ML==gcurve$c[time,4])
        esp=esp+p[jim,time%%modday]
        j=0
        while(p[jim+j,time%%modday]>0 ){
          p[jim+j,time%%modday]=0
        
          j=j+1
        }
        indexk=1
        
        while(jim-indexk>0&&p[jim-indexk,time%%modday]>0 ){
          p[jim-indexk,time%%modday]=0
                                        #print("hi")
          indexk=indexk+1
        }
      }
    }
  }
  return(list(esp=esp,peaks2=p))
}

gfcompute <- function(asp,esp){10^(esp$esp/asp)/10}
  
plotlf <- function(d=days,dm=date,da=data,pd=lfdata,curve=gcurve){
rqFreqPlot(1:d,da$ML,pd,curve$c[,4],curve$c[,3],dm)
}



plotpeak <- function(d=days,dm=date,da=data,pd=peaks$out,curve=gcurve){
rqFreqPlot(1:d,da$ML,pd,curve$c[,3],curve$c[,4],dm,barscale=10)
}




plotpeak2 <- function(d=days,dm=date,da=data,pd=esp$peaks2,curve=gcurve){
rqFreqPlot(1:d,da$ML,pd,curve$c[,3],curve$c[,4],dm,barscale=10)
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

## # Sample Session
## report <- function(d=days,dm=date,da=data,pd=lfdata,pd2=peaks,Linf,c,tw,K){
## getWinVal(scope="L");                 #reads in from gui
## print(dm)
## print(da)
## library(R2HTML)
## HTMLStart(outdir="~/html", file="myreport",extension="html", echo=TRUE, HTMLframe=TRUE)


## HTML.title("Dates", HR=1)
## HTML(print(dm))
## HTMLhr()


## HTML.title("Length Freq Data", HR=1)
## HTML(print(da))
## HTMLhr()


## HTML.title("Wetherall Plot", HR=1)
## wetherall(data)
## HTMLplot()
## HTMLhr()
## rnorm(10^7)

## HTML.title("K scan",HR=1)
## ESPplot()
## HTMLplot()
## HTMLhr()
## rnorm(10^7)

## HTML.title("Catch Plot", HR=1)
## catch(data)
## HTMLplot()
## HTMLhr()
## rnorm(10^7)

## HTML.title("Length Freq plots", HR=1)
## plotlf(d=days,dm=date,da=data,pd=lfdata,Linf,c,tw,K)
## HTMLplot()
## HTMLhr()
## rnorm(10^7)

## HTML.title("Peak plots", HR=1)
## plotpeak(d=days,dm=date,da=data,pd=peaks,Linf,c,tw,K)
## HTMLplot()
## HTMLhr()
## HTMLStop()
## }

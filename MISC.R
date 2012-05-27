
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
          indexk=indexk+1
        }
      }
    }
  }
  return(list(esp=esp,peaks2=p))
}

gfcompute <- function(asp,esp){10^(esp$esp/asp)/10}
  
plotlf <- function(d=days,dm=date,da=data,pd=lfdata,curve=gcurve){

goodfit <- NULL
getWinVal(scope="L")
growthdata <- matrix(0,ncol=days,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
lfdata<- fillgrowthdata(date,data,growthdata) #make data structure with length frequency data
peaks <- lfrestruc(lfdata)                    #create restructure lfdata into peaks and valleys.
gcurve <- curves(Linf,c,tw,K,data$ML,days,lfdata)      # compute growth curve this has index, day in growthcurve and properbin.
asp <- aspcompute(peaks)                      #compute asp
esp <- espcompute(gcurve,peaks$out,days,data$ML)               #compute esp
gf <- gfcompute(asp,esp)
 
rqFreqPlot(1:d,da$ML,pd,curve$c[,3],dm)
}

plotpeak <- function(d=days,dm=date,da=data,pd=peaks$out,curve=gcurve){

goodfit <- NULL
getWinVal(scope="L")
growthdata <- matrix(0,ncol=days,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
lfdata<- fillgrowthdata(date,data,growthdata) #make data structure with length frequency data
peaks <- lfrestruc(lfdata)                    #create restructure lfdata into peaks and valleys.
gcurve <- curves(Linf,c,tw,K,data$ML,days,lfdata)      # compute growth curve this has index, day in growthcurve and properbin.
asp <- aspcompute(peaks)                      #compute asp
esp <- espcompute(gcurve,peaks$out,days,data$ML)               #compute esp
gf <- gfcompute(asp,esp)
 
rqFreqPlot(1:d,da$ML,pd,curve$c[,3],dm,barscale=10)
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
  plot(Li,Liprime)
}



plotkscan <- function(Linf,c,tw,smooth,Kmax,data,days,growthdata,lfdata,peaks)
  { 
goodfit <- NULL
getWinVal(scope="L")
growthdata <- matrix(0,ncol=days,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
lfdata<- fillgrowthdata(date,data,growthdata) #make data structure with length frequency data
peaks <- lfrestruc(lfdata)                    #create restructure lfdata into peaks and valleys.
gcurve <- curves(Linf,c,tw,K,data$ML,days,lfdata)      # compute growth curve this has index, day in growthcurve and properbin.
asp <- aspcompute(peaks)                      #compute asp
esp <- espcompute(gcurve,peaks$out,days,data$ML)               #compute esp
gf <- gfcompute(asp,esp)
   
    kscan(Linf,c,tw,smooth,Kmax,data,days,growthdata,lfdata,peaks)
  }

kscan <- function(Linf,c,tw,smooth,Kmax,data,days,growthdata,lfdata,peaks)
{
  K <- seq(.1,Kmax,length.out=1000)
  for(j in 1:1000){
    gcurve <- curves(Linf,c,tw,K[j],data$ML,days,lfdata) #compute growth curve this has index, day in growthcurve and properbin.
    asp <- aspcompute(peaks)                             #compute asp
    esp <- espcompute(gcurve,peaks$out,days,data$ML)     #compute esp
    gf <- gfcompute(asp,esp)                             #compute goodness of fit
    goodfit[j] <- gf
    
  }
  ma <- function(x,n=smooth){filter(x,rep(1/n,n), sides=2)}
  goodfit <- ma(goodfit,smooth)
  plot(K,goodfit,type="l")              #make plots
}


## catch <- function(da=data){
##   data2 <- data
##   data2$ML <- data$ML*0
##   z <- rowSums(data2)
## #  print(z)
##   plot(data$ML,z)
## }

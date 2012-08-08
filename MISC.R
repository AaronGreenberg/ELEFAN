#%############################################################
##this function fills in the growth data with the data that is being read in.
##It is important to realize that we need to have a data structure that keeps track of time.
##This is a really sparse structure, but it allows us keep time proportional.

fillgrowthdata <- function(date,data,growthdata){ 
  interval <- vector()
  for(i in 1:(length(date$Date)-1)){    # compute intervals between dates so that dates are stored correctly. 
    interval[i]=date$Date[i+1]-date$Date[1] 
  }
  for(i in 1:(length(date$Date)-1)){#assign length frequency data to big array of date length frequency data
    growthdata[,interval[i]]=data[,i+1]
  }
  return(growthdata)# return data structure with either zeros or length frequency data by day
}

#%############################################################

curves <- function(Linf,c,tw,K,ML,modday,lfdata,sdate,sML){
  K <- K/365                            #converts growth parameter from years to days
  tw <- tw/365                          #converts winter point from years to days I think winter point has dimenstion t/year ?
  cur <- matrix(0,nrow=1,ncol=4)        #initalize growth curve data structure
  bin <- function(ML,x,lf=lfdata,z=z){  #figure out which width class the growth curve goes through
    if(sum(lf[,time%%modday])==0){z <- z}
    else{
      z <- ML[which.min(((ML)-x)^2)]
  }
    return(z)
  }

  
  sweep <- function(sdate,sML,modday,K,Linf,c,tw)#this function finds the best starting point for the growth curve
    {
      curtemp <- matrix(0,nrow=1,ncol=3)        #initalize growth curve data structure
      dist <- vector(mode="numeric",length=length(1:modday))
      for(tstart in 1:modday){
        time=1
        print("hi--zero")
          print(tstart)
        while((curtemp[time,3]<=.95*Linf)|(time%%modday!=0)){ #Loop over time until the growth curve reaches 95% of L infinity
          if(time<tstart){
            print("hi--one")
            curtemp <- rbind(cur,c(time,time%%modday,0))
          } else
          { print("hi--two")
            g <- Linf*(1-exp(-K*((time))-(c*K)/(2*pi)*sin(2*pi*((time)-tw)))) #computes growth curve. I am fairly sure this is right, but...
            
              print(curtemp) 
              curtemp <-rbind(cur,c(time,time%%modday,g))
                                  print("hi--didn't crash-00")
            print("hi--didn't crash")
          }
          print(time)
          time=time+1
        }
        print(tstart)
        dist[tstart] <- min((curtemp[,3]-sML)^2)
        
      }
      to <- which.min(dist)
      return(to)
    }

  modto <- sweep(sdate,sML,modday,K,Linf,c,tw)
  print("hi modto")
  print(modto)
    ## Initalize things.
  time=1
  
  while((cur[time,3]<=.95*Linf)|(time%%modday!=0)){ #Loop over time until the growth curve reaches 95% of L infinity
   if(time<floor(modto)){
     cur <- rbind(cur,c(time,time%%modday,0,bin(ML,0,lfdata,cur[time,4])))
   }
   if(time>=floor(modto))
   {
   g <- Linf*(1-exp(-K*((time)-modto)-(c*K)/(2*pi)*sin(2*pi*((time)-tw)))) #computes growth curve. I am fairly sure this is right, but...
   cur <- rbind(cur,c(time,time%%modday,g,bin(ML,g,lfdata,cur[time,4])))
   } 
   time=time+1
   
      }

  #print(head(cur,2210))
  #cur[,1] <- cur[,1]+sdate
return(list(c=cur))
}

ccurves<- cmpfun(curves)


aspcompute <- function(peaks){sum(peaks$asp[2:length(peaks$asp)])} #compute sum of asp. 
caspcompute <- cmpfun(aspcompute)
espcompute <- function(gcurve,p=peaks$out,modday,ML)
{                                       #compute ESP
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
cespcompute <- cmpfun(espcompute)
gfcompute <- function(asp,esp){10^(esp$esp/asp)/10}
cgfcompute <- cmpfun(gfcompute)


wetherall <- function(da=data,points=3){
  data2 <- data
  data2$ML <- data$ML*0
  z <- rowSums(data2)
  print(z)
  Li=Liprime=z*0
  for(i in 1:length(data2$ML)){
    Li[i]=data$ML[i]
    Liprime[i]=mean(z[i:length(z)])
  }
  Lipoints=Liprime[(length(Liprime)-points):length(Liprime)]
  Lip=Li[(length(Liprime)-points):length(Liprime)]
  z=lm(Lipoints~Lip)
  inter=as.vector(z$coefficients)
  Linfest=-1*inter[1]/inter[2]
  graphics.off()
  par(1,las=1)
  plot(Li,Liprime,xlim=c(Li[1],Linfest+3))
  
  points(Lip,Lipoints,pch=19,col="red")
  abline(z)
  return(Linfest)
}



kscan <- function(Linf,c,tw,smooth,Kmin,Kmax,sweep,dat=data,d=days,growthdata,lfdata,peaks)
{
  K <- seq(Kmin,Kmax,length.out=sweep)
  for(j in 1:length(K)){
    gcurve <- curves(Linf,c,tw,K[j],dat$ML,days,lfdata) #compute growth curve this has index, day in growthcurve and properbin.
    asp <- caspcompute(peaks)                             #compute asp
    esp <- cespcompute(gcurve,peaks$out,d,dat$ML)     #compute esp
    gf <- cgfcompute(asp,esp)                             #compute goodness of fit
    goodfit[j] <- gf
    
  }




  # x: the vector
# n: the number of samples
# centered: if FALSE, then average current sample and previous (n-1) samples
#           if TRUE, then average symmetrically in past and future. (If n is even, use one more sample from future.)
movingAverage <- function(x, n=1, centered=TRUE) {

    if (centered) {
        before <- floor  ((n-1)/2)
        after  <- ceiling((n-1)/2)
    } else {
        before <- n-1
        after  <- 0
    }

    # Track the sum and count of number of non-NA items
    s     <- rep(0, length(x))
    count <- rep(0, length(x))

    # Add the centered data 
    new <- x
    # Add to count list wherever there isn't a 
    count <- count + !is.na(new)
    # Now replace NA_s with 0_s and add to total
    new[is.na(new)] <- 0
    s <- s + new

    # Add the data from before
    i <- 1
    while (i <= before) {
        # This is the vector with offset values to add
        new   <- c(rep(NA, i), x[1:(length(x)-i)])

        count <- count + !is.na(new)
        new[is.na(new)] <- 0
        s <- s + new

        i <- i+1
    }

    # Add the data from after
    i <- 1
    while (i <= after) {
        # This is the vector with offset values to add
        new   <- c(x[(i+1):length(x)], rep(NA, i))

        count <- count + !is.na(new)
        new[is.na(new)] <- 0
        s <- s + new

        i <- i+1
    }

    # return sum divided by count
    s/count
}
  
   goodfit2 <- movingAverage(goodfit,smooth)
  print(max(goodfit))
  plot(K,goodfit2,type="l",ylim=c(0.0,max(goodfit)+.1))              #make plots
  points(K[which.max(goodfit)],max(goodfit),col="red")
}
ckscan <- cmpfun(kscan)


###THE FUNCTIONS THAT INTERACT WITH THE GUI ARE BELOW!!!
##
plotpeak <- function(d=days,dm=date,da=data,pd2=lfdata,pd=peaks$out,curve=gcurve){
goodfit <- NULL
getWinVal(scope="L")
print("date")
print(Date)
startdate <- as.Date(Date)
startime <- as.numeric(startdate-dm[1,1])
print("stime")
print(startime)
print(startdate)
print("ML")
ML <- as.numeric(ML)
print(ML)
#need to convert start date to dime.
growthdata <- matrix(0,ncol=days,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
lfdata<- fillgrowthdata(date,data,growthdata) #make data structure with length frequency data
peaks <- lfrestruc(lfdata)                    #create restructure lfdata into peaks and valleys.
gcurve <- curves(Linf,c,tw,K,data$ML,days,lfdata,startime,ML)      # compute growth curve this has index, day in growthcurve and properbin.
asp <- aspcompute(peaks)                      #compute asp
esp <- espcompute(gcurve,peaks$out,days,data$ML)               #compute esp
gf <- gfcompute(asp,esp)
graphics.off()

if(ptype=="Peaks"){
  rqFreqPlot(1:d,da$ML,pd,startime,ML,curve$c[,3],dm,barscale=10)
}
if(ptype=="LF"){
  rqFreqPlot(1:d,da$ML,pd2,startime,ML,curve$c[,3],dm)
}
}

plotwetherall <- function(da=data){
  getWinVal(scope="L")
  Linfest<- wetherall(data,(points-1))
  setWinVal(list(Linfest=Linfest))
}


plotkscan <- function(Linf,c,tw,smooth,Kmax,dat=data,d=days,growthdata,lfdata,peaks)
  { 
goodfit <- NULL
getWinVal(scope="L")
growthdata <- matrix(0,ncol=d,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
lfdata<- fillgrowthdata(date,dat,growthdata) #make data structure with length frequency data
peaks <- lfrestruc(lfdata)                    #create restructure lfdata into peaks and valleys.
gcurve <- curves(Linf,c,tw,K,dat$ML,days,lfdata)      # compute growth curve this has index, day in growthcurve and properbin.
asp <- aspcompute(peaks)                      #compute asp
esp <- espcompute(gcurve,peaks$out,d,dat$ML)               #compute esp
gf <- gfcompute(asp,esp)
    ckscan(Linf,c,tw,smooth,Kmin,Kmax,sweep,dat,d,growthdata,lfdata,peaks)
  }



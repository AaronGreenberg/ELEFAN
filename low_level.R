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
        g <- 0
        #print(tstart)
        while((g<=.95*Linf)|(time%%modday!=0)){ #Loop over time until the growth curve reaches 95% of L infinity
          #print(g)
          if(time<tstart){
            if(((time%%modday)==sdate)){curtemp <- rbind(curtemp,c(time,time%%modday,g))}

          } else
          { 
            g <- Linf*(1-exp(-K*((time-tstart))-(c*K)/(2*pi)*(sin(2*pi*((time)-tw)))-sin(2*pi*(tstart-tw)))) #computes growth curve. 
            if(((time%%modday)==sdate)){curtemp <- rbind(curtemp,c(time,time%%modday,g))}
 
            }
          time=time+1
        }

        dist[tstart] <- min((curtemp[,3]-sML)^2)
        
      }

      to <- which.min(dist)
      return(to)
    }
  csweep <- cmpfun(sweep)
  modto <- csweep(sdate,sML,modday,K,Linf,c,tw)
    ## Initalize things.
  time=1
  
  while((cur[time,3]<=.95*Linf)|(time%%modday!=0)){ #Loop over time until the growth curve reaches 95% of L infinity
   if(time<floor(modto)){
     cur <- rbind(cur,c(time,time%%modday,0,bin(ML,0,lfdata,cur[time,4])))
   }
   if(time>=floor(modto))
   {
   g <- Linf*(1-exp(-K*((time)-modto)-(c*K)/(2*pi)*(sin(2*pi*((time)-tw)))-sin(2*pi*(modto-tw)))) #computes growth curve. 
   cur <- rbind(cur,c(time,time%%modday,g,bin(ML,g,lfdata,cur[time,4])))
   } 
   time=time+1
   
      }


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
  #print(z)
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
  plot(Li,Liprime,xlim=c(Li[1],Linfest+3),ylim=c(0,max(Liprime)+6),xlab="Cutoff Length (L' cm)",ylab="Mean Cutoff Length")
  points(Lip,Lipoints,pch=19,col="red")
  text(Li,Liprime+4,as.character(sort(1:length(Liprime),decreasing=TRUE)))
  abline(z)
  return(Linfest)
}


kscan <- function(Linf,c,tw,dat=data,d=days,growthdata,lfdata,peaks)
{  #compute growth curve

  Ksweep <-function(K,Linf,c,tw,asp,stime,MLi,dat=data,d=days,growthdata,lfdata,peaks)
   {
    # it allows for the use of lapply
    gcurve <- ccurves(Linf,c,tw,K,data$ML,d,lfdata,stime,MLi)      # compute growth curve this has index, day in growthcurve and properbin.      
    esp <- cespcompute(gcurve,peaks$out,days,data$ML)               #compute esp
    gf <- gfcompute(asp,esp)
    return(gf)
    }
  
 cKsweep <- cmpfun(Ksweep)
 asp <- caspcompute(peaks)                      #compute asp
 out<- matrix(0,nrow=days*length(dat$ML),ncol=4)
 index <- 1
  for(i in 1:days)
    { 
      for(j in 1:length(dat$ML)){
         K <- seq(.1,3,.1)
           if(sum(lfdata[,i])>=1){
             if(peaks$out[j,i]>=0){
               gf <- K*0
               for(ki in 1:length(K)){
               gf[ki] <- Ksweep(K[ki],Linf,c,tw,asp,i,dat$ML[j],dat,d,growthdata,lfdata,peaks)
            }

               out[index,] <- c(max(gf),K[which.max(gf)],i,dat$ML[j])
               print(out[index,])
               index <- index+1

           }
  
       }
  }
}
  return(out)
}

ckscan <- cmpfun(kscan)























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



growth_rootf <- function(x,K,Linf,C,TW){
#makes computing tstart and time when length is .95%Linf easy.
  out <- Linf*(1-exp(-K*(0+x)))
  return(out)
}

bisect <- function(a,b,equal,K,Linf,C,TW){
  #This function uses bisection to compute the required values of tstart and...
  if((growth_rootf(a,K,Linf,C,TW)-equal)*(growth_rootf(b,K,Linf,C,TW)-equal)>=0){#make sure that inputs are okay... 
    print("f(xup) and f(xlow) are of same sign")
    return(1)} 
termtest <- 1#set counter to protect against errors. 
  while(termtest<= 10000) {# limit iterations to prevent infinite loop
    c <- (a + b)/2 #new midpoint
    if(((growth_rootf(c,K,Linf,C,TW)-equal)==0||(b-a)/2<= 10^-(10))) { #solution found
    return(c)
    break
  }
  termtest <- termtest + 1 #increment step counter
  if(sign(growth_rootf(c,K,Linf,C,TW)-equal) == sign(growth_rootf(a,K,Linf,C,TW)-equal)){ a <- c}
  else{b <- c }# new interval
}
print("Method failed. max number of steps exceeded")#just a nice test to make sure that the first test really worked.
}


curves <- function(Linf,K,sML,C,TW){
  K <- K/365
  C <- C/365
  #first  compute time_start
  timestart <-  bisect(0,200*365,sML,K,Linf,C,TW)
  print(timestart)
  #second compute time of  95%*Linf
  nintyfivetime <-  bisect(0,200*365,.95*Linf,K,Linf,C,TW)
  print(nintyfivetime)
  time <- -(floor(timestart)):ceiling(nintyfivetime)#get time vector.
  cur <- Linf*(1-exp(-K*(time+timestart)))
  plot(time/365,cur,type="l")
  points(0,10)
  #third compute growth curve and put it in the right place.

  
return(list(c=cur))
}


#%############################################################
##this function fills in the growth data with the data that is being read in.
##It is important to realize that we need to have a data structure that keeps track of time.
##This is a really sparse structure, but it allows us keep time proportional.

fillgrowthdata <- function(date,data,growthdata){ 
  interval <- vector()
  for(i in 1:(length(date$Date)-1)){    # compute intervals between dates so that dates are stored correctly. 
    interval[i]=date$Date[i+1]-date$Date[1]
  }
  for(i in 1:(length(date$Date)-1)){    #assign length frequency data to big array of date length frequency data
    growthdata[,interval[i]]=data[,i+1]
  }
  return(growthdata)# return data structure with either zeros or length frequency data by day
}

#%############################################################
#%############################################################

curves <- function(Linf,c,tw,K,ML,modday,lfdata,sdate,sML){
  K <- K/365
  C <- c
  w <- 1/365
  TW <- tw
     
 
growth_rootf <- function(x,K,Linf,C,TW){
#makes computing tstart and time when length is .95%Linf easy.
  w <- 1/365
  period <- (C*K)/(2*pi*w)*(sin(2*pi*w*(0-TW))-sin(2*pi*w*(TW+x)))
  out <- Linf*(1-exp(-K*(0+x)+period))
  return(out)
}

bisect <- function(a,b,equal,K,Linf,C,TW){
  #This function uses bisection to compute the required values of tstart and...
  if((growth_rootf(a,K,Linf,C,TW)-equal)*(growth_rootf(b,K,Linf,C,TW)-equal)>=0){#make sure that inputs are okay... 
    print("f(xup) and f(xlow) are of same sign")
    return(1)} 
termtest <- 1#set counter to protect against errors. 
  while(termtest<= 10000) {# limit iterations to prevent infinite loop
    d <- (a + b)/2 #new midpoint
    if(((growth_rootf(d,K,Linf,C,TW)-equal)==0||(b-a)/2<= 10^-(10))) { #solution found
    return(d)
    break
  }
  termtest <- termtest + 1 #increment step counter
  if(sign(growth_rootf(d,K,Linf,C,TW)-equal) == sign(growth_rootf(a,K,Linf,C,TW)-equal)){ a <- d}
  else{b <- d }# new interval
}
print("Method failed. max number of steps exceeded")#just a nice test to make sure that the first test really worked.
}

  #I should talk to Laura about making things polymorphic... Cause this is ugly and has code redundancy. 
   
growth_rootf2 <- function(x,K,Linf,C,TW,ts){
#makes computing to and time when length is .95%Linf easy. 
  w <- 1/365
  period <- (C*K)/(2*pi*w)*(sin(2*pi*w*(x-TW))-sin(2*pi*w*(TW+ts)))
  out <- Linf*(1-exp(-K*(x+ts)+period))
  return(out)
}

bisect2 <- function(a,b,equal,K,Linf,C,TW,ts){
  #This function uses bisection to compute the required end time values
  if((growth_rootf2(a,K,Linf,C,TW,ts)-equal)*(growth_rootf2(b,K,Linf,C,TW,ts)-equal)>=0){#make sure that inputs are okay... 
    print("f(xup) and f(xlow) are of same sign")
    return(1)} 
termtest <- 1#set counter to protect against errors. 
  while(termtest<= 10000) {# limit iterations to prevent infinite loop
    d <- (a + b)/2 #new midpoint
    if(((growth_rootf2(d,K,Linf,C,TW,ts)-equal)==0||(b-a)/2<= 10^-(10))) { #solution found
    return(d)
    break
  }
  termtest <- termtest + 1 #increment step counter
  if(sign(growth_rootf2(d,K,Linf,C,TW,ts)-equal) == sign(growth_rootf2(a,K,Linf,C,TW,ts)-equal)){ a <- d}
  else{b <- d }# new interval
}
print("Method failed. max number of steps exceeded")#just a nice test to make sure that the first test really worked.
}


  #first  compute time_start
  timestart <-  bisect(0,200*365,sML,K,Linf,C,TW)
 # print(timestart)
  #second compute time of  95%*Linf
  nintyfivetime <-  bisect2(0,200*365,.95*Linf,K,Linf,C,TW,timestart)
 # print(nintyfivetime)
  #compute tzero
  #really only important when C!=0 because it should be about -timestart  but ...
  zerotime <-  bisect2(-200*365,200*365,0,K,Linf,C,TW,timestart)
#  print(zerotime)
  #get vector of times!
upwind <- (ceiling(nintyfivetime))
downwind <- (floor(zerotime))
time <- downwind:upwind
 #third compute growth curve and put it in the right place.
  cur <- matrix(0,(nrow=upwind+(-1)*downwind+1),ncol=4)        #initalize growth curve data structure
  period <- (C*K)/(2*pi*w)*(sin(2*pi*w*(time-TW))-sin(2*pi*w*(TW+timestart)))
  cur[,1] <-(time+sdate)#keep real time
  cur[,2] <-(time+sdate)%%modday #wrap time so mapping the time to the plot is easy
  cur[,3] <- Linf*(1-exp(-K*((time+timestart))+period))#put in the growth curve
  fn <- function(i){ML[which.min(((ML)-cur[i,3])^2)]} #snazzy function that allows use of sapply  to find the right bins!
  cur[,4] <- sapply(1:length(cur[,3]),fn)             #get a version of the growth curve that makes computing esp and asp easy
#  x11()#This plot will probably not be in the final version however debugging plots make me feel warm and fuzzy!
 # plot(time,cur[,3],type="l",xlab="time", ylab="Length",main="Debug growth curve plot")
 # points(0,sML)
#print(head(cur))
return(list(c=cur))
}

ccurves<- cmpfun(curves)


aspcompute <- function(peaks){sum(peaks$asp[2:length(peaks$asp)])} #compute sum of asp. 
caspcompute <- cmpfun(aspcompute)
espcompute <- function(gcurve,p=peaks$out,modday,ML)
{                                       #compute ESP
  peaks2 <- p #need a structure to turn to zero to prevent counting a peak more than once.
#  print(head(peaks2))
  esp <- vector()
  #x11()
#   graphics.off()
 # rqFreqPlot(1:365,ML,peaks2,sdate,13,gcurve,date,barscale=10,xlab="test")
  for(timesweep in 1:length(gcurve$c[,1])){#sweep over time
    gclocation <- ifelse(gcurve$c[timesweep,3]>=min(ML),which(ML==gcurve$c[timesweep,4]),0)#figure out what ML index we are on!
    if(gclocation>0){
    # print("glocation")
    #print(gclocation)
    #print(ML[gclocation])
    tsweep <- timesweep%%modday+1
    #print("tsweep")
    #print(tsweep)
    #print("peaks")  
    #print(peaks2[gclocation,tsweep])
    if(peaks2[gclocation,tsweep]>0){
    esp[timesweep] <- peaks2[gclocation,tsweep]
    peaks2[gclocation,tsweep] <- 0
    }else{ esp[timesweep] <- peaks2[gclocation,tsweep]}
  }else{esp[timesweep] <- 0}
  }
#   print(head(sort(esp,decreasing=TRUE),30))
#   print(head(sort(esp,decreasing=FALSE),30))
#  print(esp[which(esp!=0)])
  ESP=sum(esp)
 # print("ESP==")
 # print(ESP)
  return(list(esp=ESP,peaks2=peaks2))
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
  text(Li,Liprime+1,as.character(sort(1:length(Liprime),decreasing=TRUE)))
  abline(z)
  return(Linfest)
}



kscan <- function(Linf,c,tw,dat=data,d=days,growthdata,lfdata,pd=peaks){
  print("hi My name is Kscan")
  out <- matrix(2,nrow=2,ncol=2)
  #startdate <- as.Date(Date)
  #startime <- as.numeric(startdate-dm[1,1])
  K <- .2
  c <- 0
  tw <- .1
  ML <- 27#as.numeric(ML)
  startime <- 14
  Linf <- 35
                                        #need to convert start date to dime.
  growthdata <- matrix(0,ncol=d,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
  lfdata<- fillgrowthdata(date,dat,growthdata) #make data structure with length frequency data
  peaks <- lfrestruc(lfdata)                    #create restructure lfdata into peaks and valleys.
  index <- 1
  K <- seq(.1,10,.1)
  out <- matrix(0,nrow=length(K)*length(dat$ML)*d,ncol=4)
  for(i in 1:length(K)){
    for(j in 1:length(dat$ML)){
      for(sdate in 1:d){
       # print(c("sdate","j","i"))
       # print(c(sdate,j,i))
        if(peaks$out[j,sdate]>0 & lfdata[j,sdate]>0){
        gcurve <- curves(Linf,c,tw,K[i],dat$ML,days,peaks,sdate,dat$ML[j])      # compute growth curve this has index, day in growthcurve and properbin.
        asp <- aspcompute(pd)                      #compute asp
        esp <- espcompute(gcurve,peaks$out,days,dat$ML)               #compute esp
        gf <- gfcompute(asp,esp)
      } else{gf <- 0}
        if(gf>0){
        print(c(gf,K[i],dat$ML[j],sdate))
      }
        out[index,] <- c(gf,K[i],dat$ML[j],sdate)
#        print(head(out))
        index <- index+1
    }
  }
  }
 
  return(out)
}
ckscan <- cmpfun(kscan)


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




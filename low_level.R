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
  TW <- tw/365
  
 
growth_rootf <- function(x,K,Linf,C,TW){
#makes computing tstart and time when length is .95%Linf easy.
  w <- 1/365
  period <- (C*K)/(2*pi*w)*(sin(2*pi*w*(0-TW-.5/365))-sin(2*pi*w*(x-TW-.5/365)))
  out <- Linf*(1-exp(-K*(0-x)+period))
  return(out)
}
cgrowth_rootf <- cmpfun(growth_rootf)
bisect <- function(a,b,equal,K,Linf,C,TW){
  if((cgrowth_rootf(a,K,Linf,C,TW)-equal)^2<10^(-10)){return(a)}
  if((cgrowth_rootf(b,K,Linf,C,TW)-equal)^2<10^(-10)){return(b)}
  #This function uses bisection to compute the required values of tstart and...
  if((growth_rootf(a,K,Linf,C,TW)-equal)*(growth_rootf(b,K,Linf,C,TW)-equal)>0){#make sure that inputs are okay... 
    print("f(xup) and f(xlow) are of same sign 1")
    print(paste("fxup::",(cgrowth_rootf(a,K,Linf,C,TW)-equal),"fxlow::",(cgrowth_rootf(b,K,Linf,C,TW)-equal)))
    return(1)} 
termtest <- 1#set counter to protect against errors. 
  while(termtest<= 10000) {# limit iterations to prevent infinite loop
    d <- (a + b)/2 #new midpoint
    if(((cgrowth_rootf(d,K,Linf,C,TW)-equal)==0||(b-a)/2<= 10^-(10))) { #solution found
    return(d)
    break
  }
  termtest <- termtest + 1 #increment step counter
  if(sign(cgrowth_rootf(d,K,Linf,C,TW)-equal) == sign(cgrowth_rootf(a,K,Linf,C,TW)-equal)){ a <- d}
  else{b <- d }# new interval
}
print("Method failed. max number of steps exceeded")#just a nice test to make sure that the first test really worked.
}

cbisect <- cmpfun(bisect)
  #I should talk to Laura about making things polymorphic... Cause this is ugly and has code redundancy. 
   
growth_rootf2 <- function(x,K,Linf,C,TW,ts){
#makes computing to and time when length is .95%Linf easy. 
  w <- 1/365
  period <- (C*K)/(2*pi*w)*(sin(2*pi*w*(x-TW-.5/365))-sin(2*pi*w*(ts-TW+.5/365)))
  out <- Linf*(1-exp(-K*(x-ts)+period))
  return(out)
}
cgrowth_rootf2 <- cmpfun(growth_rootf2)
bisect2 <- function(a,b,equal,K,Linf,C,TW,ts){
  if(((cgrowth_rootf2(a,K,Linf,C,TW,ts)-equal)^2)<10^(-10)){return(a)}
  if(((cgrowth_rootf2(b,K,Linf,C,TW,ts)-equal)^2)<10^(-10)){return(b)}
  #This function uses bisection to compute the required end time values
  if((growth_rootf2(a,K,Linf,C,TW,ts)-equal)*(growth_rootf2(b,K,Linf,C,TW,ts)-equal)>0){#make sure that inputs are okay... 
    print("f(xup) and f(xlow) are of same sign 2")
    print(paste("fxup::",(cgrowth_rootf2(a,K,Linf,C,TW,ts)-equal),"fxlow::",(cgrowth_rootf2(b,K,Linf,C,TW,ts)-equal)))
    print(paste("a::",a,"b::",b))
    return(1)} 
termtest <- 1#set counter to protect against errors. 
  while(termtest<= 10000) {# limit iterations to prevent infinite loop
    d <- (a + b)/2 #new midpoint
    if(((cgrowth_rootf2(d,K,Linf,C,TW,ts)-equal)==0||(b-a)/2<= 10^-(10))) { #solution found
    return(d)
    break
  }
  termtest <- termtest + 1 #increment step counter
  if(sign(cgrowth_rootf2(d,K,Linf,C,TW,ts)-equal) == sign(cgrowth_rootf2(a,K,Linf,C,TW,ts)-equal)){ a <- d}
  else{b <- d }# new interval
}
print("Method failed. max number of steps exceeded")#just a nice test to make sure that the first test really worked.
}
cbisect2 <- cmpfun(bisect2)

  #first  compute time_start
  timestart <-  bisect(-200*365,200*365,sML,K,Linf,C,TW)
  #print("time start")
  #print(timestart)
  #second compute time of  95%*Linf
  nintyfivetime <-  bisect2(-200*365,200*365,.95*Linf,K,Linf,C,TW,timestart)
#  print("time when length is .95Linf")
#  print(nintyfivetime)
  #compute tzero
  #really only important when C!=0 because it should be about -timestart  but ...
  zerotime <- bisect2(-200*365,200*365,0,K,Linf,C,TW,timestart)
  #print("time when length is zero")
  #print(zerotime)
  #get vector of times!
upwind <- (ceiling(nintyfivetime))
downwind <- (floor(zerotime))
time <- downwind:upwind
 #third compute growth curve and put it in the right place.
  cur <- matrix(0,(nrow=upwind+(-1)*downwind+1),ncol=4)        #initalize growth curve data structure
  period <- (C*K)/(2*pi*w)*(sin(2*pi*w*(time-TW-.5))-sin(2*pi*w*(timestart-TW+.5)))
  cur[,1] <-(time+sdate)#keep real time
  cur[,2] <-(time+sdate)%%modday #wrap time so mapping the time to the plot is easy
  cur[,3] <- Linf*(1-exp(-K*((time-timestart))+period))#put in the growth curve
  fn <- function(i){ML[which.min(((ML)-cur[i,3])^2)]} #snazzy function that allows use of sapply  to find the right bins!
  cur[,4] <- sapply(1:length(cur[,3]),fn)             #get a version of the growth curve that makes computing esp and asp easy
#  x11()#This plot will probably not be in the final version however debugging plots make me feel warm and fuzzy!
 # plot(time,cur[,3],type="l",xlab="time", ylab="Length",main="Debug growth curve plot")
 # points(0,sML)
#print(head(cur))
return(list(c=cur))
}

ccurves<- cmpfun(curves)


aspcompute <- function(peaks){
  asp <- sum(peaks$asp)
      } #compute sum of asp. 
caspcompute <- cmpfun(aspcompute)
espcompute <- function(gcurve,p=peaks$out,modday,ML)
  {                                       #compute ESP
    peaks2 <- p #need a structure to turn to zero to prevent counting a peak more than once.
    timesweep <- 1:length(gcurve$c[,1])
    esp <- timesweep*0
    for(timesweeper in timesweep){
      gclocation <- ifelse(gcurve$c[timesweeper,3]>=min(ML),which(ML==gcurve$c[timesweeper,4]),0)#figure out what ML index we are on!
      if(gclocation>0){
        tsweep <- gcurve$c[timesweeper,2]+1
        peaks2val <- peaks2[gclocation,tsweep]
        if(peaks2val>0){
          ## print("pos peaks")
          ## print(gclocation)
          ## print(peaks2val)
          ## print(gcurve$c[timesweep,])
          esp[timesweeper] <- peaks2val
          peaks2[gclocation,tsweep] <- 0
          for(i in 1:200){
            pos <- ifelse(gclocation+i<which.max(ML),gclocation+i,which.max(ML))
            neg <- ifelse(gclocation-i>which.min(ML),gclocation-i,which.min(ML))
            if(peaks2[pos,tsweep]>0) {peaks2[pos,tsweep] <- 0}
            if(peaks2[neg,tsweep]>0) {peaks2[neg,tsweep] <- 0}
            if(peaks2[neg,tsweep]<0 && peaks2[pos,tsweep]<0 ) {break}
          }
        }else{
          if(peaks2val<0){
            ## print("neg peak")
            ## print(gclocation)
            ## print(peaks2val)
            ## print(gcurve$c[timesweep,])
             }
          esp[timesweeper] <- peaks2val}
      }else{esp[timesweeper] <- 0}
    }
    #print(esp[which(esp!=0)])
    ESP <- sum(esp)
    #print(sum(esp))
    #print(ESP)
   return(list(esp=ESP))#,peaks2=peaks2))
}
cespcompute <- cmpfun(espcompute)
gfcompute <- function(asp,esp){
  #print(esp$esp)
  #print(asp)
gf <- 10^(esp$esp/asp)/10}
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




kscan <- function(Linf,c,tw,dat=data,d=days){
  
  growthdata <- matrix(0,ncol=d,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
  lfdata<- fillgrowthdata(date,dat,growthdata) #make data structure with length frequency data
  peaks <- lfrestruc(lfdata)                    #create restructure lfdata into peaks and valleys.
  asp <- aspcompute(peaks)                      #compute asp
  print("asp")
  print(asp)
  K <- seq(.1,10,length.out=100)
  out <- matrix(0,nrow=length(K),ncol=4)
  c <- 1.18
  tw <- 0.18
  Linf <- 1.5

  
 for(i in 1:length(K)){
        index <- 1
        inside <- matrix(0,nrow=length(dat$ML)*d,ncol=3)#
   for(j in 1:(length(dat$ML))){
      for(sdate in 1:d){
        if(peaks$out[j,sdate]>0 & lfdata[j,sdate]>0){
        gcurve <- curves(Linf,c,tw,K[i],dat$ML,days,peaks,sdate,dat$ML[j])      # compute growth curve this has index, day in growthcurve and properbin.
        esp <- espcompute(gcurve,peaks$out,days,dat$ML)               #compute esp
        gf <- gfcompute(asp,esp)
      } else{gf <- 0}
        if(gf>0){
      }
        inside[index,] <- c(gf,dat$ML[j],sdate)
        index <- index+1
    }
  }
    out[i,] <- c(max(inside[,1]),K[i],inside[which.max(inside[,1]),2],inside[which.max(inside[,1]),3])
    print(i/length(K))  
  }

  return(out)
}                                      


  


##   for(i in 1:length(K)){
##         index <- 1
##         inside <- matrix(0,nrow=length(dat$ML)*d,ncol=3)#
##     for(j in 1:(length(dat$ML))){
##       for(sdate in 1:d){
##         if(peaks$out[j,sdate]>0 & lfdata[j,sdate]>0){
##         gcurve <- curves(Linf,c,tw,K[i],dat$ML,days,peaks,sdate,dat$ML[j])      # compute growth curve this has index, day in growthcurve and properbin.
##         esp <- espcompute(gcurve,peaks$out,days,dat$ML)               #compute esp
##         gf <- gfcompute(asp,esp)
##       } else{gf <- 0}
##         if(gf>0){
##       }
##         inside[index,] <- c(gf,dat$ML[j],sdate)
##         index <- index+1
##     }
##   }
##     out[i,] <- c(max(inside[,1]),K[i],inside[which.max(inside[,1]),2],inside[which.max(inside[,1]),3])
##     print(i/length(K))  
##   }
##  out <- c(1,10)
##   return(out)
## }                                      
ckscan <- cmpfun(kscan)



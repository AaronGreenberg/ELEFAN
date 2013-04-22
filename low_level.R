
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

curves <- function(Linf,Cseasonal,tw,K,ML,modday,lfdata,sdate,sML,birthdaycurve=BIRTHDAY){
  K <- K/365
  w <- 1/365
  TW <- tw*365
growth_rootf <- function(x,K,Linf,Cseasonal,TW,age){
#makes computing tstart and time when length is .95%Linf easy.
  w <- 1/365
  
  period <- (Cseasonal*K)/(2*pi*w)*(sin(2*pi*w*(age-TW-.5*365))-sin(2*pi*w*(x-TW-.5*365)))
  out <- Linf*(1-exp(-K*(age-x)+period))
  return(out)
}
cgrowth_rootf <- cmpfun(growth_rootf)
  
bisect <- function(a,b,equal,K,Linf,Cseasonal,TW,age){
  if((cgrowth_rootf(a,K,Linf,Cseasonal,TW,age)-equal)^2<10^(-10)){return(a)}
  if((cgrowth_rootf(b,K,Linf,Cseasonal,TW,age)-equal)^2<10^(-10)){return(b)}#make sure zero isn't endpoints.
  #This function uses bisection to compute the required values of tstart and...
  if((growth_rootf(a,K,Linf,Cseasonal,TW,age)-equal)*(growth_rootf(b,K,Linf,Cseasonal,TW,age)-equal)>0){#make sure that inputs are okay... 
    print("f(xup) and f(xlow) are of same sign 1")
    print(paste("fxup::",(cgrowth_rootf(a,K,Linf,Cseasonal,TW,age)-equal),"fxlow::",(cgrowth_rootf(b,K,Linf,Cseasonal,TW,age)-equal)))
    return(1)} 
termtest <- 1#set counter to protect against errors. 
  while(termtest<= 10000) {# limit iterations to prevent infinite loop
    d <- (a + b)/2 #new midpoint
    if(((cgrowth_rootf(d,K,Linf,Cseasonal,TW,age)-equal)==0||(b-a)/2<= 10^-(10))) { #solution found
    return(d)
    break
  }
  termtest <- termtest + 1 #increment step counter
  if(sign(cgrowth_rootf(d,K,Linf,Cseasonal,TW,age)-equal) == sign(cgrowth_rootf(a,K,Linf,Cseasonal,TW,age)-equal)){ a <- d}
  else{b <- d }# new interval
}
print("Method failed. max number of steps exceeded")#just a nice test to make sure that the first test really worked.
}

cbisect <- cmpfun(bisect)
  #I should talk to Laura about making things polymorphic... Cause this is ugly and has code redundancy. 
   
growth_rootf2 <- function(x,K,Linf,Cseasonal,TW,ts,age){
#makes computing to and time when length is .95%Linf easy. 
  w <- 1/365
  period <- (Cseasonal*K)/(2*pi*w)*(sin(2*pi*w*(x-(TW-age)-.5*365))-sin(2*pi*w*(ts-TW-.5*365)))
  out <- Linf*(1-exp(-K*(x-(ts-age))+period))
  return(out)
}
cgrowth_rootf2 <- cmpfun(growth_rootf2)
  
bisect2 <- function(a,b,equal,K,Linf,Cseasonal,TW,ts,age){
  if(((cgrowth_rootf2(a,K,Linf,Cseasonal,TW,ts,age)-equal)^2)<10^(-10)){return(a)}
  if(((cgrowth_rootf2(b,K,Linf,Cseasonal,TW,ts,age)-equal)^2)<10^(-10)){return(b)}
  #This function uses bisection to compute the required end time values
  if((growth_rootf2(a,K,Linf,Cseasonal,TW,ts,age)-equal)*(growth_rootf2(b,K,Linf,Cseasonal,TW,ts,age)-equal)>0){#make sure that inputs are okay... 
    print("f(xup) and f(xlow) are of same sign 2")
    print(paste("fxup::",(cgrowth_rootf2(a,K,Linf,Cseasonal,TW,ts,age)-equal),"fxlow::",(cgrowth_rootf2(b,K,Linf,Cseasonal,TW,ts,age)-equal)))
    print(paste("a::",a,"b::",b))
    return(1)}
  
termtest <- 1#set counter to protect against errors. 
  while(termtest<= 10000) {# limit iterations to prevent infinite loop
    d <- (a + b)/2 #new midpoint
    if(((cgrowth_rootf2(d,K,Linf,Cseasonal,TW,ts,age)-equal)==0||(b-a)/2<= 10^-(10))) { #solution found
    return(d)
    break
  }
  termtest <- termtest + 1 #increment step counter
  if(sign(cgrowth_rootf2(d,K,Linf,Cseasonal,TW,ts,age)-equal) == sign(cgrowth_rootf2(a,K,Linf,Cseasonal,TW,ts,age)-equal)){ a <- d}
  else{b <- d }# new interval
}
print("Method failed. max number of steps exceeded")#just a nice test to make sure that the first test really worked.
}
  
cbisect2 <- cmpfun(bisect2)
  age= sdate-birthdaycurve #need to compute age.
  #first  compute time_start
  timestart <-  bisect(-200*365,200*365,sML,K,Linf,Cseasonal,TW,age)
  #second compute time of  95%*Linf
  nintyfivetime <-  bisect2(-200*365,200*365,.95*Linf,K,Linf,Cseasonal,TW,timestart,age)
  #compute tzero
  #really only important when Cseasonal!=0 because it should be about -timestart  but ...
  zerotime <- bisect2(-200*365,200*365,0,K,Linf,Cseasonal,TW,timestart,age)
  #get vector of times!
upwind <- (ceiling(nintyfivetime))
downwind <- (floor(zerotime))
time <- downwind:upwind
 #third compute growth curve and put it in the right place.
  cur <- matrix(0,(nrow=upwind+(-1)*downwind+1),ncol=4)        #initalize growth curve data structure
  period <- (Cseasonal*K)/(2*pi*w)*(sin(2*pi*w*(time-(TW-age)-.5*365))-sin(2*pi*w*(timestart-TW-.5*365)))
  cur[,1] <-(time+sdate)#keep real time
  cur[,2] <-(time+sdate)%%modday #wrap time so mapping the time to the plot is easy
  cur[,3] <- Linf*(1-exp(-K*((time-(-age+timestart)))+period))#put in the growth curve
  fn <- function(i){ML[which.min(((ML)-cur[i,3])^2)]} #snazzy function that allows use of sapply  to find the right bins!
  cur[,4] <- sapply(1:length(cur[,3]),fn)             #get a version of the growth curve that makes computing esp and asp easy
return(list(c=cur,tzero=downwind))
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
          for(i in 1:200){ #sweep up check peaks
            pos <- ifelse(gclocation+i<which.max(ML),gclocation+i,which.max(ML))
            if(peaks2[pos,tsweep]>0) {peaks2[pos,tsweep] <- 0}
            if(peaks2[pos,tsweep]<0 ) {break}
          }
            for(i in 1:200){#sweep down check peaks
            neg <- ifelse(gclocation-i>which.min(ML),gclocation-i,which.min(ML))
            if(peaks2[neg,tsweep]>0) {peaks2[neg,tsweep] <- 0}
            if(peaks2[neg,tsweep]<0) {break}
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
  z <- rowSums(data2)#sum up all the frequencies
  Li=Liprime=z*0
  for(i in 1:length(data2$ML)){
    Li[i]=data$ML[i]-(data$ML[2]-data$ML[1])/2    #get list of cut off values
    Liprime[i]=sum(data$ML[i:length(data$ML)]*z[i:length(data$ML)])/sum(z[i:length(data$ML)])  #get list scaled mean lengths
  }
#  points=8
  Lipoints=Liprime[(length(Liprime)-points):length(Liprime)]
  Lip=Li[(length(Liprime)-points):length(Liprime)]
  
  par(las=1,bty='n')
  z=lm(Lipoints~Lip)
  print(z)
  print(z$coefficients)
  Linfest <- -z$coefficients[1]/(z$coefficients[2]-1)#compute Linf 
  plot(Li,Liprime,xlim=c(0,max(Li,Linfest)*1.22),ylim=c(0,max(Liprime,Linfest)*1.22),xlab="Cutoff Length L'",ylab=expression(paste("Mean Length above cutoff  ", bar(L) )))
  points(Lip,Lipoints,col="black",pch=19)
  points(Linfest,z$coefficients[1]+z$coefficients[2]*Linfest,col="black",pch=19,cex=.5)
  abline(a=0,b=1,col="grey")
  lines(c(Lip,Linfest),z$coefficients[1]+z$coefficients[2]*c(Lip,Linfest),col="black")
  abline(v=Linfest,col="grey")
  abline(h=0,col="grey")
  temp=signif(Linfest,3)
  l1 <-expression(paste("L",infinity))
  text(Li,Liprime+.4*log(max(Liprime)),as.character(sort(0:(length(Li)-1),decreasing=TRUE)))
  text(Linfest*1.0,min(1,.01*(z$coefficients[1]+z$coefficients[2]*Linfest)),l1)
  temp2 <-bquote(paste("L",infinity==.(temp),"  ",
                       bar(L), "=",.(signif(z$coefficients[1],3)),"+",.(signif(z$coefficients[2],3)),"L'",
                       "  ",R^2,"=",.(signif(summary(z)$r.squared,2))))
  print(temp2)
  
  legend(x="topleft",legend=temp2)
  return(Linfest)
}




kscan <- function(Linf=Linf,c=c,tw=tw){
  print(paste("Linf","c","tw"))
  temp <- c(Linf,c,tw)
  print(temp)
  d=days
  dat=data
  growthdata <- matrix(0,ncol=d,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
  lfdata<- fillgrowthdata(date,dat,growthdata) #make data structure with length frequency data
  peaks <- lfrestruc(lfdata)                    #create restructure lfdata into peaks and valleys.
  asp <- aspcompute(peaks)                      #compute asp
  print("asp")
  print(asp)
  K <- exp(seq(log(.1),log(10),length.out=50))
  zkscan <- matrix(0,nrow=length(K),ncol=4)

  pb <- tkProgressBar(title = "progress bar", min = 0, max = length(K))

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
    zkscan[i,] <- c(max(inside[,1]),K[i],inside[which.max(inside[,1]),2],inside[which.max(inside[,1]),3])
    setTkProgressBar(pb, i, label=paste( round(i/length(K)*100, 0),"% done"))
    
  }
  zkscan<<-zkscan
  return(zkscan)
}                                      

ckscan <- cmpfun(kscan)




fixedkscan <- function(sdate=sdate,ML=ML,Linf=Linf,C=C,tw=tw){
  d=days
  dat=data
  

  growthdata <- matrix(0,ncol=d,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
  lfdata<- fillgrowthdata(date,dat,growthdata) #make data structure with length frequency data
  peaks <- lfrestruc(lfdata)                    #create restructure lfdata into peaks and valleys.
  fn <- function(dayl,peaks){sum(lfdata[,dayl])}
  temp <- sapply(1:d,fn,lfdata)
  print(temp)
  temp2 <- which(temp!=0)
  print(temp2)
  print(sdate)
  sdate <- temp2[sdate]
  print("fixed sdate?")
  print(sdate)
  asp <- aspcompute(peaks)                      #compute asp
  print("asp")
  print(asp)
  K <- exp(seq(log(.1),log(10),length.out=200))
  pb <- tkProgressBar(title = "progress bar", min = 0,max = length(K))
  fixzkscan <- matrix(0,nrow=length(K),ncol=2)
 for(i in 1:length(K)){
        gcurve <- curves(Linf,C,tw,K[i],dat$ML,d,peaks,sdate,ML)      # compute growth curve this has index, day in growthcurve and properbin.
        esp <- espcompute(gcurve,peaks$out,days,dat$ML)               #compute esp
        gf <- gfcompute(asp,esp)
        print(i/length(K))
        fixzkscan[i,] <- c(gf,K[i])
          setTkProgressBar(pb, i, label=paste( round(i/length(K)*100, 0),"% done"))
      }

  fixzkscan<<-fixzkscan
  return(fixzkscan)
}                                      

cfixedkscan <- cmpfun(fixedkscan)



## % ~~~~THIS FILE COMPUTES AVALIABLE SUMS OF PEAKS.
## %############################################################
## %############################################################
## % 

##ROUTINES 

ma5 <- function(x){                     #moving average function
  K<- length(x)                         #store length of x
  y<-numeric(K)                         #initialize y
  y[1] <- 0+0+x[1]+x[1+1]+x[1+2]        #first ma
  y[2] <- 0+x[1]+x[2]+x[2+1]+x[2+2]     #second ma
  y[K] <- x[K-2]+x[K-1]+x[K]+0+0        #other end 
  y[K-1] <- x[K-3]+x[K-2]+x[K-1]+x[K]+0 #other end
  for(i in 3:(K-2))                     #main loop
    {
      y[i]=x[i-2]+x[i-1]+x[i]+x[i+1]+x[i+2] #compute moving average for middle part
    }
  y<-y/5                                #scale things
  return(y)
}

quotientsN_ma <- function(N,ma){      #compute the freq/moving average
  #perhaps should eventually get moved into moving average function
  K <- length(N)
  y<-numeric(K)
  y <- ifelse(ma>0,N/ma,0)                             
  return(y)
}

peaksmap <- function(Q){                   #Compute Peaks
  y <- Q/mean(Q)-1                         #scale and shift
return(y)
}

isolate <- function(x,freq){            #Identify isolated peaks
  K <- length(x)                        #initalize stuff
  print(x)
  #print(freq)
  count <- numeric(K)                  
  zeros <- numeric(K)
  zeros <- which(freq==0) #get vector of places where frequencies are zero

  #End cases
  count[1]=2+count[1]                   
  count[2]=1+count[2]
  count[K]=2+count[K]
  count[K-1]=1+count[K-1]
 
  #count middle frequencies     #This is for the middle places
  if(sum(zeros)>0){ #make sure that there are interior zeros.
    print("zeros")
    print(zeros-1)
    print(zeros-2)
  if((zeros-1)>0){count[zeros-1]=1+count[zeros-1]} #Make sure that we don't have zeros in the smallest width classes
  if((zeros-2)>0){count[zeros-2]=1+count[zeros-2]}  
}
  count[zeros+2]=1+count[zeros+2]       #
  count[zeros+1]=1+count[zeros+1]
  count<-ifelse(x>=0,count,count*(-1))  #remember signs are important
  #print(count)
  return(count)
}


deemph <- function(q,nz){
  y <- ifelse(nz>0,q*.5^(nz),q)         #soften up the peaks
  return(y)
}


spv <- function(unw){           #rescale according to Routine F in Manual page 63
  y <- ifelse(unw==(-1),0,unw)
  negi <- which(unw<0)
  posi <- which(unw>0)
  psum <- sum(unw[posi])
  nsum <- sum(unw[negi])
  y <- ifelse(y<0,y*psum/(-nsum),y)
  return(y)
}


availablesumpeaks <- function(x){
  yindex <- which(x>0)
  ytemp <- ifelse(x<0,0,x)
  #print(ytemp)
  for(i in yindex){
    if(i==length(x)){
    ytemp[i] <-max(c(ytemp[i],ytemp[i-1]))
    }
    else if(i==1){
    ytemp[i] <-max(c(ytemp[i],ytemp[i+1]))
    }else{
    ytemp[i] <-max(c(ytemp[i],ytemp[i-1]+ytemp[i+1]))
    }
    ytemp[i-1] <- 0
    ytemp[i+1] <- 0
  }
  return(sum(ytemp))
}


## % ~~MAIN SECTION
## %############################################################
## %############################################################
## %

main1 <- function(data){#This function just puts things in order and returns a appropreate data structure
datatmp <- NULL
datatmp$OBS <-data
datatmp$A <- ma5(datatmp$OBS)           #first moving average
datatmp$B <- quotientsN_ma(datatmp$OBS,datatmp$A) #quotient rescale
datatmp$C <- peaksmap(datatmp$B)                     #make peaks
datatmp$D <- isolate(datatmp$C,datatmp$OBS)       #isolate peaks
datatmp$E <- deemph(datatmp$C,datatmp$D)          #deemph according to manual (SERIOUSLY see manual...)
datatmp$F <- spv(datatmp$E)                       #final rescale
return(as.vector(datatmp$F))
}

main2 <-function(peaks) {               #this computes the available sum of peaks for each data set
ASP <- availablesumpeaks(peaks)
return(ASP)
}

lfrestruc<- function(ldata){            #puts main1 and main2 together to 
   ret <- NULL                          #initalizing output data structure
   ret$out <- matrix(0,nrow=length(ldata[,1]),ncol=length(ldata[1,])) 
   ret$asp <- vector()
   count=1
   for(j in 2:(length(ldata[1,]))){          #looping over the different sets of lf data

     if(sum(ldata[,j])==0){
       ldata[,j]=ldata[,j]}
     else{
       
     ret$out[,j] <- main1(ldata[,j])     #applying main1
     ret$asp[count] <-  main2(ret$out[,j])  #applying main2
     count=count+1
     }
   }
    print(ret)
    return(ret)                         
 }


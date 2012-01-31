 ## % ~~~~THIS FUNCTION COMPUTES AVALIABLE SUMS OF PEAKS.
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
  K <- length(N)
  y<-numeric(K)
  y <- ifelse(ma>0,N/ma,0)                             
  return(y)
}

peaks <- function(Q){                   #Compute Peaks
  y <- Q/mean(Q)-1                      #scale and shift
return(y)
}

isolate <- function(x,freq){            #Identify isolated peaks
  K <- length(x)
  count <- numeric(K)                   #initalize stuff
  zeros <- numeric(K)
  zeros <- which(freq==0) #get vector of places where frequencies are zero
  #count middle frequencies
  count[zeros-1]=1+count[zeros-1]       #Count Places where Frequency is Zero
  count[zeros-2]=1+count[zeros-2]       #This is for the middle places
  count[zeros+2]=1+count[zeros+2]       #
  count[zeros+1]=1+count[zeros+1]
  #End cases
  count[1]=2+count[1]                   #the end cases are kinda different because they have zeros added to the edge...
  count[2]=1+count[2]
  count[K]=2+count[K]
  count[K-1]=1+count[K-1]
  #change signs
  count<-ifelse(x>=0,count,count*(-1))  #remember signs are important
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
  #print(ytemp)
  #print(sum(ytemp))
  return(sum(ytemp))
}


## % ~~MAIN SECTION
## %############################################################
## %############################################################
## %

main1 <- function(data){
datatmp <- NULL
#print("in main1")
#print(data)
datatmp$OBS <-data
#print(datatmp$OBS)
#print("eh??ma 5")
datatmp$A <- ma5(datatmp$OBS)
#print("eh?? quotient")
datatmp$B <- quotientsN_ma(datatmp$OBS,datatmp$A)
#print("eh?? peaks")
datatmp$C <- peaks(datatmp$B)
#print("eh?? isolate")
#print(datatmp$C)
#print(datatmp$OBS)
datatmp$D <- isolate(datatmp$C,datatmp$OBS)
#print(datatmp$D)
#print("eh?? deemph")
datatmp$E <- deemph(datatmp$C,datatmp$D)
#print("eh??,spv")
datatmp$F <- spv(datatmp$E)
#print(datatmp)
#print("leaving main")
return(as.vector(datatmp$F))
}

main2 <-function(peaks) {
ASP <- availablesumpeaks(peaks)
#print(data)
#print(ASP)
}

 main<- function(data,date){
   ret <- NULL
   ret$out <- matrix(0,nrow=length(data[,1]),ncol=length(date))
   ret$asp <- vector()
   for(j in 2:(length(date))){
     #print(j/(length(date)))
     #print("hi")
     #print(data[,j])
     #print(main1(data[,j]))
     ret$out[,j] <- main1(data[,j])
     ret$asp[j] <-  main2(ret$out[,j])
   }
     #print(ret)
    return(ret)
 }


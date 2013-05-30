#' Minimal doc
#' @description minimal documentation for roxygen purposes and could be added later 
#' @export



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
  count <- numeric(K)                  
  zeros <- numeric(K)
  zeros[which(freq==0)]<-TRUE
  zeros[which(freq!=0)]<-FALSE
  #take care of left
  countf <- function(nz,K,zeros){
    #this function takes in an index and returns the number of zeros next to it
    if(nz==1){count=2+zeros[nz+1]+zeros[nz+2]}
    if(nz==2){count=1+zeros[nz+1]+zeros[nz+2] +zeros[nz-1]}
    if(nz==K){count=2+zeros[nz-1]+zeros[nz-2]}
    if(nz==(K-1)){count=1+zeros[nz-1]+zeros[nz-2]+zeros[nz+1]}
    if(nz>2&nz<(K-1)){count=zeros[nz-1]+zeros[nz-2]+zeros[nz+1]+zeros[nz+2]}
    return(count)
    }
  #take care of right
  count<-sapply(1:K,countf,K,zeros)
  count<-ifelse(x>=0,count,count*(-1))  #remember signs are importan
  #print(count)
  return(count)
}


deemph <- function(q,nz){
  y <- ifelse(nz>0,q*.5^(nz),q)         #soften up the peaks
  lengthy <- length(y)
  y[lengthy] <- ifelse(y[lengthy]<0,0,y[lengthy])
  y[lengthy-1] <- ifelse(y[lengthy-1]<0,y[lengthy]*.5,y[lengthy])
  return(y)
}


spv <- function(unw){           #rescale according to Routine F in Manual page 63
  y <- ifelse(unw==(-1),0,unw)
  #print(y)
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
  m <- vector()
  partition <- which(ytemp==0 )# find the places where it is zero
  #print(partition)
  m1 <- max(ytemp[1:partition[1]])#get lower limit
  #print(m1)
  mend <- max(ytemp[length(partition+1):length(ytemp)])# get upper limit
  #print(mend)
  for(i in 1:(length(partition)-1)){
    #print(ytemp[partition[i]:partition[i+1]])
   m[i] <- max(ytemp[partition[i]:partition[i+1]])
  }
  ## print("m and good stuff")
  ## print(m)
  ## print(m1)
  ## print(mend)
  return((sum(m)+m1+mend))
}


## % ~~MAIN SECTION
## %############################################################
## %############################################################
## %

main1 <- function(datain){#This function just puts things in order and returns a appropreate data structure
datatmp <- NULL
datatmp$OBS <-datain
datatmp$A <- ma5(datatmp$OBS)           #first moving average
datatmp$B <- quotientsN_ma(datatmp$OBS,datatmp$A) #quotient rescale
datatmp$C <- peaksmap(datatmp$B)                     #make peaks
datatmp$D <- isolate(datatmp$C,datatmp$OBS)       #isolate peaks
datatmp$E <- deemph(datatmp$C,datatmp$D)          #deemph according to manual (SERIOUSLY see manual...)
#datatmp$F <- spv(datatmp$E/((1+2/datatmp$OBS)^.5))                       #final rescale
datatmp$F <- spv(datatmp$E)                       #final rescale
print("data restructure")
#print(datatmp$F)
print(datatmp)
return(as.vector(datatmp$F))
}

main2 <-function(peaks) {               #this computes the available sum of peaks for each data set
ASP <- availablesumpeaks(peaks)

#print("ASP")
#print(ASP)
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
     #print("lf restructure counter")
     #print(j)
     #print(count)
     ret$out[,j] <- main1(ldata[,j])     #applying main1
     ret$asp[count] <-  main2(ret$out[,j])  #applying main2
     #print("partial asp")
     #print(ret$asp)
     #print(sum(ret$asp))
     count=count+1
     }
   }
    return(ret)                         
 }




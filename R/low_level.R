#' Minimal doc
#' @description this function fills in the growth data with the data that is being read in.
#' It is important to realize that we need to have a data structure that keeps track of time.
#' This is a really sparse structure, but it allows us keep time proportional.
#' @export

fillgrowthdata <- function(datein,datain,growthdata){ 
  interval <- vector()
  for(i in 1:(length(datein$Date)-1)){    # compute intervals between dates so that dates are stored correctly. 
    interval[i]=datein$Date[i+1]-datein$Date[1]
  }
  for(i in 1:(length(datein$Date)-1)){    #assign length frequency data to big array of date length frequency data
    growthdata[,interval[i]]=datain[,i+1]
  }
  return(growthdata)# return data structure with either zeros or length frequency data by day
}





aspcompute <- function(peaks){
  asp <- sum(peaks$asp)
  print("ASP REAL")
  print(asp)
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
#        print(peaks2val)
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


wetherall <- function(da=datain,points=3){
  data2 <- datain
  data2$ML <- datain$ML*0
  z <- rowSums(data2)#sum up all the frequencies
  points <- points-1
  Li=Liprime=z*0
  for(i in 1:length(data2$ML)){
    Li[i]=datain$ML[i]-(datain$ML[2]-datain$ML[1])/2    #get list of cut off values
    Liprime[i]=sum(datain$ML[i:length(datain$ML)]*z[i:length(datain$ML)])/sum(z[i:length(datain$ML)])  #get list scaled mean lengths
  }
#  points=8
  Lipoints=Liprime[(length(Liprime)-points):length(Liprime)]
  Lip=Li[(length(Liprime)-points):length(Liprime)]
  
  par(las=1,bty='n')
  z=lm(Lipoints~Lip)
  print(z)
  print(z$coefficients)
  Linfest <- -z$coefficients[1]/(z$coefficients[2]-1)#compute Linf 
  plot(Li,Liprime,xlim=c(0,max(Li,Linfest)*1.22),ylim=c(0,max(Liprime,Linfest)*1.22),xlab="Cutoff Length L'",ylab=expression(paste("Mean length above cutoff  ", bar((L)) )))
  points(Lip,Lipoints,col="black",pch=19)
  points(Linfest,z$coefficients[1]+z$coefficients[2]*Linfest,col="black",pch=19,cex=.5)
  abline(a=0,b=1,col="grey")
  rangeofline <- c(Lip,Linfest)
  lines(rangeofline,z$coefficients[1]+z$coefficients[2]*rangeofline,col="black")
  abline(v=Linfest,col="grey")
  abline(h=0,col="grey")
  temp=signif(Linfest,3)
  l1 <-expression(paste("L",infinity))
  text(Li,Liprime+.4*log(max(Liprime)),as.character(sort(1:(length(Li)),decreasing=TRUE)))
  text(Linfest*1.0,min(1,.01*(z$coefficients[1]+z$coefficients[2]*Linfest)),l1)
  temp2 <-bquote(paste("L",infinity==.(temp),"  ",
                       bar(L), "=",.(signif(z$coefficients[1],3)),"+",.(signif(z$coefficients[2],3)),"L'",
                       "  ",R^2,"=",.(signif(summary(z)$r.squared,2))))
  print(temp2)
  
  legend(x="topleft",legend=temp2)
  return(Linfest)
}




kscan <- function(Linf=Linf,cloc=cloc,tw=tw){
  print(paste("Linf","c","tw"))
  temp <- c(Linf,cloc,tw)
  d=days
  dat=datain
  #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
  growthdata <- matrix(0,ncol=d,nrow=lfbin)
  lfdata<- fillgrowthdata(datein,dat,growthdata) #make data structure with length frequency data
  peaks <- lfrestruc(lfdata)                    #create restructure lfdata into peaks and valleys.
  asp <- aspcompute(peaks)                      #compute asp
  K <- exp(seq(log(.1),log(10),length.out=100))
  zkscan <- matrix(0,nrow=length(K),ncol=4)
 for(i in 1:length(K)){
        index <- 1
        inside <- matrix(0,nrow=length(dat$ML)*d,ncol=3)#
        
   for(j in 1:(length(dat$ML))){
      for(sdate in 1:d){
        
        if(peaks$out[j,sdate]>0 & lfdata[j,sdate]>0){
        gcurve <- curves_cpp(Linf,cloc,tw,K[i],dat$ML,days,sdate,dat$ML[j],BIRTHDAY)      # compute growth curve thishas index, day in growthcurve and properbin.

        esp <- espcompute(gcurve,peaks$out,days,dat$ML)               #compute esp
        print("ESPtest")

        gf <- gfcompute(asp,esp)
        print(c(gf,":::",K[i],i/(length(K)*length(dat$ML)*d)))
      } else{gf <- 0}
        if(gf>0){
      }
        inside[index,] <- c(gf,dat$ML[j],sdate)
        index <- index+1
    }
  }
    zkscan[i,] <- c(max(inside[,1]),K[i],inside[which.max(inside[,1]),2],inside[which.max(inside[,1]),3])
  }
  zkscan<<-zkscan
  return(zkscan)
}                                      

ckscan <- cmpfun(kscan)




fixedkscan <- function(sdate=sdate,ML=ML,Linf=Linf,C=C,tw=tw){
  d=days
  dat=datain
  growthdata <- matrix(0,ncol=d,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
  lfdata<- fillgrowthdata(datein,dat,growthdata) #make data structure with length frequency data
  peaks <- lfrestruc(lfdata)                    #create restructure lfdata into peaks and valleys.
  fn <- function(dayl,lfdata){sum(lfdata[,dayl])}
  temp <- sapply(1:d,fn,lfdata)
  
  #pritn(temp)
  temp2 <- which(temp!=0)
  #print("days with nonzero lfdata")
  #print(temp2)
  #print("sdate")
  #print(sdate)
  sdate <- temp2[sdate]
  #print("fixed sdate?")
  #print(sdate)
  asp <- aspcompute(peaks)                      #compute asp
  #print("asp")
  #print(asp)
  K <- exp(seq(log(.1),log(10),length.out=250))

  fixzkscan <- matrix(0,nrow=length(K),ncol=2)
 for(i in 1:length(K)){
 
        gcurve <- curves_cpp(Linf,C,tw,K[i],dat$ML,days,sdate,ML,BIRTHDAY)      # compute growth curve this has index, day in growthcurve and properbin.
        esp <- espcompute(gcurve,peaks$out,days,dat$ML)               #compute esp
        gf <- gfcompute(asp,esp)
        print(i/length(K))
        fixzkscan[i,] <- c(gf,K[i])
      }

  fixzkscan<<-fixzkscan
  return(fixzkscan)
}                                      

cfixedkscan <- cmpfun(fixedkscan)



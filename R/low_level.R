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
    growthdata[,interval[i]]=datain[,i+1]/sum(datain[,i+1])
  }
  return(growthdata)# return data structure with either zeros or length frequency data by day
}





aspcompute <- function(peaks){
  asp <- sum(peaks$asp)
  #print("ASP REAL")
  #print(asp)
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
#        #print(peaks2val)
        if(peaks2val>0){
          ## #print("pos peaks")
          ## #print(gclocation)
          ## #print(peaks2val)
          ## #print(gcurve$c[timesweep,])
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
            ## #print("neg peak")
            ## #print(gclocation)
            ## #print(peaks2val)
            ## #print(gcurve$c[timesweep,])
             }
          esp[timesweeper] <- peaks2val}
      }else{esp[timesweeper] <- 0}
    }
    ##print(esp[which(esp!=0)])
    ESP <- sum(esp)

   return(list(esp=ESP))#,peaks2=peaks2))
}
cespcompute <- cmpfun(espcompute)

gfcompute <- function(asp,esp){gf <- 10^(esp$esp/asp)/10}
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

  Lipoints=Liprime[(length(Liprime)-points):length(Liprime)]
  Lip=Li[(length(Liprime)-points):length(Liprime)]
  par(las=1,bty='n',oma=c(0,1,1,1))
  z=lm(Lipoints~Lip)
  Linfest <- -z$coefficients[1]/(z$coefficients[2]-1)#compute Linf 
  plot(Li,Liprime,xlim=c(0,max(Li,Linfest)*1.22),ylim=c(0,max(Liprime,Linfest)*1.22),xlab="Cutoff length (L')",ylab=expression(paste("Mean length above cutoff (",bar(L),")")),yaxt="n",xaxt="n")
  axis(2,tck=0.02,las=2)
  axis(1,tck=0.02)
  points(Lip,Lipoints,col="black",pch=19)
  points(Linfest,z$coefficients[1]+z$coefficients[2]*Linfest,col="black",pch=19,cex=.5)
  abline(a=0,b=1,col="grey")
  rangeofline <- c(Lip,Linfest)
  lines(rangeofline,z$coefficients[1]+z$coefficients[2]*rangeofline,col="black")
  abline(v=Linfest,col="grey")
  abline(h=0,col="grey")
  temp=signif(Linfest,3)
  l1 <-bquote(paste("L"[infinity]))
  text(Li,Liprime+.4*log(max(Liprime)),as.character(sort(1:(length(Li)),decreasing=TRUE)))
  text(Linfest*1.0,min(1,.01*(z$coefficients[1]+z$coefficients[2]*Linfest)),l1)
  temp2 <-bquote(paste("L"[infinity]==.(signif(temp,3))," ; ",
                       bar(L)," = ",.(signif(z$coefficients[1],3)),"+",.(signif(z$coefficients[2],3)),"*L'",
                       " ; ",r^2," = ",.(signif(summary(z)$r.squared,3))))
  legend(x="topleft",inset=0.02,legend=temp2)
  return(Linfest)
}




kscan <- function(Linf=Linf,cloc=cloc,tw=tw){
  #print(paste("Linf","c","tw"))
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
  if(Sys.info()['sysname']!="Linux"){
  pb <- winProgressBar(title = "Scanning for K", min = 0, max = length(K), width = 500)
}else{
  pb <- txtProgressBar(min = 0, max = length(K), style = 3)
}
 for(i in 1:length(K)){
        index <- 1
        inside <- matrix(0,nrow=length(dat$ML)*d,ncol=3)#
  if(Sys.info()['sysname']!="Linux"){
    setWinProgressBar(pb, i, label=paste( round(i/length(K)*100, 0),"% done"))
  }else{
    setTxtProgressBar(pb, i, label=paste( round(i/length(K)*100, 0),"% done"))
  }
        
   for(j in 1:(length(dat$ML))){
      for(sdate in 1:d){
        if(peaks$out[j,sdate]>0 & lfdata[j,sdate]>0){
        gcurve <- curves_cpp(Linf,cloc,tw,K[i],dat$ML,days,sdate,dat$ML[j],BIRTHDAY)      # compute growth curve thishas index, day in growthcurve and properbin.
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
       
  }

  zkscan<<-zkscan
  return(zkscan)
  close(pb)
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
  temp2 <- which(temp!=0)
  sdate <- temp2[sdate]
  asp <- aspcompute(peaks)                      #compute asp
  K <- exp(seq(log(.1),log(10),length.out=250))
  fixzkscan <- matrix(0,nrow=length(K),ncol=2)
  if(Sys.info()['sysname']!="Linux"){
    pb <- winProgressBar(title = "Scanning for K", min = 0, max = length(K), width = 500)
  }else{
    pb <- txtProgressBar(title = "Scanning for K", min = 0, max = length(K), width = 500)
  }
 for(i in 1:length(K)){
   if(Sys.info()['sysname']!="Linux"){
    setWinProgressBar(pb, i, label=paste(round(i/length(K)*100, 0),"% done"))
     }else{
    setTxtProgressBar(pb, i, label=paste(round(i/length(K)*100, 0),"% done"))
   }
       
        gcurve <- curves_cpp(Linf,C,tw,K[i],dat$ML,days,sdate,ML,BIRTHDAY)      # compute growth curve this has index, day in growthcurve and properbin.
        esp <- espcompute(gcurve,peaks$out,days,dat$ML)               #compute esp
        gf <- gfcompute(asp,esp)
        #print(i/length(K))
        fixzkscan[i,] <- c(gf,K[i])
      }

  fixzkscan<<-fixzkscan
  return(fixzkscan)
}                                      

cfixedkscan <- cmpfun(fixedkscan)



recruitment<- function(Kloc=K,Linfloc=Linf,Cloc=C,twloc=TW)
{
growthdata <- matrix(0,ncol=days,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
  lfdata<- fillgrowthdata(datein,datain,growthdata) #make data structure with length
recruitment <-matrix(0,ncol=2,nrow=length(datain[1,])*length(datain$ML))
count <- 0
width <- (datain$ML[2]-datain$ML[1])/2

  


  for(i in 1:days)
    {
      if(sum((lfdata[,i]))!=0){
        for(j in 1:length(datain$ML))
          { 
            count=count+1
            gcurve <- curves_cpp(Linfloc,Cloc,twloc,Kloc,datain$ML,days,i,datain$ML[j],BIRTHDAY)$tzero
            weight <-(log(1-(datain$ML[j]+width)/Linfloc)/(-1*Kloc)-gcurve)-(log(1-(datain$ML[j]-width)/Linfloc)/(-1*Kloc)-gcurve)
            
            recruitment[count,] <- c(as.numeric(format(as.Date(gcurve,origin=datein$Date[1]),"%m")),lfdata[j,i]/weight)
          }
        }
    }

recruitment <- as.data.frame(recruitment)

colnames(recruitment) <- c("month","height")
print(recruitment)
print(length(recruitment$month)/length(recruitment$height))
recruitment <- subset(recruitment, recruitment$month>0) 
recruitment2 <- aggregate(height~month, data=recruitment, FUN=sum) 

print((recruitment2))
y=(recruitment2$height-min(recruitment2$height))/(sum((recruitment2$height-min(recruitment2$height))))*100
plot(recruitment2$month,y,type="l",col="black",xlim=c(1,12),xlab="month",ylab="Percent Recruitment")
polygon( c(min(recruitment2$month), recruitment2$month, max(recruitment2$month)), c(min(y), y, min(y)), density=100,alpha=.2,col="grey" )
#points(recruitment2[,1],y,type="p",col="red",xlim=c(1,12),pch=19,cex=.5)



} 

#' Minimal doc
#' @description minimal documentation for roxygen purposes and could be added later 
#' @export

###THE FUNCTIONS THAT INTERACT WITH THE UI ARE BELOW!!!
##
plotpeak <- function(Linf,K,Cseasonal,tw,ptype,sdate,ML,scale,sdate1,ML1,hline){#function(d=days,dm=date,da=data,lfdata=lfdata,pd=peaks$out,curve=gcurve)
goodfit <- NULL
startdate <- as.Date(datein[sdate,1])
startime <- as.numeric(startdate-datein[1,1])
ML <- as.numeric(ML)



#need to convert start date to dime.
growthdata <- matrix(0,ncol=days,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
lfdata<- fillgrowthdata(datein,datain,growthdata) #make data structure with length frequency data
#head(lfdata)
peaks <- lfrestruc(lfdata)                  #create restructure lfdata into peaks and valleys.
gf <- 0
gcurve <- matrix(0,4,ncol=4)        #initalize growth curve data structure
gcurve$c <- matrix(0,4,ncol=4)

gf1 <- 0
gcurve1 <- matrix(0,4,ncol=4)        #initalize growth curve data structure
gcurve1$c <- matrix(0,4,ncol=4)


if(sdate>0){
#compute growth curves if K>0
if(K>0){  
gcurve <- curves_cpp(Linf,Cseasonal,tw,K,datain$ML,days,startime,ML,BIRTHDAY) # compute growth curve this has index, day in growthcurve and properbin.
asp <- aspcompute(peaks)                      #compute asp
#print("sdate1")
#print(sdate1)
}
if(sdate1>0){
  
startdate1 <- as.Date(datein[sdate1,1])
startime1 <- as.numeric(startdate1-datein[1,1])
ML1 <- as.numeric(ML1)
if(K>0){
gcurve1 <- curves_cpp(Linf,Cseasonal,tw,K,datain$ML,days,startime1,ML1,BIRTHDAY) # compute growth curve this has index, day in growthcurve and properbin.
esp1 <- espcompute(gcurve1,peaks$out,days,datain$ML)               #compute esp
gf1 <- gfcompute(asp,esp1)
 }
}



if(K>0){
esp <- espcompute(gcurve,peaks$out,days,datain$ML)               #compute esp
gf <- gfcompute(asp,esp)
}
}

ylabel <- paste("Length (",lengthunits,")",sep="")
if(ptype=="Peaks"){
  rqFreqPlot(1:days,datain$ML,peaks$out,startime,ML,gcurve,datein,barscale=scale,GF=signif(gf,4),birthday=BIRTHDAY,hline=hline,sdate1=startime1,sML1=ML1,curves1=gcurve1,GF1=gf1,ylab1=ylabel)
}
if(ptype=="LF"){
  rqFreqPlot(1:days,datain$ML,lfdata,startime,ML,gcurve,datein,barscale=scale,GF=signif(gf,4),birthday=BIRTHDAY,hline=hline,sdate1=startime1,sML1=ML1,curves1=gcurve1,GF1=gf1,ylab1=ylabel)
}

}

lfmanipplot <- function(hline)
  {
    
#need to convert start date to dime.
growthdata <- matrix(0,ncol=days,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
lfdata<- fillgrowthdata(datein,datain,growthdata) #make data structure with length frequency data
#print("hline")
#print(datain$ML)
#get length of lfdata2
datain2 <- matrix(0,nrow=length(hline),ncol=length(datain[1,]))
datain2 <- data.frame(datain2)
colnames(datain2)=colnames(datain)


binwidth<-datain$ML[2]-datain$ML[1]
newbinwidth <- hline[2]-hline[1]#get new bins
datain2 <- matrix(0,nrow=length(hline-1),ncol=length(datain[1,]))
datain2<- data.frame(datain2)
colnames(datain2) <- colnames(datain)
datain2$ML <- hline+1/2*newbinwidth
weights <- function(hline1,hline2,ML,binwidth)
  {# This function computes the weights that get multiplied
    WA <- vector()
    hbw <- 1/2*binwidth
    for(i in 1:length(ML))
        {
          if(((ML[i]+hbw)>hline1)||((ML[i]-hbw)<hline2))
            {#if outside reslice interval set weight =0
          WA[i]=0
            }
           if(((ML[i]-hbw)<hline1) && ((ML[i]+hbw)>hline2))
            { #if inside of reslice interval set weight=1
          WA[i]=1
            }

           if(((ML[i]+hbw)>hline1) && ((ML[i]-hbw)<=hline1))
            { #get upper weight 
          WA[i]=(hline1-(ML[i]-hbw))/binwidth
        }
          
           if(((ML[i]+hbw)>hline2) && ((ML[i]-hbw)<=hline2))
            { #get lower weight 
          WA[i]=1-((hline2-(ML[i]-hbw))/binwidth)
        }
          
        if((((ML[i]+hbw)>hline2) && ((ML[i]-hbw)<=hline2))&& (((ML[i]+hbw)>hline1) && ((ML[i]-hbw)<=hline1)))
            { #get lower weight 
          WA[i]=(hline1-hline2)/binwidth
        }
          
  }
    return(WA)
  }
    
  for(i in 1:(length(datain[1,])-1)){
     for(j in 1:(length(hline)-1)){ #for each growth curve
       Wei <- weights(hline[j+1],hline[j],datain$ML,binwidth)
       #print(Wei)
      datain2[j,i+1] <- sum(Wei*datain[,i+1])
     }
   }







#print(datain2)
#print(colSums(datain2))
growthdata2 <- matrix(0,ncol=days,nrow=length((hline))) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
lfdata2<- fillgrowthdata(datein,datain2,growthdata2) #make data structure with length frequency data
## x11()
## manipFreqPlot(1:days,datain$ML,datain2$ML,lfdata,lfdata2,hline,datein)

return(datain2)
    
  
}

kscanplot <- function(window=window,z=zkscan){
    nzero <- which(z[,1]>0)
    #par(las=1,bty="n",xaxs="i",yaxs="i")
    par(las=1,bty="n",mar=c(5.1,6,4.1,2.1),mpg=c(4,1,0),oma=c(0,1,1,1))
    ma <- movingAverage(z[nzero,1],window)
    plot(z[nzero,2],z[nzero,1],type="l",xlab=bquote(paste("K-parameter (year"^"-1",")")),ylab=bquote(paste("Goodness of fit (R"[n],")")),xaxt="n",yaxt="n",log="x",ylim=c(0,max(z[,1])*1.1),col="grey")
    axis(1,pos=0,tck=0.02)
    axis(2,tck=0.02,las=2)
    points(z[nzero,2],z[nzero,1],col="grey",cex=.9,pch=19)
    lines(z[nzero,2],ma,col="black",cex=.8)
    correctedx= z[max(which(z[,1]==max(z[,1]))),2]#get maximum of plateau... 
    points(correctedx,z[which.max(z[,1]),1],col="red",cex=.8,pch=19)
    #print("correctedx")
    #print(correctedx)
    correctedx=which.max(z[,1])
    #text(correctedx,z[which.max(z[,1]),1]+0.01,as.character(signif(z[which.max(z[,1]),2],2)))
    #text(z[which.max(z[,1]),2],z[which.max(z[,1]),1]+0.01,as.character(signif(z[which.max(z[,1]),2],2)))
    points(z[which.max(ma),2],ma[which.max(ma)],col="blue",cex=.8,pch=19)     
    text(z[which.max(ma),2],ma[which.max(ma)]+0.01,as.character(signif(z[which.max(ma),2],2)))     
    #print(max(z[nzero,1]))#gf
    ss=which(datein[,1]==as.Date(z[correctedx,4],origin=datein$Date[1]))-1 #get correct start sample
    #make legend
    legend("topright",legend=bquote(paste("K=",.(signif(z[correctedx,2],4)),"    ML=",.(z[correctedx,3]),"  Sample=",.(ss))))#,bty="n")

  }



fixedkscanplot <- function(window=window,z=fixzkscan){
    nzero <- which(z[,1]>0)
    #par(las=1,bty="n",xaxs="i",yaxs="i")
    par(las=1,bty="n",mar=c(5.1,6,4.1,2.1),mpg=c(4,1,0),oma=c(0,1,1,1))
    ma <- movingAverage(z[nzero,1],window)
    plot(z[nzero,2],z[nzero,1],type="l",xlab=bquote(paste("K-parameter (year"^"-1",")")),ylab=bquote(paste("Goodness of fit (R"[n],")")),xaxt="n",yaxt="n",log="x",ylim=c(0,max(z[,1])*1.1),col="grey",pch=.8)
    axis(1,pos=0,tck=0.02)
    axis(2,tck=0.02,las=2)
    points(z[nzero,2],z[nzero,1],col="grey",cex=.9,pch=19)
    lines(z[nzero,2],ma,col="black",cex=.6)
    correctedx= z[max(which(z[,1]==max(z[,1]))),2]#get maximum of plateau... 
    points(correctedx,z[which.max(z[,1]),1],col="red",cex=.8,pch=19)
    text(correctedx,z[which.max(z[,1]),1]+0.01,as.character(signif(z[which.max(z[,1]),2],2)))
    points(z[which.max(ma),2],ma[which.max(ma)],col="blue",cex=.8,pch=19)     
    text(z[which.max(ma),2],ma[which.max(ma)]+0.01,as.character(signif(z[which.max(ma),2],2)))     


  }

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





probsplot <- function(YieldProbs,length){
Pi=YieldProbs
 #compute p50
vec=approx(Pi,n=10000)
P50y=vec$y[which.min((vec$y-.5)^2)]
P50x=vec$x[which.min((vec$y-.5)^2)]
vec2=approx(length,n=10000)
Length=vec2$y[which.min((vec2$x-P50x)^2)]
par(las=1,bty="n",mar=c(5.1,6,4.1,2.1),mpg=c(4,1,0),oma=c(0,1,1,1))
#par(las=1,bty="n",mar=c(3.5,3.5,0.5,0.5),mpg=c(4,1,0),oma=c(1,1,1,1))
label <- paste("Length","(",lengthunits,")")
plot(length,Pi,type="p",xlab=label,ylab="Probability",xlim=c(0,1.1*max(length)),ylim=c(0,1.2))
text(length,Pi+.025,as.character(1:length(Pi)),col="black")
points(vec2$y,vec$y,col="black",type="l")
points(Length,P50y,col="red",pch=19)       
abline(h=.5,col="darkgrey")
abline(v=Length,col="darkgrey")       
}

#' Minimal doc
#' @description minimal documentation for roxygen purposes and could be added later 
#' @export

###THE FUNCTIONS THAT INTERACT WITH THE UI ARE BELOW!!!
##
plotpeak <- function(Linf,K,Cseasonal,tw,ptype,sdate,ML,scale){#function(d=days,dm=date,da=data,lfdata=lfdata,pd=peaks$out,curve=gcurve)
goodfit <- NULL
#getWinVal(scope="L")

startdate <- as.Date(datein[sdate,1])
print("start date")
print(startdate)
print(c(Linf,Cseasonal,K))
startime <- as.numeric(startdate-datein[1,1])
ML <- as.numeric(ML)
#need to convert start date to dime.
growthdata <- matrix(0,ncol=days,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
lfdata<- fillgrowthdata(datein,datain,growthdata) #make data structure with length frequency data
#head(lfdata)
peaks <- lfrestruc(lfdata)                  #create restructure lfdata into peaks and valleys.
if(K!=0){

gcurve <- curves_cpp(Linf,Cseasonal,tw,K,data$ML,days,startime,ML,BIRTHDAY) # compute growth curve this has index, day in growthcurve and properbin.
print(head(gcurve$c))
print(length(gcurve$c[,1]))


asp <- aspcompute(peaks)                      #compute asp
esp <- espcompute(gcurve,peaks$out,days,datain$ML)               #compute esp
gf <- gfcompute(asp,esp)
print("goodfit")
print(gf)
}else{
gf <- 0
gcurve <- matrix(0,4,ncol=4)        #initalize growth curve data structure
gcurve$c <- matrix(0,4,ncol=4)
}
print(datein)
if(ptype=="Peaks"){
#  png("lfplot1.png",width=1000,height=1000)
  rqFreqPlot(1:days,datain$ML,peaks$out,startime,ML,gcurve,datein,barscale=scale,GF=signif(gf,4),birthday=BIRTHDAY)
#  dev.off()
}
if(ptype=="LF"){
#  png("lfplot2.png",width=1000,height=1000)
  rqFreqPlot(1:days,datain$ML,lfdata,startime,ML,gcurve,datein,barscale=scale,GF=signif(gf,4),birthday=BIRTHDAY)
#  dev.off()
}

}




kscanplot <- function(window=window,z=zkscan){
    nzero <- which(z[,1]>0)
    par(las=1,bty="n",xaxs="i",yaxs="i")
    ma <- movingAverage(z[nzero,1],window)
    plot(z[nzero,2],z[nzero,1],type="l",xlab="K",ylab="Goodness of Fit",log="x",ylim=c(0,max(z[,1])*1.1),col="grey")
    points(z[nzero,2],z[nzero,1],col="grey",cex=.9,pch=19)
    lines(z[nzero,2],ma,col="black",cex=.8)
    points(z[which.max(z[,1]),2],z[which.max(z[,1]),1],col="red",cex=.8,pch=19)
    text(z[which.max(z[,1]),2],z[which.max(z[,1]),1]+0.01,as.character(signif(z[which.max(z[,1]),2],2)))
    points(z[which.max(ma),2],ma[which.max(ma)],col="blue",cex=.8,pch=19)     
    text(z[which.max(ma),2],ma[which.max(ma)]+0.01,as.character(signif(z[which.max(ma),2],2)))     
    print(max(z[nzero,1]))#gf
    print(z[which.max(z[,1]),2]) #K
    print(z[which.max(z[,1]),3]) #ml
    print(z[which.max(z[,1]),4])#date

  }



fixedkscanplot <- function(window=window,z=fixzkscan){
    nzero <- which(z[,1]>0)
    par(las=1,bty="n",xaxs="i",yaxs="i")
    ma <- movingAverage(z[nzero,1],window)
    plot(z[nzero,2],z[nzero,1],type="l",xlab="K",ylab="Goodness of Fit",log="x",ylim=c(0,max(z[,1])*1.1),col="grey",pch=.8)
    points(z[nzero,2],z[nzero,1],col="grey",cex=.9,pch=19)
    lines(z[nzero,2],ma,col="black",cex=.6)
    points(z[which.max(z[,1]),2],z[which.max(z[,1]),1],col="red",cex=.8,pch=19)
    text(z[which.max(z[,1]),2],z[which.max(z[,1]),1]+0.01,as.character(signif(z[which.max(z[,1]),2],2)))
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






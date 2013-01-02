
###THE FUNCTIONS THAT INTERACT WITH THE UI ARE BELOW!!!
##
plotpeak <- function(d=days,dm=date,da=data,pd2=lfdata){#function(d=days,dm=date,da=data,pd2=lfdata,pd=peaks$out,curve=gcurve){
goodfit <- NULL
getWinVal(scope="L")
startdate <- as.Date(Date)
startime <- as.numeric(startdate-dm[1,1])
ML <- as.numeric(ML)
#need to convert start date to dime.
growthdata <- matrix(0,ncol=d,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
lfdata<- fillgrowthdata(dm,da,growthdata) #make data structure with length frequency data
peaks <- lfrestruc(pd2)                    #create restructure lfdata into peaks and valleys.
gcurve <- curves(Linf,c,tw,K,da$ML,days,pd2,startime,ML)      # compute growth curve this has index, day in growthcurve and properbin.
asp <- aspcompute(peaks)                      #compute asp
esp <- espcompute(gcurve,peaks$out,days,da$ML)               #compute esp
gf <- gfcompute(asp,esp)
#graphics.off()
print("esp")
print(esp$esp)
print("goodfit")
print(gf)

print(dm)
if(ptype=="Peaks"){
#  png("lfplot1.png",width=1000,height=1000)
  rqFreqPlot(1:d,da$ML,peaks$out,startime,ML,gcurve,dm,barscale=days*(1-exp(-days))/(5*length(dm[,2])))
#  dev.off()
}
if(ptype=="LF"){
#  png("lfplot2.png",width=1000,height=1000)
  rqFreqPlot(1:d,da$ML,pd2,startime,ML,gcurve,dm,barscale=days*(1-exp(-days))/(50*length(dm[,2])))
#  dev.off()
}

}

plotwetherall <- function(da=data){
  getWinVal(scope="L")
  Linfest<- wetherall(data,(points-1))
  setWinVal(list(Linfest=Linfest))
}


plotkscan <- function(d=days,dm=date,da=data,pd2=lfdata,pd=peaks$out,curve=gcurve)
  {
    growthdata <- matrix(0,ncol=days,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
    lfdata<- fillgrowthdata(date,data,growthdata) #make data structure with length frequency data
    peaks <- lfrestruc(lfdata)                    #create restructure lfdata into peaks and valleys.
    print(date)
    print(days)
    getWinVal(scope="L")
    print(c("Linf","K"))
    print(c(Linf,K))
    z <- ckscan(Linf,c,tw,dat=data,d=days)
#    print(z)
    nzero <- which(z[,1]>0)
    plot(z[nzero,2],movingAverage(z[nzero,1],5),type="l",xlab="K",ylab="Goodness of Fit",log="x",ylim=c(0,max(z[,1])+.2))
    points(z[nzero,2],z[nzero,1],col="blue",cex=.2,pch=19)
    points(z[which.max(z[,1]),2],z[which.max(z[,1]),1],col="red",cex=.8,pch=19)
    
    print(max(z[nzero,1]))#gf
    print(z[which.max(z[,1]),2]) #K
    print(z[which.max(z[,1]),3]) #ml
    print(z[which.max(z[,1]),4])#date
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






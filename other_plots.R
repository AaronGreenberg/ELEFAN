
###THE FUNCTIONS THAT INTERACT WITH THE GUI ARE BELOW!!!
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
print("goodfit")
print(gf)


if(ptype=="Peaks"){
#  png("lfplot1.png",width=1000,height=1000)
  rqFreqPlot(1:d,da$ML,peaks$out,startime,ML,gcurve,dm,barscale=10)
#  dev.off()
}
if(ptype=="LF"){
#  png("lfplot2.png",width=1000,height=1000)
  rqFreqPlot(1:d,da$ML,pd2,startime,ML,gcurve,dm)
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
    z <- ckscan(Linf,c,tw,dat=data,d=days)
#    print(z)
    nzero <- which(z[,1]>0)
    plot(z[nzero,2],z[nzero,1],type="l",xlab="K",ylab="Goodness of Fit",log="x")
    points(z[nzero,2],z[nzero,1],col="blue",cex=.1,pch=19)
    points(z[which.max(z[,1]),2],z[which.max(z[,1]),1],col="red",cex=.8,pch=19)
    print(max(z[nzero,1]))
    print(z[which.max(z[,1]),2])
    print(z[which.max(z[,1]),3])
    print(z[which.max(z[,1]),4])
  }



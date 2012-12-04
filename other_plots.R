
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
print(peaks$out)
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
    z <- ckscan(Linf,c,tw,dat=data,d=days,growthdata,lfdata,peaks)
    plot(z[,2],z[,1]) 
  }



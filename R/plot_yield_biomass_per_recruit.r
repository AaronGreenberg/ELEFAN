#' Minimal doc
#' @description minimal documentation for roxygen purposes and could be added later 
#' @export

plot_yield_biomass_per_recruit <- function(tab_final,Pi=YieldProbs,length)
{


par(mfrow=c(2,1), las=1,bty="n", mar=c(6,6,0.5,0.5), mgp=c(4,1,0), oma=c(0,1,1,1))
is.na(tab_final) <- is.na(tab_final)
#print("plot")
#print(Pi)
#compute p50
vec=approx(Pi,n=10000)

P50y=vec$y[which.min((vec$y-.5)^2)]
P50x=vec$x[which.min((vec$y-.5)^2)]
vec2=approx(length,n=10000)
Length=vec2$y[which.min((vec2$x-P50x)^2)]

#print(tab_final)
ylim <- c(0,max(tab_final[,1:2],na.rm=TRUE))
plot(rownames(tab_final),tab_final[,1],type="l",lwd=.5,lty=2,xaxt="n",yaxt="n",ylab="Relative yield per recruit",xlab="",col=1,ylim=ylim)
lines(rownames(tab_final),tab_final[,2],type="l",lty=1,lwd=3,col="black")
axis(1,tck=0.02)
axis(2,tck=0.02,las=2)
#legend(x="topright",paste("P50 length=",signif(Length,3), "P50 prob=",signif(P50y,3)))
ylim <- c(0,max(tab_final[,3:4],na.rm=TRUE))
plot(rownames(tab_final),tab_final[,3],type="l",lwd=.5,lty=2,xaxt="n",yaxt="n",ylab="Relative biomass per recruit",xlab="Exploitation rate (E = F/Z)",col=1,ylim=ylim)
lines(rownames(tab_final),tab_final[,4],type="l",lty=1,lwd=3,col="black")
axis(1,tck=0.02)
axis(2,tck=0.02,las=2)
}



isoplath <- function(M,K,Linf,vline,hline,nlevels,Pi=YieldProbs,pas=NULL)
  {
 if (is.null(pas)) pas <- 0.015
  vlength=100
  Ein=seq(.01,.99,length=vlength)
  Littlecv=seq(.05*Linf,0.95*Linf,length.out=vlength)/Linf
  #print(Littlecv)
 iso <- array(dim=c(vlength,vlength))
for(i in 1:vlength){
   L25=max(which(Pi<.25))#which.min((Pi-.25)^2)#find
   Pitmp=Pi
   slope=.25/(Littlecv[i]*Linf-datain$ML[L25])
   Pitmp[L25:length(Pitmp)]=slope*L25:length(Pitmp)+(.25-slope*L25)
   Pitmp=ifelse(Pitmp<1,Pitmp,1)#make sure that propbabilities are less than 1
   print("Pitmp")
   print(Pitmp)
   for(j in 1:vlength){
   iso[j,i]= YR_kef(Ein[j],Littlecv[i],M,K,Linf,Pi=Pitmp,pas=NULL)
     }
  }
 par(las=1,bty="n",mar=c(5.1,6,4.1,2.1),mpg=c(4,1,0),oma=c(0,1,1,1))
 image(Ein,Littlecv,iso,col = rainbow(100,start=0,end=2/6), axes = TRUE, xlab="Exploitation Rate",ylab="Length",xlim=c(0,1),ylim=c(0,1))
 contour(Ein,Littlecv,iso,nlevels=nlevels,add=TRUE)
 abline(h=hline,col="black",lwd=1.5)
 abline(v=vline,col="black",lwd=1.5)
 points(vline,hline,pch=19,col="blue")
 legend(x="topleft",as.character(signif(YR_kef(vline,hline,M,K,Linf,Pi=Pitmp,pas=NULL)),4),col="black")
  }

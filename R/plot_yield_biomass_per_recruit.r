#' Minimal doc
#' @description minimal documentation for roxygen purposes and could be added later 
#' @export

plot_yield_biomass_per_recruit <- function(tab_final,Pi=YieldProbs,length)
{
par(mfrow=c(2,1), mar=c(6,6,0.5,0.5), mgp=c(4,1,0), oma=c(0,1,1,1))
is.na(tab_final) <- is.na(tab_final)
print("plot")
print(Pi)
#compute p50
vec=approx(Pi,n=10000)

P50y=vec$y[which.min((vec$y-.5)^2)]
P50x=vec$x[which.min((vec$y-.5)^2)]
vec2=approx(length,n=10000)
Length=vec2$y[which.min((vec2$x-P50x)^2)]

print(tab_final)
ylim <- c(0,max(tab_final[,1:2],na.rm=TRUE))
plot(rownames(tab_final),tab_final[,1],type="l",xaxt="n",yaxt="n",ylab="Relative yield per recruit",xlab="",col=1,ylim=ylim)

lines(rownames(tab_final),tab_final[,2],type="p",lty=2,col="red")
axis(1,tck=0.02)
axis(2,tck=0.02,las=2)
legend(x="topright",paste("P50 length=",signif(Length,3), "P50 prob=",signif(P50y,3)))
ylim <- c(0,max(tab_final[,3:4],na.rm=TRUE))
plot(rownames(tab_final),tab_final[,3],type="l",xaxt="n",yaxt="n",ylab="Relative biomass per recruit",xlab="Exploitation rate (E = F/Z)",col=1,ylim=ylim)
lines(rownames(tab_final),tab_final[,4],type="p",lty=2,col="red")
axis(1,tck=0.02)
axis(2,tck=0.02,las=2)
}



isoplath <- function(M,K,Littlec,Linf,vline,hline,nlevels,Pi=YieldProbs,pas=NULL)
  {
 if (is.null(pas)) pas <- 0.015
  Ein=seq(.01,1,by=pas)
  Littlec=seq(.05*Linf,0.95*Linf,length.out=length(Ein))
 iso <- array(dim=c(length(Ein),length(Littlec)))
 for(i in 1:length(Littlec)){
   iso[,i]=sapply(Ein,YR_nkef,Littlec[i],M,K,Linf,Pi=YieldProbs,pas=NULL)
  }
par(las=1,bty="n",mar=c(5.1,6,4.1,2.1),mpg=c(4,1,0),oma=c(0,1,1,1))
 image(Ein,Littlec,iso,col = terrain.colors(10), axes = TRUE, xlab="Effort",ylab="Length")
 #image(Ein,Linfin,iso,col = grey.colors(10), axes = TRUE, xlab="Effort",ylab="Length")
 contour(Ein,Littlec,iso,nlevels=nlevels,add=TRUE)
 abline(h=hline,col="red")
 abline(v=vline,col="red")
 points(vline,hline,pch=19,col="blue")
 text(vline,hline+1,as.character(signif(YR_nkef(vline,hline,M,K,Linf,Pi=YieldProbs,pas=NULL)),4),col="red")
  }

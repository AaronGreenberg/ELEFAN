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


ylim <- c(0,max(tab_final[,1:2],na.rm=TRUE))
plot(rownames(tab_final),tab_final[,1],type="l",xaxt="n",yaxt="n",ylab="Relative yield per recruit",xlab="",col=1,ylim=ylim)
lines(rownames(tab_final),tab_final[,2],type="l",lty=2,col=1)
axis(1,tck=0.02)
axis(2,tck=0.02,las=2)
legend(x="topright",paste("P50 length=",signif(Length,3), "P50 prob=",signif(P50y,3)))
ylim <- c(0,max(tab_final[,3:4],na.rm=TRUE))
plot(rownames(tab_final),tab_final[,3],type="l",xaxt="n",yaxt="n",ylab="Relative biomass per recruit",xlab="Exploitation rate (E = F/Z)",col=1,ylim=ylim)
lines(rownames(tab_final),tab_final[,4],type="l",lty=2,col=1)
axis(1,tck=0.02)
axis(2,tck=0.02,las=2)

x11()
plot(length,Pi,type="p")
points(vec2$y,vec$y,col="black",type="l")
points(Length,P50y,col="red",pch=19)       
abline(h=.5,col="darkgrey")
abline(v=Length,col="darkgrey")       
}

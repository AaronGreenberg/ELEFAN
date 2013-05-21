#' Minimal doc
#' @description minimal documentation for roxygen purposes and could be added later 
#' @export

plot_yield_biomass_per_recruit <- function(tab_final)
{
par(mfrow=c(2,1), mar=c(4,4,0.5,0.5), oma=c(0,1,5,0))
is.na(tab_final) <- is.na(tab_final)

ylim <- c(0,max(tab_final[,1:2],na.rm=TRUE))
plot(rownames(tab_final),tab_final[,1],type="l",ylab="Y/R",xlab="E",col=1,ylim=ylim)
lines(rownames(tab_final),tab_final[,2],type="l",lty=2,col=1)


ylim <- c(0,max(tab_final[,3:4],na.rm=TRUE))
plot(rownames(tab_final),tab_final[,3],type="l",ylab="B/R",xlab="E",col=1,ylim=ylim)
lines(rownames(tab_final),tab_final[,4],type="l",lty=2,col=1)

mtext("Plots of Yield per recruit (Y/R) and Biomass per recruit (B/R)
in function of the exploitation rate (E):
Knife-edge method (full lines), not Knife-edge (dashed lines)",NORTH<-3, line=0,outer=TRUE)
}
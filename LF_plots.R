#library(lattice)
 
rectplot <- function(ser,bins,xmin,xmax,ylim,barcol){
  YL=bins[1:(length(bins))]           #lower y limits
  YU=bins[2:(length(bins)+1)]             #upper y limits
  XL=ser+xmin
  XR=ser*0+xmin
  rect(XL,YL,XR,YU,col=barcol)
}


#Make Rectangles of time series Length Frequency data
rqFreqPlot <- function(time,bins,freqs,curves, xlim = c(min(time),max(time)), ylim = c(0, max(bins)), barscale = 1, barcol = length(time),boxwex = 50,xlab1="Dates in Days" ,ylab1 = "", ylab2 = "", lty = c(2, 1, 2), ...) {
   #print(curves)
   X <- time
   #print(length(curves))
   years <- length(curves$c)/365
   #print(years)
   Y <-matrix(curves$c,nrow=years,ncol=365,byrow=TRUE)
   #print(head(Y))
   jpeg("lf.jpeg")
   par(new = FALSE)
  
   plot(0,0, type = "l", xaxs = "i", lty = lty, col = 1, lwd = 2, bty = "l", xlim = xlim, ylim = ylim, xlab=xlab1,ylab = ylab1, axes=TRUE,...)
   
   
   #plot Rectangles
	for (i in 1:(length(time))){
		par(new = TRUE)
		xmin <- time[i] 
		xmax <- xlim[2] - time[i]
		ser <- as.vector(freqs[,i])
		ser <- ser * barscale
                if(sum(ser) == 0){
                }
                else{
		rectplot(-ser,bins,xmin,xmax,ylim,barcol)
              }
	}
   
   grid(nx = NA, ny = 42, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
   for(i in 1:years){
   lines(X,Y[i,],type="l")
 }
   dev.off()
 }


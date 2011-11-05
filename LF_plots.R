#library(lattice)

rectplot <- function(ser,bins,xmin,xmax,ylim,barcol){
  YL=bins[1:(length(bins)-1)]           #lower y limits
  YU=bins[2:(length(bins))]             #upper y limits
  XL=ser+xmin
  XR=ser*0+xmin
  rect(XL,YL,XR,YU,col=barcol)
}


#Make Rectangles of time series Length Frequency data
rqFreqPlot <- function(time,bins,freqs,curves, xlim = c(min(time),max(time)), ylim = c(min(bins), max(bins)), barscale = .50, barcol = length(time),boxwex = 50, ylab1 = "", ylab2 = "", lty = c(2, 1, 2), ...) {
   #print(curves)
   X <- time
   Y <- curves
   par(new = FALSE)
   plot(0,0, type = "l", xaxs = "i", lty = lty, col = 1, lwd = 2, bty = "l", xlim = xlim, ylim = ylim, ylab = ylab1, axes=TRUE,...)

   #plot Rectangles
	for (i in 1:(length(time))){
		par(new = TRUE)
		xmin <- time[i] 
		xmax <- xlim[2] - time[i]
		ser <- as.vector(freqs[,i])
		ser <- ser/max(ser) * barscale
		rectplot(-ser,bins,xmin,xmax,ylim,barcol)
                
	}
      
   lines(X,Y,type="l")
 }


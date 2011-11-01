#library(lattice)


#Make Histograms of time series Length Frequency data
rqFreqPlot <- function(time,bins,freqs,curves, xlim = c(min(time),max(time)), ylim = c(0, length(bins)), barscale = .50, barcol = length(time),boxwex = 50, ylab1 = "", ylab2 = "", lty = c(2, 1, 2), ...) {
   print(curves)
   X <- time
   Y <- curves
   par(new = FALSE)
   matplot(0,0, type = "l", xaxs = "i", lty = lty, col = 1, lwd = 2, bty = "l", xlim = xlim, ylim = ylim, ylab = ylab1, axes=FALSE,...)
   axis(as.character(bins),side=2,at=seq(1,length(bins))-.5)
   axis(as.character(time),side=1,at=seq(1,length(time)))
   lines(X,Y,type="l")
   #plot histograms
	for (i in 1:(length(time))){
		par(new = TRUE)
		xmin <- -time[i] + xlim[1]
		xmax <- xlim[2] - time[i]
		ser <- as.vector(freqs[,i])
		ser <- ser/max(ser) * barscale
		barplot(-ser, horiz = TRUE, beside=TRUE,axes = FALSE, xlim = c(xmin, xmax), ylim = ylim, col = barcol, space = 0)
                
                
	}
   
   


   
}


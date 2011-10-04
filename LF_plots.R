#Make Histograms of time series Length Frequency data
rqFreqPlot <- function(years, freqs, xlim = c(min(years),
    max(years)), ylim = c(min(years), max(years)), barscale = 100, barcol = length(years),
    boxwex = 50, ylab1 = "", ylab2 = "", lty = c(2, 1, 2), ...) {
   #make axis
   matplot(0,0, xaxs = "i", lty = lty, col = 1, lwd = 2, bty = "l", xlim = xlim, ylim = ylim, ylab = ylab1, ...)
   #plot histograms 
	for (i in 1:length(years)){
		par(new = TRUE)
		xmin <- -years[i] + xlim[1]
		xmax <- xlim[2] - years[i]
		ser <- freqs[, i+1]
		ser <- ser/max(ser) * barscale
		barplot(-ser, horiz = TRUE, axes = FALSE, xlim = c(xmin, xmax), ylim = ylim, col = barcol, space = 0)
	}

}


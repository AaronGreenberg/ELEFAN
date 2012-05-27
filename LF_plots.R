#This file makes all the length frequency and peak trough plots.
#It consists of two functions. rectplot and rqFreqPlot.
#It was written by Aaron Greenberg for the Project ELEFAN
 
rectplot <- function(ser,bins,xmin,xmax,ylim,barcol){
  #This function puts the rectangles into the rqFreqPlot
  YL=bins[1:(length(bins))]-(bins[2]-bins[1])/2           #lower y limits
  YU=bins[2:(length(bins)+1)]-(bins[2]-bins[1])/2           #upper y limits
  XL=ser+xmin                         #nonzero x limits may be right or left!
  XR=ser*0+xmin                       #axis x limits
  rect(XL,YL,XR,YU,col=barcol)
  
}


#Make Rectangles of time series Length Frequency data
rqFreqPlot <- function(time,bins,freqs,curves, dates=dates,xlim = c(min(time),max(time)), ylim = c(0, max(bins)), barscale = 1, barcol = "red",boxwex = 50,xlab1="Dates in Days" ,ylab1 = "", ylab2 = "", lty = c(2, 1, 2), ...) {
   ## This function makes the fancy graphs that seems central to the output of ELEFAN
   ## In particular it provides a way of plotting a growth curve over length frequancy data plots over time.
   ## It also allows for the plotting of different intermediate steps. Including plotting the peaks and troughs
   ##
   ## At present the xaxis needs better marking and the Y label needs to be properly written.
   ## In particular it would be nice to put the dates samples were taken on the
   #jpeg("test.jpeg")
   X <- time                            #store time
   years <- length(curves)/365        #figure out how many years the growth curve has been computed for.
   
   Y <-matrix(curves,nrow=years,ncol=length(time),byrow=TRUE) #form matrix with growth curves wrapped around
#   Y1 <-matrix(curves1,nrow=years,ncol=length(time),byrow=TRUE) #form matrix with growth curves wrapped around
   
   #create axis for plots
   par(new = FALSE)
   plot(0,0, type = "l", xaxs = "i", lty = lty, col = 1, lwd = 2, bty = "l", xlim = xlim, ylim = ylim, xlab=xlab1,ylab = ylab1, axes=TRUE,...)
   #plot Rectangles
   count=0
   lengthtime=length(time)
	for (i in 1:lengthtime){    #figure out how to make the rectangles
		par(new = TRUE)
		xmin <- time[i]         #at time i
		xmax <- xlim[2] - time[i] #got to find two points for 
		ser <- as.vector(freqs[,i]) #putting right and left sides of rectangles
		ser <- ser * barscale       #scaleing... sometimes it is nice make things bigger or smaller
                
                if(sum(ser) == 0){          #if there is no lf data at time i don't plot anything
                }
                else{                   #if there is lf data at time i make plot
                  count=count+1
		rectplot(-ser,bins,xmin,xmax,ylim,barcol)
                text(cbind(time[i]+3,40+count%%2),label=toString(dates$Date[count+1]),cex=.6,col="red")
              }
	}
   
#   grid(nx = NA, ny = 42, col = "lightgray", lty = "dotted", #make grid lines sort of chart junk but 
     #lwd = par("lwd"), equilogs = TRUE)
   for(i in 1:years){                   #draw all the growth curves 
   lines(X,Y[i,],type="l")
  # lines(X,Y1[i,],type="l",lty=2,col="red")
 }
   
#dev.off()
 }


#This file makes all the length frequency and peak trough plots.
#It consists of two functions. rectplot and rqFreqPlot.
#It was written by Aaron Greenberg for the Project ELEFAN
 
rectplot <- function(ser,bins,xmin,xmax,ylim,barcol1,barcol2){
  #This function puts the rectangles into the rqFreqPlot
  YL=bins[1:(length(bins))]-(bins[2]-bins[1])/2           #lower y limits
  YU=bins[2:(length(bins)+1)]-(bins[2]-bins[1])/2           #upper y limits
  XL=ser+xmin                         #nonzero x limits may be right or left!
  XR=ser*0+xmin                       #axis x limits
  col1 <- which(XL-xmin<=0)#switch peaks and valley colors
  col2 <- which(XL-xmin>=0)
  rect(XL[col1],YL[col1],XR[col1],YU[col1],col=barcol1)
  rect(XL[col2],YL[col2],XR[col2],YU[col2],col=barcol2)
}


#Make Rectangles of time series Length Frequency data
rqFreqPlot <- function(time,bins,freqs, sdate,sML,curves,dates=dates,xlim = c(min(time),max(time)), ylim = c(0, max(bins)), barscale = 1, barcol1 = "red",barcol2="grey",boxwex = 50,xlab1="Month" ,ylab1 = "Length (cm)", ylab2 = "", lty = c(2, 1, 2),...) {
   ## This function makes the fancy graphs that seems central to the output of ELEFAN
   ## In particular it provides a way of plotting a growth curve over length frequancy data plots over time.
   ## It also allows for the plotting of different intermediate steps. Including plotting the peaks and troughs
   X <- time                         #store time
   
   xlim <- c(min(time),max(time))
   dateaxis <-as.Date(dates$Date[1]+X)
   #create axis for plots
   par(new = FALSE)
   x11()
   plot(0,0, type = "l" , lty = lty, col = 1, lwd = 2, bty = "l", xaxt="n", xlim = xlim, ylim = ylim, xlab=xlab1,ylab = ylab1, axes=TRUE,las=2,...)
   #plot Rectangles
   axis.Date(1, at=seq(dateaxis[1],dateaxis[length(dateaxis)],by="month"))
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
		rectplot(-ser,bins,xmin,xmax,ylim,barcol1,barcol2)
                text(cbind(time[i]+3,max(bins)+(count%%2)*1.2),label=toString(dates$Date[count+1]),cex=.75,col="black")
              }
	}
   points(sdate,sML,col="black",pch=19)
 
   breaksu <- which(curves$c[,2]==(length(time)-1))
   breaksd <- which(curves$c[,2]==1)
   for(index in 1:length(breaksd)){
     plotvec <- breaksd[index]:breaksu[index]
     lines(curves$c[plotvec,2],curves$c[plotvec,3])
   }
   
 }


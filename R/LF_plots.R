#' Minimal doc
#' @description This file makes all the length frequency and peak trough plots.
#' It consists of two functions. rectplot and rqFreqPlot.
#' It was written by Aaron Greenberg for the Project New ELEFAN
#' These are the fancy plots that are super important to ELEFAN
#' @export


rectplot <- function(ser,bins,xmin,xmax,ylim,barcol1,barcol2){
  #This function puts the rectangles into the rqFreqPlot
  YL=bins[1:(length(bins))]-(bins[2]-bins[1])/2           #lower y limits
  YU=bins[1:(length(bins))]+(bins[2]-bins[1])/2           #upper y limits
  XL=ser+xmin                         #nonzero x limits may be right or left!
  XR=ser*0+xmin                       #axis x limits
  col1 <- which(XL-xmin<=0)#switch peaks and valley colors
  col2 <- which(XL-xmin>=0)
  rect(XL[col1],YL[col1],XR[col1],YU[col1],col=barcol1)#make valley boxes
  rect(XL[col2],YL[col2],XR[col2],YU[col2],col=barcol2)#make peak boxes 
}



rqFreqPlot <- function(time,bins,freqs, sdate,sML,curves,datesloc=dates,xlim = c(min(time),max(time)), ylim = c(0, ceiling((max(bins)*1.1)+.2*(bins[2]-bins[1]))), barscale = 1, barcol1 = "black",barcol2="grey",boxwex = 50,xlab1="Month" ,ylab1 = "Length (cm)", ylab2 = "", lty = c(2, 1, 2),title=" ",GF=0,birthday=0,...) {
   ## This function makes the fancy graphs that seems central to the output of ELEFAN
   ## In particular it provides a way of plotting a growth curve over length frequancy data plots over time.
   ## It also allows for the plotting of different intermediate steps. Including plotting the peaks and troughs
#     print("LF datesloc")
#   print(datesloc)
   X <- time                         #store time
   xlim <- c(min(time+as.numeric(datesloc$Date[1])),max(time+as.numeric(datesloc$Date[1])))
   temp <- vector()
   temp2 <- vector()
   temp3 <- vector()

   #this is all about scaling the bars so they look alright... however the user still has the slider.
   for(j in 1:(length(time))){temp[j] <-sum(as.vector(freqs[,j]))}
   temp2 <- which(temp!=0)
   for(i in 1:(length(temp2)-1)){temp3[i] <- temp2[i+1]-temp2[i]}
   maxscale<- max(freqs)/min(temp3)*1.1
   dateaxis <-as.Date(datesloc$Date[1]+X)#place things right location
   #create axis for plots
   par(new = FALSE)
   plot(0,0, type = "l" , lty = lty, col = 1, lwd = 2, bty = "l", xaxt="n", xlim = xlim, ylim = ylim, xlab=xlab1,ylab = ylab1, axes=TRUE,las=2,...)
   title(main=title, col.main="red", font.main=1)
   #plot Rectangles

 #   print("MAXscale")
 #   print(maxscale)

   count=0
   lengthtime=length(time)
	for (i in 1:lengthtime){    #figure out how to make the rectangles
		par(new = TRUE)
		xmin <- (time[i]+as.numeric(datesloc$Date[1]))         #at time i
		xmax <- xlim[2] - (time[i]+as.numeric(datesloc$Date[1]))       #got to find two points for 
		ser <- as.vector(freqs[,i]) #putting right and left sides of rectangles
                
		ser <-ser/maxscale*barscale      #scaleing... sometimes it is nice make things bigger or smaller
                if(sum(ser) != 0){          #if there is lf data at time i make plot
                count=count+1
		rectplot(-ser,bins,xmin,xmax,ylim,barcol1,barcol2)#make bar plot
                #abline(h=bins,col="gray60",lty=1,cex=.001)
                text(cbind((time[i]+as.numeric(datesloc$Date[1])),max(bins)+((count%%2)*.7+1.1)*min(c((bins[2]-bins[1]),1))),label=as.character(datesloc$Date[count+1],format="%y-%m-%d"),cex=.75,col="black")#add datesloc to things
              }
	}
    #print(bins)
   if(sum(curves$c[,2])!=0){
   points(sdate+as.numeric(datesloc$Date[1]),sML,col="blue",pch=19) #These plots may need to be revisited.. however for the moment they are  good enough.
   points(curves$c[,2]+as.numeric(datesloc$Date[1]),curves$c[,3],pch=1 ,cex=.2,col="black")# make real growth curve!
#   points(curves$c[,2]+as.numeric(datesloc$Date[1]),curves$c[,4],pch=1 ,cex=.02,col="pink")# make real growth curve!
                                        #      lines(curves$c[,2],curves$c[,3],col="red")
 }
   abline(v=birthday+as.numeric(datesloc$Date[1]),col="red")
   print(c("birthday",birthday)) 
   axis.Date(1, at=seq(dateaxis[1],dateaxis[length(dateaxis)],by="months") ,format="%b")
  if(GF!=0){ legend(x="topleft",legend=paste("gf=",GF))}
 }




catchrqFreqPlot <- function(time,bins,freqs, sdate,sML,tzeros,curves1,curves2,maincurve, pointscurve,timeblue,datesloc=dates,xlim = c(min(time),max(time)), ylim = c(0, ceiling((max(bins)*1.1)+.2*(bins[2]-bins[1]))), barscale = 1, barcol1 = "black",barcol2="grey",boxwex = 50,xlab1="Month" ,ylab1 = "Length (cm)", ylab2 = "", lty = c(2, 1, 2),title=" ",GF=0,birthday=BIRTHDAY,...) {
   ## This function makes the fancy graphs that seems central to the output of ELEFAN
   ## In particular it provides a way of plotting a growth curve over length frequancy data plots over time.
   ## It also allows for the plotting of different intermediate steps. Including plotting the peaks and troughs
  
   X <- time                         #store time
   xlim <- c(min(time+as.numeric(datesloc$Date[1])),max(time+as.numeric(datesloc$Date[1])))
   temp <- vector()
   temp2 <- vector()
   temp3 <- vector()

   #this is all about scaling the bars so they look alright... however the user still has the slider.
   for(j in 1:(length(time))){temp[j] <-sum(as.vector(freqs[,j]))}
   temp2 <- which(temp!=0)
   for(i in 1:(length(temp2)-1)){temp3[i] <- temp2[i+1]-temp2[i]}
   maxscale<- max(freqs)/min(temp3)*1.1
   dateaxis <-as.Date(datesloc$Date[1]+X)#place things right location
   #create axis for plots
   par(new = FALSE)
   plot(0,0, type = "l" , lty = lty, col = 1, lwd = 2, bty = "l", xaxt="n", xlim = xlim, ylim = ylim, xlab=xlab1,ylab = ylab1, axes=TRUE,las=2,...)
   title(main=title, col.main="red", font.main=1)
   #plot Rectangles

  

   count=0
   lengthtime=length(time)
	for (i in 1:lengthtime){    #figure out how to make the rectangles
		par(new = TRUE)
		xmin <- (time[i]+as.numeric(datesloc$Date[1]))         #at time i
		xmax <- xlim[2] - (time[i]+as.numeric(datesloc$Date[1]))       #got to find two points for 
		ser <- as.vector(freqs[,i]) #putting right and left sides of rectangles
                
		ser <-ser/maxscale*barscale      #scaleing... sometimes it is nice make things bigger or smaller
                if(sum(ser) != 0){          #if there is lf data at time i make plot
                count=count+1
		rectplot(-ser,bins,xmin,xmax,ylim,barcol1,barcol2)#make bar plot
                #abline(h=bins,col="gray60",lty=1,cex=.001)
                text(cbind((time[i]+as.numeric(datesloc$Date[1])),max(bins)+((count%%2)*.2+1.1)*min(c((bins[2]-bins[1]),1))),label=as.character(datesloc$Date[count+1],format="%y-%m-%d"),cex=.75,col="black")#add datesloc to things
              }
	}
    #print(bins)

      points(tzeros+as.numeric(datesloc$Date[1]),tzeros*0,pch=19,cex=1.8,col="orange")
   for(i in 1:length(sdate)){

   points(sdate[i]+as.numeric(datesloc$Date[1]),sML[i],col="magenta",pch=19) 
 }

   points(curves1$c[,1]+as.numeric(datesloc$Date[1]),curves1$c[,3],pch=1 ,cex=.2,col="red")# make real growth curve!
   points(curves2$c[,1]+as.numeric(datesloc$Date[1]),curves2$c[,3],pch=1 ,cex=.2,col="black")# make real growth curve!
      points(timeblue+as.numeric(datesloc$Date[1]),maincurve,pch=1 ,cex=.2,col="purple")# make real growth curve!

   #for(i in 1:length(bins)){
     points(pointscurve[,2]+as.numeric(datesloc$Date[1]),pointscurve[,3],col="blue",pch=1,cex=1.95)
    points(pointscurve[,2],pointscurve[,3],col="red",pch=2,cex=1.95)
     
#}
    abline(v=birthday+as.numeric(datesloc$Date[1]),col="red")

   axis.Date(1, at=seq(dateaxis[1],dateaxis[length(dateaxis)],by="months") ,format="%b")
  if(GF!=0){ legend(x="topleft",legend=paste("gf=",GF))}
 }




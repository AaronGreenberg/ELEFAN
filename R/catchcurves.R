#' Minimal doc
#' @description minimal documentation for roxygen purposes and could be added later 
#' @export

plotnonseacatchcurve <- function(Kloc=K,Linfloc=Linf,pointsupper,pointslower){
#time to catchompute the standard length-converted catch curves :LCC1 sec 2.2 pg2
#first I compute the number of fish caught in each age class.
  print(Kloc)
  print(Linfloc)
  print(points)
  dataloc <- datain
  size <- length(dataloc[1,]) #
  
  if(size>2){
  stripdataloc <- dataloc[,2:size]  
  sumsample <- rowSums(stripdataloc)
  }else{sumsample <- dataloc[,2]}
    
#then I compute the time needed for the fish of a given length class to grow
#through that length class
  width <- 2*(dataloc$ML[2]-dataloc$ML[1])/2 #assuming that the lengthfreq lengths are mid points.
  print("catch curve ")
  print(width)
  delti <- -1/Kloc*log((Linfloc-(dataloc$ML+width))/(Linfloc-(dataloc$ML-width)))#-to
  ti <- -1/Kloc*log(1-(dataloc$ML)/Linfloc)
sumsample <- ifelse(sumsample==0,mean(sumsample),sumsample)#get rid of zeros
#Ok time to make the plot
widthvec <- pointslower:(pointsupper)
print(ti)
widthvec2 <-0:pointslower
z <- lm(log(sumsample[widthvec]/delti[widthvec])~ti[widthvec])
par(1,las=1,bty='n',oma=c(0,1,1,1))
plot(x=ti[widthvec2],y=(z$coefficients[1]+z$coefficients[2]*ti[widthvec2]),type="l",lty=2,col="black",xlab=bquote(paste("Relative age (t-t"[o],")")), ylab=expression(paste("Relative abundance (ln(N/",Delta,"t))")),yaxt="n",xaxt="n",xlim=c(-0.5,ceiling(max(ti))),ylim=c(0,ceiling(1.1*max(log(sumsample/delti)))))#make the line that does not count
axis(2,tck=0.02,las=2)
axis(1,tck=0.02)

points(ti,log(sumsample/delti))
text(ti,log(sumsample/delti)+.2*log(max(log(sumsample/delti))),as.character(1:length(ti)))
points(ti[widthvec],log(sumsample[widthvec]/delti[widthvec]),pch=19,col="black")

lines(x=ti[widthvec],y=(z$coefficients[1]+z$coefficients[2]*ti[widthvec]),col="black")#make the line through the selected points

  
temp2 <-bquote(paste("ln(N/",Delta,"t) = ",.(signif(z$coefficients[1],3)),.(signif(z$coefficients[2],3)),"*age"," ; ",r^2," = ",.(signif(summary(z)$r.squared,3))))  
legend(x="topright",legend=temp2,inset=0.02)  
print(z)
selectivity <- list()
selectivity$prob <- 1:length(datain[,1])*0+1
selectivity$index <- 1:pointslower
selectivity$prob[1:pointslower]<- (sumsample[1:(pointslower)]/delti[1:(pointslower)])/exp(z$coefficients[1]+z$coefficients[2]*ti[1:(pointslower)])#compute selectivity probability
print(selectivity)
dataout <- datain#initialize
for(i in 2:length(datain[1,])){dataout[,i] <-datain[,i]/selectivity$prob} 
  
return(nonseasonal=list(data=dataout,prob=selectivity$prob))  
}




plotseacatchcurve<- function(Kloc=K,Linfloc=Linf,Cloc=C,TW=Tw){

  # initialize data structure
  growthdata <- matrix(0,ncol=days,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
  lfdata<- fillgrowthdata(datein,datain,growthdata) #make data structure with length frequency data

  
  #--1--compute oldest and youngest
  #locate the oldest and youngest fish.
  youngest <- max(which(lfdata[1,]>0))            #get oldest fish
  oldest <- min(which(lfdata[length(lfdata[,1]),]>0)) #get youngest fish.
  print("hum")
  # compute growth curve that goes through oldest and youngest

  gcurve1 <- curves_cpp(Linfloc,Cloc,TW,Kloc,datain$ML,days,youngest,datain$ML[1],BIRTHDAY)#compute growth curve that goes through oldest
  gcurve2 <- curves_cpp(Linfloc,Cloc,TW,Kloc,datain$ML,days,oldest,datain$ML[length(datain$ML)],BIRTHDAY)#compute growth curve that goes through youngest
  index <-which(colSums(lfdata)>0)
  print("index where lfdata not equal zero")
  print(index)
  tzero <- sort(seq(oldest+gcurve2$tzero,youngest+gcurve1$tzero,length.out=(length(datain$ML)+1)),decreasing=TRUE)
# gcurvemain <- 0*lfdata
 count=1
  pointscurve <- matrix(0,ncol=4,nrow=days*datain$ML)
  
  for(i in 1:length(datain$ML)){
  tempered <- curves_cpp(Linfloc,Cloc,TW,Kloc,datain$ML,days,tzero[i],0,BIRTHDAY)$c
  print(index)
  for( j in index){
      gcurvemain <- as.vector(tempered[,3])
      if(j>tzero[i]){
      
      int <- which(tempered[,1]==j)
#      print("int")
#      print(int)
#      print(length(int))
      if(length(int)!=0){
      pointscurve[count,1] <- i #determine curve
      pointscurve[count,2] <- tempered[int,2]#get index of location x axis.date...
      pointscurve[count,3] <- tempered[int,3]#get index of location y axis. size at date...
      pointscurve[count,4] <- tempered[int,4]#get index of location bin probably not needed...
       count=count+1
    }
    }
    }
 }


  x11()
  timeblue <- as.vector(tempered[,1])  
catchrqFreqPlot(1:days,datain$ML,lfdata,c(youngest,oldest,oldest+gcurve2$tzero,youngest+gcurve1$tzero),c(datain$ML[1],datain$ML[length(datain$ML)],0,0),tzero,gcurve1,gcurve2,gcurvemain,pointscurve,timeblue,datein,barscale=1,GF=0)

  abundancein <- matrix(0,ncol=length(tzero),nrow=length(index))
  #sort things by day and length
  binwidth <- datain$ML[2]-datain$ML[1]
  pointscurve2 <- pointscurve[order(pointscurve[,2],pointscurve[,3]),]
  print((pointscurve2))
  LFdata <- datain
  for(i in 1:(length(index))) {#loop over days
   cindex=which(pointscurve2[,2]==index[i]) #get index for correct day
   tempcount <- 1
  for(j in 1:(length(tzero))){ #loop over pointscurve2
    #get lp
     if(j<min(pointscurve2[cindex,1]))
       {
         #print(c("min",j,pointscurve2[cindex,1]))
         abundancein[i,j]=0

       }
     else{
      print(c("tempcount",tempcount))  
     print(cindex)  

     print(pointscurve2[cindex[tempcount],])  
     lp=c(pointscurve2[cindex[tempcount],3],pointscurve2[cindex[tempcount],4])
     #get up
     up=c(pointscurve2[cindex[tempcount+1],3],pointscurve2[cindex[tempcount+1],4])
     print(tempcount)
     print(up)
     print(lp)

      print("Day and curve")
      print(c(index[i],tempcount))
           LFindex=which(LFdata[,1]>=lp[2]&LFdata[,1]<=up[2])
      top=1
     if( (datain$ML[length(datain$ML)]+.5*binwidth)>up[1]){
     top=(up[2]+.5*binwidth-up[1])/(binwidth)#*LFdata[LFindex[1],i+1]
   }
     bottom=1#make sure that curve is in data
     if(lp[1]>(lp[2]-.5*binwidth)){
     bottom= 1-(lp[2]+.5*binwidth-lp[1])/(binwidth)#*LFdata[LFindex[length(LFindex)],i+1]
   }
     
     print("Edges")
     if(top>1){print(top)}
     if(bottom>1){print(bottom)}
     if(top<01){print(top)}
     if(bottom<01){print(bottom)}
     
     if(length(LFindex)>2){
      print("LFindex")
      print("okay?")
      print(LFindex)
     print(LFindex[2:(length(LFindex)-1)])

     abundancein[i,j]=sum(LFdata[LFindex[2:(length(LFindex)-1)],i+1])+top+bottom
      tempcount=tempcount+1
     }else{
       print("LFindex")
      print("okay2?")
      print(LFindex)
       abundancein[i,j]=top+bottom
       tempcount=tempcount+1
     }
  }
   }

}

  
print("abundancein")
print(abundancein)
  
storagesum <- colSums(abundancein)



print(datain)
print("STORAGESUM")
  print(storagesum)
#plot long vs slow.
 plot(1:(length(datain$ML)+1),storagesum)


}








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
  index <-which(colSums(lfdata)>0)
  print("index where lfdata not equal zero")
  print(index)
  print(colSums(lfdata)[index])
  print(max(index))
  print(min(index))
  print(lfdata[,max(index)])
  print(lfdata[,min(index)])
  youngest <- max(index)#lfdata[1,max(index)]
  oldest <- min(index)#lfdata[length(datain$ML),min(index)]
  #youngest <- max(which(lfdata[1,]>0))            
  #oldest <- max(which(lfdata[length(lfdata[,1]),]>0)) 
  print("hum")
  # compute growth curve that goes through oldest and youngest

  gcurve1 <- curves_cpp(Linfloc,Cloc,TW,Kloc,datain$ML,days,youngest,datain$ML[1],BIRTHDAY)#compute growth curve that goes through oldest
  gcurve2 <- curves_cpp(Linfloc,Cloc,TW,Kloc,datain$ML,days,oldest,datain$ML[length(datain$ML)],BIRTHDAY)#compute growth curve that goes through youngest
  #index <-which(colSums(lfdata)>0)
  
  tzero <- floor(sort(seq(oldest+gcurve2$tzero,youngest+gcurve1$tzero,length.out=(length(datain$ML)+1)),decreasing=TRUE))
# gcurvemain <- 0*lfdata
 count=1
  pointscurve <- matrix(0,ncol=4,nrow=length(tzero)*length(index))
  gcurvemain <- vector()
  timeblue <- vector()
  for(i in 1:length(tzero)){#loop over curves
  tempered <- curves_cpp(Linfloc,Cloc,TW,Kloc,datain$ML,days,tzero[i],0,BIRTHDAY)
  #print(index)
  for( j in 1:length(index)){ #loop over days
      gcurvemain <- c(gcurvemain,as.vector(tempered$c[,3]))
      timeblue <- c(timeblue,as.vector(tempered$c[,1]))
      int <- which(tempered$c[,1]==index[j])
      print(int)
      print(j)
      print(pointscurve)
      if(length(int)!=0){
      pointscurve[count,1] <- i #determine curve
      pointscurve[count,2] <- tempered$c[int,1]#+tempered$tzero#get index of location x axis.date...
      pointscurve[count,3] <- tempered$c[int,3]#get index of location y axis. size at date...
      pointscurve[count,4] <- tempered$c[int,4]#get index of location bin probably not needed...
       count=count+1

    }
    }
 }
#The above makes the table.
  
  pointcurve <- as.data.frame(pointscurve)
  colnames(pointcurve) <- c("curve","day","length","bin")
  print(pointcurve)
  pointsout <- matrix(0,nrow=length(tzero),ncol=length(index))
  #loop over days
  for(i in 1:length(index)){
    
   print(i)
   
  #loop over correct number of curves and fill in table.
   subday=subset(pointcurve, day==index[i]) #get part that is correct day.
   print(subday)
   print(index[i])
   print(datain$ML)
   #get bins between curves
   for(j in 1:(length(datain$ML)-1)){

    curvebin <- which(datain$ML >= floor(subday$length[j]) &datain$ML <= ceiling(subday$length[j+1]))
    print(paste("the vector of bins between curve", j, "and",j+1))
    print(curvebin)
    print(datain$ML[curvebin])
    pointsout[j,i] <- sum(datain[curvebin,i+1])#add up all things
  #row sums are sum of points inside curves.
  }
 }
  #pointsout <- prop.table(pointsout,2)
  ages <- vector()
  for(i in 1:length(tzero)){#loop over curves
  tempered <- curves_cpp(Linfloc,Cloc,TW,Kloc,datain$ML,days,tzero[i],0,BIRTHDAY)
  ages[i] <- tempered$c[max(index),1]+tempered$tzero
  }
  print(pointsout)
  print(colSums(pointsout))
  plot(ages/365,log(rowSums(pointsout)))

  x11()

catchrqFreqPlot(1:days,datain$ML,lfdata,c(youngest,oldest,oldest+gcurve2$tzero,youngest+gcurve1$tzero),c(datain$ML[1],datain$ML[length(datain$ML)],0,0),tzero,gcurve1,gcurve2,gcurvemain,pointscurve,timeblue,datein,barscale=1,GF=0)
  
  

}








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
par(1,las=1,bty='n')
plot(x=ti[widthvec2],y=(z$coefficients[1]+z$coefficients[2]*ti[widthvec2]),type="l",
     col="black",xlab="Age", ylab=bquote(paste("ln(N)/",Delta,"T")),xlim=c(-0.5,ceiling(max(ti))),ylim=c(0,ceiling(1.1*max(log(sumsample/delti)))))#make the line that does not count  
points(ti,log(sumsample/delti))
text(ti,log(sumsample/delti)+.2*log(max(log(sumsample/delti))),as.character(1:length(ti)))
points(ti[widthvec],log(sumsample[widthvec]/delti[widthvec]),pch=19,col="black")

lines(x=ti[widthvec],y=(z$coefficients[1]+z$coefficients[2]*ti[widthvec]),col="red")#make the line through the selected points

  
temp2 <-bquote(paste("log(N)/",Delta,"T"," = ",.(signif(z$coefficients[1],4)),.(signif(z$coefficients[2],4)),"Age","  ",R^2,"=",.(signif(summary(z)$r.squared,4))))  
legend(x="topright",legend=temp2)  
print(z)
selectivity <- list()
selectivity$prob <- 1:length(datain[,1])*0+1
selectivity$index <- 1:pointslower
selectivity$prob[1:pointslower]<- (sumsample[1:(pointslower)]/delti[1:(pointslower)])/exp(z$coefficients[1]+z$coefficients[2]*ti[1:(pointslower)])#compute selectivity probability
print(selectivity)
dataout <- datain#initialize
for(i in 2:length(datain[1,])){dataout[,i] <-datain[,i]*selectivity$prob} 
  
return(nonseasonal=list(data=dataout,prob=selectivity$prob))  
}




plotseacatchcurve<- function(Kloc=K,Linfloc=Linf,Cloc=C,TW=Tw){
  print("Hi")
  # initialize data structure
  growthdata <- matrix(0,ncol=days,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
  lfdata<- fillgrowthdata(datein,datain,growthdata) #make data structure with length frequency data
  print("so")
  
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
 tzero <- (seq(oldest+gcurve2$tzero,youngest+gcurve1$tzero,length.out=length(datain$ML)))
# gcurvemain <- 0*lfdata
 count=1
  pointscurve <- matrix(0,ncol=4,nrow=days*datain$ML)
  
  for(i in 1:14){
  tempered <- curves_cpp(Linfloc,Cloc,TW,Kloc,datain$ML,days,tzero[i],0,BIRTHDAY)$c
  print(index)
  for( j in index){
      gcurvemain <- as.vector(tempered[,3])
      if(j>tzero[i]){
      int <- which(tempered[,1]==j)
      print("int")
      print(int)
      print(tempered[int,1])
      if(int>=1){
      pointscurve[count,1] <- i #determine curve
      pointscurve[count,2] <- tempered[int,2]#get index of location x axis.date...
      pointscurve[count,3] <- tempered[int,3]#get index of location y axis. size at date...
      pointscurve[count,4] <- tempered[int,4]#get index of location bin probably not needed...
     # print(pointscurve)
       count=count+1
    }
    }
    }
     }
  
 timeblue <- as.vector(tempered[,1])
  

 

print(pointscurve)
  
catchrqFreqPlot(1:days,datain$ML,lfdata,c(youngest,oldest,oldest+gcurve2$tzero,youngest+gcurve1$tzero),c(datain$ML[1],datain$ML[length(datain$ML)],0,0),tzero,gcurve1,gcurve2,gcurvemain,pointscurve,timeblue,datein,barscale=1,GF=0)


}








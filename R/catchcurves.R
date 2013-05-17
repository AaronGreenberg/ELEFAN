
plotnonseacatchcurve <- function(Kloc=K,Linfloc=Linf,pointsupper,pointslower){
#time to compute the standard length-converted catch curves :LCC1 sec 2.2 pg2
#first I compute the number of fish caught in each age class.
  print(Kloc)
  print(Linfloc)
  print(points)
  dataloc <- data
  size <- length(dataloc[1,]) #
  
  if(size>2){
  stripdataloc <- dataloc[,2:size]  
  sumsample <- rowSums(stripdataloc)
  }else{sumsample <- dataloc[,2]}
    
#then I compute the time needed for the fish of a given length class to grow
#through that length class
  width <- 2*(dataloc$ML[2]-dataloc$ML[1])/2 #assuming that the lengthfreq lengths are mid points.
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
selectivity$prob <- 1:length(data[,1])*0+1
selectivity$index <- 1:pointslower
selectivity$prob[1:pointslower]<- (sumsample[1:(pointslower)]/delti[1:(pointslower)])/exp(z$coefficients[1]+z$coefficients[2]*ti[1:(pointslower)])#compute selectivity probability
print(selectivity)
dataout <- data#initialize
for(i in 2:length(data[1,])){dataout[,i] <-data[,i]*selectivity$prob} 
  
return(dataout)  
}




plotseacatchcurve<- function(Kloc=K,Linfloc=Linf,Cloc=C,TW=Tw){
  print("Hi")
  # initialize data structure
  growthdata <- matrix(0,ncol=days,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
  lfdata<- fillgrowthdata(date,data,growthdata) #make data structure with length frequency data
  print("so")
  
  #--1--compute oldest and youngest
  #locate the oldest and youngest fish.
  youngest <- max(which(lfdata[1,]>0))            #get oldest fish
  oldest <- min(which(lfdata[length(lfdata[,1]),]>0)) #get youngest fish.
  print("hum")
  # compute growth curve that goes through oldest and youngest

  gcurve1 <- curves_cpp(Linfloc,Cloc,TW,Kloc,data$ML,days,youngest,data$ML[1],BIRTHDAY)#compute growth curve that goes through oldest
  gcurve2 <- curves_cpp(Linfloc,Cloc,TW,Kloc,data$ML,days,oldest,data$ML[length(data$ML)],BIRTHDAY)#compute growth curve that goes through youngest

 tzero <- ceiling(seq(oldest+gcurve2$tzero,youngest+gcurve1$tzero,length.out=5))
# gcurvemain <- matrix(0,nrow=5,ncol=days)
  
 tempered <- curves_cpp(Linfloc,Cloc,TW,Kloc,data$ML,days,tzero[2],0,BIRTHDAY)$c
 gcurvemain <- as.vector(tempered[,3])

  

 timeblue <- as.vector(tempered[,1])


  
catchrqFreqPlot(1:days,data$ML,lfdata,c(youngest,oldest,oldest+gcurve2$tzero,youngest+gcurve1$tzero),c(data$ML[1],data$ML[length(data$ML)],0,0),tzero,gcurve1,gcurve2,gcurvemain,timeblue,date,barscale=1,GF=0)


}








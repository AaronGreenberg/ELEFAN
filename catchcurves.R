
plotnonseacatchcurve <- function(Kloc=K,Linfloc=Linf,pointsupper,pointslower){
#time to compute the standard length-converted catch curves :LCC1 sec 2.2 pg2
#first I compute the number of fish caught in each age class.
  print(Kloc)
  print(Linfloc)
  print(points)
  dataloc <- data
  size <- length(dataloc[1,]) #
  
  if(size>g2){
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
par(1,las=1,bty='n')
plot(ti,log(sumsample/delti),xlab="Age", ylab="ln(N)/delT)",xlim=c(0,ceiling(max(ti))),ylim=c(0,ceiling(max(log(sumsample/delti)))))
text(ti,log(sumsample/delti)+.2*log(max(log(sumsample/delti))),as.character(1:length(ti)))
points(ti[widthvec],log(sumsample[widthvec]/delti[widthvec]),pch=19,col="black")
z <- lm(log(sumsample[widthvec]/delti[widthvec])~ti[widthvec])
lines(x=ti[widthvec],y=(z$coefficients[1]+z$coefficients[2]*ti[widthvec]),col="black")
temp2 <-bquote(paste("log(N)/(delT)"," = ",.(signif(z$coefficients[1],4)),.(signif(z$coefficients[2],4)),"Age","  ",R^2,"=",.(signif(summary(z)$r.squared,4))))  
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


plotseacatchcurve<- function(Kloc=K,Linfloc=Linf,dataloc=data){ print("This is still a dummy")}

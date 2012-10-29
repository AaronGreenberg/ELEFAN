
plotnonseacatchcurve<- function(Kloc=K,Linfloc=Linf,dataloc=data){
#time to compute the standard length-converted catch curves :LCC1 sec 2.2 pg2
#first I compute the number of fish caught in each age class.
  size <- length(dataloc[1,]) #
#  print("size")
  stripdataloc <- dataloc[,2:size]
  sumsample <- rowSums(stripdataloc)
#  print("sumsample")
#then I compute the time needed for the fish of a given length class to grow
#through that length class
to <- 1
  
  width <- 2*(dataloc$ML[2]-dataloc$ML[1])/2#assuming that the lengthfreq lengths are mid points.
  print(width)
  delti <- -1/Kloc*log((Linfloc-(dataloc$ML+width))/(Linfloc-(dataloc$ML-width)))-to
  ti <- -1/Kloc*log(1-(dataloc$ML)/Linfloc)
  print(ti)
#Ok time to make the plot
widthvec <- 5:(length(sumsample)-5)
#print(widthvec)

par(1,las=1)
plot(ti,log(sumsample/delti),xlab="Years", ylab="ln(N)/delT)")
points(ti[widthvec],log(sumsample[widthvec]/delti[widthvec]),pch=19,col="red")
z <- lm(log(sumsample[widthvec]/delti[widthvec])~ti[widthvec])

abline(z)
print(z)
}


plotseacatchcurve<- function(Kloc=K,Linfloc=Linf,dataloc=data){ print("This is still a dummy")}

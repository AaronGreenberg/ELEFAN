
plotnonseacatchcurve <- function(Kloc=K,Linfloc=Linf,points){
#time to compute the standard length-converted catch curves :LCC1 sec 2.2 pg2
#first I compute the number of fish caught in each age class.
  print("Hi i am catch curve")
  print(Kloc)
  print(Linfloc)
  print(points)
  dataloc <- data
  size <- length(dataloc[1,]) #
#  print("size")
  stripdataloc <- dataloc[,2:size]
  sumsample <- rowSums(stripdataloc)
#  print("sumsample")
#then I compute the time needed for the fish of a given length class to grow
#through that length class

  width <- 2*(dataloc$ML[2]-dataloc$ML[1])/2#assuming that the lengthfreq lengths are mid points.
  print(width)
  delti <- -1/Kloc*log((Linfloc-(dataloc$ML+width))/(Linfloc-(dataloc$ML-width)))#-to
  ti <- -1/Kloc*log(1-(dataloc$ML)/Linfloc)
  print("x")
  print(ti)
  print("delti")
  print(delti)
  print("sumsample")
  print(sumsample)
  print("y")
  print(log(sumsample/delti))
#Ok time to make the plot
widthvec <- points:(length(sumsample))
print(widthvec)

par(1,las=1,bty='n')
plot(ti,log(sumsample/delti),xlab="Years", ylab="ln(N)/delT)",xlim=c(0,ceiling(max(ti))),ylim=c(0,ceiling(max(log(sumsample/delti)))))
points(ti[widthvec],log(sumsample[widthvec]/delti[widthvec]),pch=19,col="black")
z <- lm(log(sumsample[widthvec]/delti[widthvec])~ti[widthvec])
abline(z)
print(z)
}


plotseacatchcurve<- function(Kloc=K,Linfloc=Linf,dataloc=data){ print("This is still a dummy")}

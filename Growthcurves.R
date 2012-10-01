#This script computes the LCC and Seasonaly adjusted growth curves for
#the salmon paper.

data <- read.table("salmon.dat",head=TRUE)
print(data)
#time to compute the standard length-converted catch curves :LCC1 sec 2.2 pg2

#first I compute the number of fish caught in each age class.
  size <- length(data[1,]) #
  stripdata <- data[,2:size]
  sumsample <- rowSums(stripdata)
#then I compute the time needed for the fish of a given length class to grow
#through that length class
  K <- .087
  Linf <- 70.8
  to <- -0.67
  
  width <- 2*(data$ML[2]-data$ML[1])/2#assuming that the lengthfreq lengths are mid points.
  print(width)
  delti <- -1/K*log((Linf-(data$ML+width))/(Linf-(data$ML-width)))-to
  ti <- -1/K*log(1-(data$ML)/Linf)
  print(ti)
#Ok time txo make the plot
widthvec <- 9:(length(sumsample)-5)
print(widthvec)
jpeg("catchcurve.jpg")
par(1,las=1)
plot(ti,log(sumsample/delti),xlab="Years", ylab="ln(N)/delT)")
points(ti[widthvec],log(sumsample[widthvec]/delti[widthvec]),pch=19,col="red")
z <- lm(log(sumsample[widthvec]/delti[widthvec])~ti[widthvec])

abline(z)
print(z)
dev.off()
# is .794 ~.805?
#### OK onto seasonal curves. 
C <- .5
WP <- .5
ts <- .5+WP

#There are five steps supposibly.
#first make data file. This is typically done.

#second find maximum and minimum relative ages
t0 <- 0
t <- 1:1000
#the seasonal growth curve is
lt  <- Linf*(1-exp(-1*(K*(t-to)+(K*C/2*pi)*(sin(2*pi*(t-ts)-sin(2*pi*(t-to)))))))
plot(t,lt)
#third compute the time difference tmax-tmin by the number of length classes in the file.
Lmax <-max(data$ML)#get max length
Lmin <-min(data$ML)#get min length
print(c(Lmax,Lmin))
#compute tmin and tmax
Tmin <- t[which.min((lt-Lmin)^2)]
Tmax <- t[which.min((lt-Lmax)^2)]
print(c(Tmin,Tmax))
#compute intervals.
intervals <- (Tmax-Tmin)/length(data$ML)

#fourth compute N



#fifth plot  N

#in the seasonal case we must create psudo-cohorts
#we do this by choosing different t0s? and stuff

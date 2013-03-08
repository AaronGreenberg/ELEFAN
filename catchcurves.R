
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


plotseacatchcurve<- function(Kloc=K,Linfloc=Linf,Cloc=C,TW=Tw){
 
 #This takes five easy steps.
 dataloc <- data
  w <- 1/365
  Kloc <- Kloc/365
 #--1--compute oldest and youngest
 
  print(head(dataloc))
  edge <- dataloc$ML[2]-data$ML[1]
  edge <- edge/2
  cohortvector <- dataloc$ML-edge # vector of points that the growth curves go through
 #--2--compute slices
  #each slide is determined by a unique t0
 growth_rootf <- function(x,K,Linf,Cloc,TW){
  #makes computing tstart and time when length is .95%Linf easy.
  Tw <- 1/365
  period <- (Cloc*K)/(2*pi*w)*(sin(2*pi*w*(0-TW-.5/365))-sin(2*pi*w*(x-TW-.5/365)))
  out <- Linf*(1-exp(-K*(0-x)+period))
  return(out)
  }
cgrowth_rootf <- cmpfun(growth_rootf)
  bisect <- function(a,b,equal,K,Linf,Cloc,TW){ #returns t0
  if((cgrowth_rootf(a,K,Linf,Cloc,TW)-equal)^2<10^(-10)){return(a)}
  if((cgrowth_rootf(b,K,Linf,Cloc,TW)-equal)^2<10^(-10)){return(b)}
  #This function uses bisection to compute the required values of tstart and...
  if((growth_rootf(a,K,Linf,Cloc,TW)-equal)*(growth_rootf(b,K,Linf,Cloc,TW)-equal)>0){#make sure that inputs are okay... 
    print("f(xup) and f(xlow) are of same sign 1")
    print(paste("fxup::",(cgrowth_rootf(a,K,Linf,Cloc,TW)-equal),"fxlow::",(cgrowth_rootf(b,K,Linf,Cloc,TW)-equal)))
    return(1)} 
  termtest <- 1#set counter to protect against errors. 
  while(termtest<= 10000) {# limit iterations to prevent infinite loop
    d <- (a + b)/2 #new midpoint
    if(((cgrowth_rootf(d,K,Linf,Cloc,TW)-equal)==0||(b-a)/2<= 10^-(10))) { #solution found
    return(d)
    break
  }
  termtest <- termtest + 1 #increment step counter
  if(sign(cgrowth_rootf(d,K,Linf,Cloc,TW)-equal) == sign(cgrowth_rootf(a,K,Linf,Cloc,TW)-equal)){ a <- d}
  else{b <- d }# new interval
  }
  print("Method failed. max number of steps exceeded")#just a nice test to make sure that the first test really worked.
}
cbisect <- cmpfun(bisect)


 tzero <- cohortvector*0 # initalize tzero vector
 for (i in 1:length(cohortvector)){
 tzero[i] <- bisect(-200*365,200*365,cohortvector[i],Kloc,Linfloc,Cloc,TW)
 }
 print(cohortvector)
 print(tzero)

 time <- 1:(length(cohortvector)*days)
 timestart <- 0*time
    startdate <- as.Date(date[1,1])
   startime <- as.numeric(startdate-date[1,1])
 loccurve <- function(Cloc=Cloc,Kloc=Kloc,TW=TW,timestart=timestart,time=time){

  cur<- matrix(0,length(cohortvector)*days,ncol=4)        #initalize growth curve data structure    
  for( j in 1:length(timestart))
    {
      k <- floor(j/days)+1
      if(j%%days==0){print(k)}
      timestart[j]=tzero[k]+startime
    }

  cur[,1] <-(time)+startime#keep real time
  cur[,2] <-(time)%%days+startime #wrap time so mapping the time to the plot is easy
  period <- (Cloc*Kloc)/(2*pi*w)*(sin(2*pi*w*(time%%days-TW-.5))-sin(2*pi*w*(timestart-TW+.5)))
  cur[,3] <- Linfloc*(1-exp(-Kloc*((time%%days-timestart))+period))#put in the growth curve
#  fn <- function(i){dataloc$ML[which.min(((dataloc$ML)-cur[i,3])^2)]} #snazzy function that allows use of sapply  to find the right bins!
#  cur[,4] <- sapply(1:length(cur[,3]),fn)             #get a version of the growth curve that makes computing esp and asp easy
  print("am local")
  print(head(cur))
   ## cur <- matrix(1,length(cohortvector)*365*2,4)
   cur[,4] <- time*0
   ## cur[,2] <- time%%days
 return(list(c=cur))
}
  growthdata <- matrix(0,ncol=days,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
  lfdata<- fillgrowthdata(date,data,growthdata) #make data structure with length frequency data
  gcurve<-loccurve(Cloc=Cloc,Kloc=Kloc,TW=TW,timestart=timestart,time=time)
 
  rqFreqPlot(1:days,data$ML,lfdata,1,1,gcurve,date,barscale=days*(1-exp(-days))/(50*length(date[,2])),GF=0)
  
 #--3--compute sums
  
 #--4--make plots


}

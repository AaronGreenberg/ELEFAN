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
  width <- (dataloc$ML[2]-dataloc$ML[1]) #assuming that the lengthfreq lengths are mid points.
  print("catch curve ")
  print(width)
  delti <- -1/Kloc*log((Linfloc-(dataloc$ML+width))/(Linfloc-(dataloc$ML-width)))#-to
  ti <- -1/Kloc*log(1-(dataloc$ML)/Linfloc)
sumsample <- ifelse(sumsample==0,mean(sumsample),sumsample)#get rid of zeros
#Ok time to make the plot
widthvec <- pointslower:(pointsupper) #get vector of circles used in linear regression.
print(ti)
widthvec2 <-0:pointslower#points not selected
z <- lm(log(sumsample[widthvec]/delti[widthvec])~ti[widthvec])#compute linear model.
par(1,las=1,bty='n',oma=c(0,1,1,1))
plot(x=ti[widthvec2],y=(z$coefficients[1]+z$coefficients[2]*ti[widthvec2]),type="l",lty=2,col="black",xlab=bquote(paste("Relative age (t-t"[o],";year)")), ylab=expression(paste("Relative abundance (ln(N/",Delta,"t))")),yaxt="n",xaxt="n",xlim=c(-0.5,ceiling(max(ti))),ylim=c(0,ceiling(1.1*z$coefficients[1])))#make the line that does not count goes trough widthvec2 ... the points NOT selected
axis(2,tck=0.02,las=2)
axis(1,tck=0.02)

points(ti,log(sumsample/delti))#make open circles
text(ti,log(sumsample/delti)+.2*log(max(log(sumsample/delti))),as.character(1:length(ti))) #put on text 
points(ti[widthvec],log(sumsample[widthvec]/delti[widthvec]),pch=19,col="black") #make filled circles only on points in width vect

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
  
return(nonseasonal=list(data=dataout,prob=selectivity$prob,ages=ti))  
}




plotseacatchcurve<- function(Kloc=K,Linfloc=Linf,Cloc=C,TW=Tw,pointsupper,pointslower){
  # initialize data structure
  growthdata <- matrix(0,ncol=days,nrow=lfbin) #create matrix of zeros that will represent a years worth of data(see fillgrowth data)
  lfdata<- fillgrowthdata(datein,datain,growthdata) #make data structure with length frequency data

  #--1--compute oldest and youngest
  #locate the oldest and youngest fish.
  index <-which(colSums(lfdata)>0)
  youngest <- max(index)  #lfdata[1,max(index)]
  oldest <- min(index)    #lfdata[length(datain$ML),min(index)]
 #2 compute growth curve that goes through oldest and youngest
  binwidth <- (datain$ML[2]-datain$ML[1]) #compute width of bins
  gcurve1 <- curves_cpp(Linfloc,Cloc,TW,Kloc,datain$ML,days,youngest,datain$ML[1]-.5*binwidth,BIRTHDAY)#compute growth curve that goes through oldest
  gcurve2 <- curves_cpp(Linfloc,Cloc,TW,Kloc,datain$ML,days,oldest,datain$ML[length(datain$ML)]+.5*binwidth,BIRTHDAY)#compute growth curve that goes through youngest

  #find vector of tzeros that determine slicing growth curves
 tzero <- floor(sort(seq(oldest+gcurve2$tzero,youngest+gcurve1$tzero,length.out=length(datain$ML)),decreasing=TRUE))

# tzero <- floor(sort(seq(oldest+gcurve2$tzero,youngest+gcurve1$tzero,length.out=6),decreasing=TRUE))
  #3 compute pointscurve this computes the growth curves.
 count=1
  pointscurve <- matrix(0,ncol=4,nrow=length(tzero)*length(index))
  gcurvemain <- vector()
  timeblue <- vector()
  for(i in 1:length(tzero)){#loop over curves
  tempered <- curves_cpp(Linfloc,Cloc,TW,Kloc,datain$ML,days,tzero[i],0,BIRTHDAY)
  zeroout <- c(which(tempered$c[,1]>max(index)),which(tempered$c[,1]<min(index)))
  tempered$c[zeroout,] <- 0#set all growth curves greater than date =0
  #print(index)
  for( j in 1:length(index)){ #loop over days
      gcurvemain <- c(gcurvemain,as.vector(tempered$c[,3]))
      timeblue <- c(timeblue,as.vector(tempered$c[,1]))
      int <- which(tempered$c[,1]==index[j])#sort out real time
      if(length(int)!=0){
      pointscurve[count,1] <- i #determine curve
      pointscurve[count,2] <- tempered$c[int,1]#+tempered$tzero#get index of location x axis.date...
      pointscurve[count,3] <- min(c(tempered$c[int,3],datain$ML[which.max(datain$ML)]+.5*binwidth))#get index of location y axis. size at date...
      pointscurve[count,4] <- tempered$c[int,4]#get index of location bin probably not needed...
    }else{
      pointscurve[count,1] <- i #determine curve
      pointscurve[count,2] <- index[j]#get index of location x axis.date...
      if(max(pointscurve[which(pointscurve[,2]==index[j]),3])>=datain$ML[which.max(datain$ML)]){#check to see if the curve is bigger than 95% Linf
      pointscurve[count,3] <- datain$ML[which.max(datain$ML)]+.5*binwidth#get index of location y axis. size at date...
      pointscurve[count,4] <- datain$ML[which.max(datain$ML)]#get index of location bin probably not needed...
        }else{#the fish haven't been born yet.
      pointscurve[count,3] <- datain$ML[which.min(datain$ML)]-.5*binwidth#get index of location y axis. size at date...
      pointscurve[count,4] <- datain$ML[which.min(datain$ML)]#get index of location bin probably not needed...
          }
        }      
       count=count+1
 
    }
    }

 
  #The above makes the table. of growth curves
  #now we need to intersect the growth curves and the data
  #and add things up. 
  pointcurve <- as.data.frame(pointscurve)
  colnames(pointcurve) <- c("curve","day","length","bin")
  #print(pointcurve)
  pointsout <- matrix(0,nrow=length(tzero),ncol=length(index))
  #loop over days
  for(i in 1:length(index)){
  #loop over correct number of curves and fill in table.
   z=as.data.frame(subset(pointcurve,pointcurve$day==index[i])) #get part of point curve table that corresponds to correct day
   colnames(z) <- c("curve","day","length","bin") #get part that is correct day.
   subday=z#correct subday!
   count=0;
   weight1=1
   weight2=1
   weight0=1
   for(j in 1:(length(tzero)-1)){ #for each growth curve
     #this works!
     upper <- ceiling(subday$length[j+1])
     lower<- floor(subday$length[j])
     if(is.na(subday$length[j+1])){upper=max(datain$ML)+binwidth/2}
     if(is.na(subday$length[j])){lower=min(datain$ML)-binwidth/2}
     curvebin <- which(datain$ML >= lower & datain$ML <= upper)#get bins between growth curves
     lcut <-which.min((datain$ML[min(curvebin)]-subday$bin)^2) #index of lower cut in weighting routine
     ucut <-which.min((datain$ML[max(curvebin)]-subday$bin)^2) #index of upper cut in weighting routine

     if(!is.na(sum(datain[curvebin,i+1]))){#check that curvebin is not empty
        lengthcurvebin <- length(curvebin)#get length of curve bin
        
     if(lengthcurvebin>1){#if length of curve bin is bigger than one compute weights
        #make sure that the length of curve bin is greater than zero
       if(!is.na(subday$length[lcut])&&(subday$length[lcut]>(min(datain$ML)-binwidth/2))){
        #we need to compute two weights which determine the correct weights for the 
         if(subday$length[lcut]<=subday$bin[lcut]){
       weight1=.5+(subday$bin[lcut]-subday$length[lcut])/(binwidth)
     }else{
       weight1=.5-(-subday$bin[lcut]+subday$length[lcut])/(binwidth)
     }
      }else{
        #print("weight 1")
        weight1=1}
      if(!is.na(subday$length[ucut])&&(subday$length[ucut]<(max(datain$ML)+binwidth/2))){
         if(subday$length[ucut]<=subday$bin[ucut]){
       weight2=.5+(subday$length[ucut]-subday$bin[ucut])/(binwidth) 
     }else{
       weight2=.5-(-subday$length[ucut]+subday$bin[ucut])/(binwidth)
         }
      }else{
        #print("weight 2")
        weight2=1}  
      }else{

       if(lengthcurvebin==1){
         ## print("HI")
         ## print(c(i,j))
         upper <-subday$length[j+1]     #
         lower <- subday$length[j]
         if(upper>=max(datain$ML)+binwidth/2){upper <- max(datain$ML)+binwidth/2}
         if(lower>=max(datain$ML)+binwidth/2){lower <- max(datain$ML)+binwidth/2}
         if(upper<=min(datain$ML)-binwidth/2){upper <- min(datain$ML)-binwidth/2}
         if(lower<=min(datain$ML)-binwidth/2){lower <- min(datain$ML)-binwidth/2}
         ## print(c(subday$bin[j],subday$length[j]),digits=10)
         ## print(c(subday$bin[j+1],subday$length[j+1]),digits=10)
         if(is.na(subday$length[j+1])){upper=max(datain$ML)+binwidth/2}
         if(is.na(subday$length[j])){lower=min(datain$ML)-binwidth/2}
         weight0 <- (upper-lower)/binwidth
         #print(weight0)
        }
       
     }

        
        if(lengthcurvebin>=3){#if there are more than three bins in curvebin
         pointsout[subday$curve[j],i] <- sum(datain[curvebin[2:(lengthcurvebin-1)],i+1])+datain[curvebin[1],i+1]*(weight1)+datain[curvebin[lengthcurvebin],i+1]*weight2# add upper bin # add lower bin #add up all things in middle

       }else{
         if(lengthcurvebin==2){#if there are just two bins in the curvebin
           pointsout[subday$curve[j],i] <- datain[curvebin[1],i+1]*(weight1)+datain[curvebin[lengthcurvebin],i+1]*weight2# add upper bin
         }else{
           if(lengthcurvebin==1){#if there is just one bin? Am I doing this right?
           ## print("so there is just one in length curve bin")
           ## print(weight0)
           ## print(c(i,j))
           ## print(datain[curvebin[1],i+1]*(weight0))
           pointsout[subday$curve[j],i] <- datain[curvebin[1],i+1]*(weight0) # add lower bin
           }else{ #this should never happen ? right?
            pointsout[subday$curve[j],i] <- 0
         }}
           
       }
     }
   }
 }

#  pointsout <- prop.table(pointsout,2)*100#normalize the columns
  ages <- vector()#compute ages for plotting
  for(i in 1:length(tzero)){#loop over curves
  tempered <- curves_cpp(Linfloc,Cloc,TW,Kloc,datain$ML,days,tzero[i],0,BIRTHDAY)
  ages[i] <- ifelse(max(index)<length(tempered$c[,1]),tempered$c[max(index),1]-tempered$tzero,tempered$c[length(tempered$c[,1])]-tempered$tzero)
  }

  #pointsout <- pointsout[nrow(pointsout):1,]# got to reverse order so youngest fish are plotted first!
  ages <- sort(ages)
  widthvec <- pointslower:pointsupper
  widthvec2 <-1:nrow(pointsout)
  ## print("points out")
  ## print(ages/365)
  ## print(c(pointslower,pointsupper))
  ## print(log(rowSums(pointsout)))
  ## print(widthvec)
  ## print(widthvec2)
fully <- ifelse((rowSums(pointsout,na.rm=TRUE))==0,1,rowSums(pointsout,na.rm=TRUE))
x <- ages[widthvec]/365
y <- log(fully[widthvec])
z <- lm(y~x)

## print("pointsout")
## print(pointsout,digits=6)
## print("row Sums")  
## print(rowSums(pointsout),digits=6)
#print("col Sums")  
#print(colSums(pointsout),digits=6)
tempsum <- sum(rowSums(pointsout),na.rm=TRUE)
#print(tempsum,digits=6)
#should follow what I did in Catch curve 1.
par(1,las=1,bty='n',oma=c(0,1,1,1))
#make the plots
print(z)
ylimu <-z$coefficients[1]+z$coefficients[2]*floor(min(ages/365))
ylimr <- c(floor(min(log(fully))),ceiling(ylimu))
plot(ages/365,log(fully),xlab=bquote(paste("Relative age (t-t"[o],";year)")),ylab=expression(paste("Relative abundance (ln(N))")),yaxt="n",xaxt="n",ylim=ylimr,xlim=c(floor(min(ages/365)),ceiling(max(ages/365))))
points(ages[widthvec]/365,log(fully[widthvec]),pch=19,col="black") #make filled circles only on points in width vect
axis(2,tck=0.02,las=2)
axis(1,tck=0.02)
text(ages/365,log(fully)+max(c(.2*log(max(c(log(fully),1))),.1)),as.character(1:length(ages))) #put on text
#text(ages/365,log(fully)-.05*log(max(c(log(fully),1))),as.character(round(fully)),col="red") #put on text
lines(x=ages[widthvec2]/365,y=(z$coefficients[1]+z$coefficients[2]*ages[widthvec2]/365),col="grey")#make the line through the selected points
lines(x=ages[widthvec]/365,y=(z$coefficients[1]+z$coefficients[2]*ages[widthvec]/365),col="black")#make the line through the selected points

temp2 <-bquote(paste("ln(N) = ",.(signif(z$coefficients[1],3)),.(signif(z$coefficients[2],3)),"*age"," ; ",r^2," = ",.(signif(summary(z)$r.squared,3))))#),";  sum",.(signif(tempsum,6))))  
legend(x="topright",legend=temp2,inset=0.02)  


  
## x11() #useful plot for debugging 
## catchrqFreqPlot(1:days,datain$ML,lfdata,c(youngest,oldest,oldest+gcurve2$tzero,youngest+gcurve1$tzero),c(datain$ML[1],datain$ML[length(datain$ML)],0,0),tzero,gcurve1,gcurve2,gcurvemain,pointscurve,timeblue,datein,barscale=1,GF=0)

}








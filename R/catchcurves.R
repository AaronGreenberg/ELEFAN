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
  gcurve1 <- curves_cpp(Linfloc,Cloc,TW,Kloc,datain$ML,days,youngest,datain$ML[1],BIRTHDAY)#compute growth curve that goes through oldest
  gcurve2 <- curves_cpp(Linfloc,Cloc,TW,Kloc,datain$ML,days,oldest,datain$ML[length(datain$ML)],BIRTHDAY)#compute growth curve that goes through youngest
  #index <-which(colSums(lfdata)>0)
  
  #find vector of tzeros that determine slicing growth curves
  #tzero <- floor(sort(seq(oldest+gcurve2$tzero,youngest+gcurve1$tzero,length.out=length(datain$ML)),decreasing=TRUE))#(length(datain$ML)+1)),decreasing=TRUE))
  tzero <- floor(sort(seq(oldest+gcurve2$tzero,youngest+gcurve1$tzero,length.out=3),decreasing=TRUE))#(length(datain$ML)+1)),decreasing=TRUE))
  #3 compute pointscurve this computes the growth curves.
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
      ## print(int)
      ## print(j)
      ## print(pointscurve)
      if(length(int)!=0){
      pointscurve[count,1] <- i #determine curve
      pointscurve[count,2] <- tempered$c[int,1]#+tempered$tzero#get index of location x axis.date...
      pointscurve[count,3] <- tempered$c[int,3]#get index of location y axis. size at date...
      pointscurve[count,4] <- tempered$c[int,4]#get index of location bin probably not needed...
       count=count+1

    }
    }
 }
#The above makes the table. of growth curves
  #now we need to intersect the growth curves and the data
  #and add things up. This is the part with problems

  
  pointcurve <- as.data.frame(pointscurve)
  colnames(pointcurve) <- c("curve","day","length","bin")
  print(pointcurve)
  pointsout <- matrix(0,nrow=length(tzero),ncol=length(index))
  #loop over days
  
  for(i in 1:length(index)){
  #loop over correct number of curves and fill in table.
   z=as.data.frame(subset(pointcurve,pointcurve$day==index[i])) #get part of point curve table that corresponds to correct day
   colnames(z) <- c("curve","day","length","bin") #get part that is correct day.
   subday=z#correct subday!
   count=0;
   for(j in 1:(length(tzero)-1)){ #for each growth curve
     #this works!
     weight1=1
     weight2=1
     binwidth <- (datain$ML[2]-datain$ML[1]) #compute width of bins
     upper <- ceiling(subday$length[j+1])
     lower<- floor(subday$length[j])
     if(is.na(subday$length[j+1])){upper=max(datain$ML)+binwidth/2}
     if(is.na(subday$length[j])){lower=min(datain$ML)-binwidth/2}
     print("subday")
     print(subday$length[j])
     print("lower")
     print(lower)
     print("ceiling")
     print(ceiling(subday$length[j+1]))
     print("upper")
     print(upper)

     curvebin <- which(datain$ML >= lower & datain$ML <= upper)#get bins between growth curves
     print(curvebin)
     print(c(i,j))
      #curvebin <- which(datain$ML >= subday$bin[j] & datain$ML <=subday$bin[j+1])#get bins between growth curves
      if(!is.na(sum(datain[curvebin,i+1]))){#check that curvebin is not empty
        lengthcurvebin <- length(curvebin)#get length of curve bin

        #seems to work up to now!
        if(lengthcurvebin>0){#if length of curve bin is bigger than zero compute weights
        #make sure that the length of curve bin is greater than zero
       if(!is.na(subday$length[curvebin[1]])&&(subday$length[curvebin[1]]>=(min(datain$ML)-binwidth/2))){
        #we need to compute two weights which determine the correct weights for the Are these right?!
         if(subday$length[curvebin[1]]<subday$bin[curvebin[1]]){
       weight1=.5-(subday$bin[curvebin[1]]-subday$length[curvebin[1]])/(binwidth)
     }else{
       weight1=.5+(-subday$bin[curvebin[1]]+subday$length[curvebin[1]])/(binwidth)
     }
      }
       
       else{weight1=1}
       if(!is.na(subday$length[curvebin[lengthcurvebin]])&&(subday$length[curvebin[lengthcurvebin]]<=(max(datain$ML)+binwidth/2))){
         if(subday$length[curvebin[lengthcurvebin]]<subday$bin[curvebin[lengthcurvebin]]){
       weight2=.5-(subday$length[curvebin[lengthcurvebin]]-subday$bin[curvebin[lengthcurvebin]])/(binwidth) 
     }else{
       weight2=(-subday$length[curvebin[lengthcurvebin]]+subday$bin[curvebin[lengthcurvebin]])/(binwidth) +.5}
      }else{weight2=1}  

     }
       if(max(c(weight1,weight2))>1){#error check. Weights should be between zero and one
       print("Weights Error weights greater than one")
       print(c(weight1,weight2))
       print(j)
       print(subday$length[curvebin])
       print(subday$bin[curvebin])
      
     }
       if(min(c(weight1,weight2))<0){#error check. Weights should be between zero and one
       print("Weights Error less than one")
       print(c(weight1,weight2))
       print(j)
       print(subday$length[curvebin])
       print(subday$bin[curvebin])
      
     }  
        ## print("curvebin loop")
        ## print(curvebin)
        ## print(datain[curvebin,i+1])
        ## print(sum(datain[curvebin,i+1]))
        ## print(c(i,j))
        if(lengthcurvebin>=3){#if there are more than three bins in curvebin
         pointsout[subday$curve[j],i] <- sum(datain[curvebin[2:(lengthcurvebin-1)],i+1])#add up all things in middle
         pointsout[subday$curve[j],i] <- pointsout[subday$curve[j],i]+datain[curvebin[1],i+1]*(1-weight1) # add lower bin
         pointsout[subday$curve[j],i] <- pointsout[subday$curve[j],i]+datain[curvebin[lengthcurvebin],i+1]*weight2# add upper bin
         print("pointsout")
         print(pointsout)
       }else{
         if(lengthcurvebin==2){#if there are just two bins in the curvebin
           pointsout[subday$curve[j],i] <- datain[curvebin[1],i+1]*(1-weight1) # add lower bin
           pointsout[subday$curve[j],i] <- pointsout[subday$curve[j],i]+datain[curvebin[2],i+1]*weight2# add upper bin
         print("pointsout")
         print(pointsout)
         }else{
           if(lengthcurvebin==1){#if there is just one bin? Am I doing this right?
           pointsout[subday$curve[j],i] <- datain[curvebin[1],i+1]*(1-weight1) # add lower bin
         print("pointsout")
         print(pointsout)
           }else{ #this should never happen ? right?
             print("OH KNOW!!!")
           pointsout[subday$curve[j],i] <- .1
         }}
       }
     }
   }
 }

#  pointsout <- prop.table(pointsout,2)*100#normalize the columns
  ages <- vector()#compute ages for plotting
  for(i in 1:length(tzero)){#loop over curves
  tempered <- curves_cpp(Linfloc,Cloc,TW,Kloc,datain$ML,days,tzero[i],0,BIRTHDAY)
  ages[i] <- tempered$c[max(index),1]-tempered$tzero
  }

  pointsout <- pointsout[nrow(pointsout):1,]# got to reverse order so youngest fish are plotted first!
  widthvec <- (pointslower):pointsupper
  widthvec2 <-c(1:pointslower, pointsupper:nrow(pointsout))
  
x <- ages[widthvec]/365
y <- log(rowSums(pointsout)[widthvec])
z <- lm(y~x)



tempsum <- sum(rowSums(pointsout),na.rm=TRUE)
print("tempsum")
print(pointsout)  
print(rowSums(pointsout))
print(tempsum) 

#should follow what I did in Catch curve 1.
par(1,las=1,bty='n',oma=c(0,1,1,1))
#make the plots
plot(ages/365,log(rowSums(pointsout)),xlab=bquote(paste("Relative age (t-t"[o],";year)")),ylab=expression(paste("Relative abundance (ln(N))")),yaxt="n",xaxt="n")
points(ages[widthvec]/365,log(rowSums(pointsout)[widthvec]),pch=19,col="black") #make filled circles only on points in width vect
axis(2,tck=0.02,las=2)
axis(1,tck=0.02)
text(ages/365,log(rowSums(pointsout))+.1*log(max(c(log(rowSums(pointsout)),1))),as.character(length(ages):1)) #put on text
text(ages/365,log(rowSums(pointsout))-.05*log(max(c(log(rowSums(pointsout)),1))),as.character(round(rowSums(pointsout))),col="red") #put on text
lines(x=ages[widthvec2]/365,y=(z$coefficients[1]+z$coefficients[2]*ages[widthvec2]/365),col="grey")#make the line through the selected points
lines(x=ages[widthvec]/365,y=(z$coefficients[1]+z$coefficients[2]*ages[widthvec]/365),col="black")#make the line through the selected points

temp2 <-bquote(paste("ln(N) = ",.(signif(z$coefficients[1],3)),.(signif(z$coefficients[2],3)),"*age"," ; ",r^2," = ",.(signif(summary(z)$r.squared,3)),";  sum",.(signif(tempsum,3))))  
legend(x="topright",legend=temp2,inset=0.02)  


  
x11() #useful plot for debugging 
catchrqFreqPlot(1:days,datain$ML,lfdata,c(youngest,oldest,oldest+gcurve2$tzero,youngest+gcurve1$tzero),c(datain$ML[1],datain$ML[length(datain$ML)],0,0),tzero,gcurve1,gcurve2,gcurvemain,pointscurve,timeblue,datein,barscale=1,GF=0)

}








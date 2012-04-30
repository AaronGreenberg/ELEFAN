
ESP <- function(data2=peaks,asp=datafreq$asp,ML=data$ML,dm=date){ #this needs rework.
  getWinVal(scope="L");                 #reads in from gui
  countesp=0;
  ctest <- (curves(dm,Linf,c,tw,K))
  #print(length(ctest$c))
  #print(ctest$c)
  for(i in 1:length(ctest$c)){
   z=(ML-ctest$c[i])^2             #find right bin for each time
   ke=which.min(z)
   #print(ke)
                                        #choose minimum distance
   #print(data2)
   years <- length(ctest$c)/365
   #print(c(ctest$c[i],ML[1]))
   if(ctest$c[i]>ML[1]){
   
   if(data2[ke,i%%years+1] < 0){
     countesp=countesp-data2[ke,i%%years+1]
     #print(c(data2[ke,i%%years+1],i))
   }else if(data2[ke,i%%years+1] > 0){
     #print(c(data2[ke,i%%years+1],i))
     countesp=countesp+data2[ke,i%%years+1]
     data2[ke,i%%years+1]=0
   }else if(data2[ke,i%%years+1] ==0){
     countesp=countesp
   }
 }}
  print(c("ASP::",sum(asp[2:length(asp)])))
  print(c("ESP::",countesp))
  gf <- 10^(countesp/sum(asp[2:length(asp)]))/10
  print(c("GoodFit::",gf))
  return(c(esp=countesp,gf=gf))
}

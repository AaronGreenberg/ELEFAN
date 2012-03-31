
ESP <- function(data2=datab,dm=date,time,Linf,c,tw,K){ #this needs rework.

  countesp=0;
  ctest <- (curves(dm,time,Linf,c,tw,K))
  for(i in 1:length(ctest$c)){
   z=(data$ML-ctest$c[i])^2             #find right bin for each time
   ke=which.min(z)
                                        #choose minimum distance
   #print(data2)
   years <- length(ctest$c)/365
   #print(data2[ke,i]) 
   if(data2[ke,i%%years+1] < 0){
     countesp=countesp-data2[ke,i]
   }else if(data2[ke,i] > 0){ 
     countesp=countesp+data2[ke,i]
     data2[ke,i]=0
   }
 }

  return(countesp)
}

goodfit <-function(esp=explainedpeaks,asp=datafreq$asp){

  gf <- 10^(esp/sum(asp[2:length(asp)]))/10
return(gf)
}

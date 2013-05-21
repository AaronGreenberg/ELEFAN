#' Minimal doc
#' @description minimal documentation for roxygen purposes and could be added later 
#' @export
ESP <- function(data2=peaks,asp=datafreq$asp,ML=datain$ML,dm=datein){ #this needs rework.
  getWinVal(scope="L");                 #reads in from gui
  countesp=0;
  ctest <- (curves(dm,Linf,c,tw,K))
  #print(length(ctest$c))
  #print(ctest$c)
  plot(ML,data2[,1])
  for(i in 1:length(ctest$c)){
    years <- length(ctest$c)/365
    imod <- i%%years+1
   z=(ML-ctest$c[i])^2             #find right bin for each time
    #print(z)
   ke=which.min(z)#choose minimum distance
   #print(ke)
    
   if(sum(abs(data2[,imod]))==0){
     #print(c("i","imod","years","ke","data","ML"))
     #print(c(i,imod,years,ke,data2[ke,imod],ML[ke]))
     countesp=countesp
   }else {
   plot(ML,data2[,imod],type="l") 
   #print(data2)
   
   ###print(c(ctest$c[i],ML[1]))
   if(ctest$c[i]>ML[1]){
   
   if(data2[ke,imod] < 0){
     #print(c("i","imod","years","ke","data","ML"))
     #print(c(i,imod,years,ke,data2[ke,imod],ML[ke]))
     countesp=countesp+data2[ke,imod]
    }else if(data2[ke,imod] > 0){
      #print(c("i","imod","years","ke","data","ML"))
      #print(c(i,imod,years,ke,data2[ke,imod],ML[ke]))
     countesp=countesp+data2[ke,imod]
     
     j=0
    while(data2[ke+j, imod]>0){
          
          data2[ke+j,imod]=0
          j=j+1
   }
     j=0
   while(data2[ke-j, imod]>0){
     
     data2[ke-j,imod]=0
     j=j+1
   }
     data2[ke,imod]=0
     
   }else if(data2[ke,imod] ==0){
      countesp=countesp
   }
 }}
  }
  print(c("ASP::",sum(asp[2:length(asp)])))
  print(c("ESP::",countesp))
  gf <- 10^(countesp/sum(asp[2:length(asp)]))/10
  print(c("GoodFit::",gf))
  print(max(data2))
  return(c(esp=countesp,gf=gf))
}



ESPplot <- function(data2=peaks,asp=datafreq$asp,ML=datain$ML,dm=datein){ #this needs rework.
  getWinVal(scope="L");                 #reads in from gui
  
  lopper <- function(Kin){
  countesp=0;
  ctest <- (curves(dm,Linf,c,tw,Kin))
  data3=data2
  for(i in 1:length(ctest$c)){
    years <- length(ctest$c)/365
    
    if(sum(abs(data2[,imod]))==0){
     countesp=countesp}else{
   
   z=(ML-ctest$c[i])^2             #find right bin for each time
   ke=which.min(z)
   
   if(ctest$c[i]>ML[1]){
   if(data3[ke,imod] < 0){
     countesp=countesp+data3[ke,imod]
   }else if(data3[ke,imod] > 0){
     countesp=countesp+data3[ke,imod]
     j=0
     while(data3[ke+j, imod]>0){
     data3[ke+j,imod]=0
     j=j+1
   }
     j=0
     while(data3[ke-j,imod]>0){
     data3[ke-j,imod]=0
   j=j+1
   }
   }else if(data3[ke,imod] ==0){
     countesp=countesp
   }
 }}}
  gf <- 10^(countesp/sum(asp[2:length(asp)]))/10
  #print(c("GoodFit::",gf))
  return(gf=gf)
}
  

  K <- seq(0.1,1,.001)
  z <- lapply(K,lopper)
  plot(K,z,type="l")
}



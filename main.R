require(PBSmodelling);
createWin("ELEFAN.txt");
source("ASP.R")
source("ESP.R")
source("LF_plots.R")
data<-read.table("test.dat",head=TRUE)  #read in data



curves <- function(ti=1:12){#plots
  getWinVal(scope="L");
  c <- Linf*(1-exp(-K*(ti-tw)-sin(2*pi*(ti-tw))))
return(c)
}


curves2 <- function(ti=1:12,Linf,tw,K){#plots #for optimization
  
  c <- Linf*(1-exp(-K*(ti-tw)-sin(2*pi*(ti-tw))))
return(c)
}


data <-main(data)
data2 <- main2(data)
timecurves <- 1:12

plotdata <- as.data.frame(cbind(data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F))
plotdata2 <- as.data.frame(cbind(data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS))


#ESP
ESP <- function(Linf,tw,K,pdata){
  cur <- curves2(1:12,Linf,tw,K)
  for(i in 1:length(cur)){
    if
  
  }
}


plotlf <- function(da=data,pd=plotdata2){
  c1 <- curves(1:12) 
rqFreqPlot(1:12,da$ML,pd,c1)
}

plotpeak <- function(da=data,pd2=plotdata){
  c2 <- curves(1:12) 
rqFreqPlot(1:12,da$ML,pd2,c2,barcol="red")
}



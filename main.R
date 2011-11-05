require(PBSmodelling);
createWin("ELEFAN.txt");
source("ASP.R")
source("LF_plots.R")
data<-read.table("test.dat",head=TRUE)  #read in data

curves <- function(ti=1:12){#plots
  getWinVal(scope="L");
  c <- Linf*(1-exp(-K*(ti-tw)-sin(2*pi*(ti-tw))))
return(c)
}

data <-main(data) 

timecurves <- 1:12
#c1 <- curves(1:12,35,.9,.2)
plotdata <- as.data.frame(cbind(data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F,data$F))
plotdata2 <- as.data.frame(cbind(data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS,data$OBS))

plotlf <- function(da=data,pd=plotdata2){
  c1 <- curves(1:12) 
rqFreqPlot(1:12,da$ML,pd,c1)
}

plotpeak <- function(da=data,pd2=plotdata){
  c2 <- curves(1:12) 
rqFreqPlot(1:12,da$ML,pd2,c2,barcol="red")
}



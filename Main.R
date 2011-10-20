
## % Preliminary stuff
## %############################################################
## %############################################################
## %


rm(list=ls())                           #clear work space
#set.seed(999)                           #set random seed for fake data generation

## % Libraries needed
## %############################################################
## %############################################################
## %


library(stats)                          
library(quantreg)
library(plotrix)
library(Hmisc)
source("LF_plots.R")

## % Simulate fake data
## %############################################################
## %############################################################
## %

linf<-100
K<-.2

age<-1:10
growthsim<-function(linf,K,age,sample){
  growth<-NULL#matrix(age,sample+1)
      sampleage<-sample(age,sample,replace=TRUE)
  
  growth<-linf*(1-exp(-K*sampleage))*exp(rnorm(1,0,.11))
  x11()
      hist(growth)
      return(growth)
}





## % bin functions and preparing data for plotting
## %############################################################
## %############################################################
## %


bins<-c(0,10,20,30,40,50,60,70,80,90,100)

binmaker<-function(bin,vect)
  {
    #this function takes a vector of upper limits for sizes and a vector of frequencies
    #puts things into bins.
    svect<-sort(vect)                   #sort the vector
    counts<-bin*0                       #initialize
    for( i in 2:length(bin))
      {
        counts[i]<-sum(ifelse((vect<=bin[i])&(vect>=bin[i-1]),1,0))
      }
    
    
    return((counts))

    
  }


## % bin functions and preparing data for plotting
## %############################################################
## %############################################################
## %
 freqsa<-NULL
  z<-binmaker(bins,growthsim(100,.2,1:10,3000))
 freqsa<-matrix(c(z,z,z),nrow=3,ncol=length(z))
 freqs<-as.data.frame(t(freqsa))
 names(freqs)<-c("a","b0","b10")#,"b20","b30","b40","b50","b60","b70","b80","b90","b100")

years<-1:2
print(head(freqs))
rqFreqPlot(years,bins,freqs, barscale = .7, barcol = "gray90",
    boxwex = 1.50, xlim = c(0, length(years)), ylim = c(0,105), main = "",
    xlab = expression(paste(italic("t"), " (years)")),
    ylab1 = expression(paste(italic("Length Class"), " (mm)")),
    ylab2 = expression(paste(Delta, italic("D"), " (mm)")), las = 1, lty = 1)
title("Length Frequencies over several years plots")

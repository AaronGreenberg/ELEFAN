#' Minimal doc
#' @import compiler PBSmodelling animation
#' @description minimal documentation for roxygen purposes and could be added later 
#' @export

#require(compiler)
#require(PBSmodelling);                  #nice software that makes making gui easier
#library(animation)
#source("R/ASP.R")                         #load routines to compute peaks and available sum of peaks
#source("R/ESP.R")                         #load routines to compute Explained Sum of Peakz
#source("R/catchcurves.R")
#source("R/LF_plots.R")                    #load routines to make the special plots in particular rqFreqplot
#source("R/other_plots.R")                 #load routines that make other plots
#source("R/low_level.R")                   #load misc routines really mostly data proccessing     
#createWin("main_gui.txt")



## % Read in the data 
## %############################################################
## %############################################################
## %



datefilein <- function()
{
rm(datain)#remove data
rm(datein)#remove date
#this function reads in a single data file and returns datain, date in, and lfbin.
fname1 <<- selectFile()
#top <- scan(fname1,what="string",nlines=1)
lengthunits<<-strsplit(scan(fname1,what="string",nlines=1),",")[[1]]
top <- strsplit(scan(fname1,what="string",nlines=1,skip=1),",")[[1]]

print(top)
datain <- read.csv(fname1,head=TRUE,as.is=TRUE,skip=2)
colnames(datain) <- top
lfbin <<-length(datain$ML)
datein <- top
datein[1] <- NA

print(datein)
index <- 1:length(top)
datein <- as.data.frame((cbind(datein,index)))

colnames(datein)=c("Date","index")
print(datein)

print(lfbin)
datein$Date=(as.Date(datein$Date,format="%d/%m/%Y"))       #convert dates into the date class.
datelength <- length(datein$Date)                                      #get number of days data was collected

yeartemp <- as.numeric(format(datein$Date[2],"%Y")) #sort out birthday
datein$Date[1] <- as.Date(paste("01/01/",yeartemp),format="%d/%m/%Y")
BIRTHDAY <<- as.numeric(julian(as.Date(paste("01/01/",yeartemp),format="%d/%m/%y"))-julian(datein$Date[1]))
days<-as.numeric(julian(datein$Date[datelength])-julian(datein$Date[1])) #set default number of da
moddays<- (365-days%%365)+days                                         #compute width of plot window in years...
days<<-moddays
print(datein)
datein<<-datein
datain<<-datain
datein<<-datein
datain<<-datain



}



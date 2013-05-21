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

datefilein <- function(){
fname1 <<- selectFile()
datein<-read.table(fname1,head=TRUE,as.is=TRUE)  #read in datein key for the length frequency data This should probably be documented
datein$Date=(as.Date(datein$Date,format="%d/%m/%y"))       #convert dates into the date class.
datelength <- length(datein$Date)                                      #get number of days data was collected
print(datein)
yeartemp <- as.numeric(format(datein$Date[2],"%y")) #sort out birthday
#date$Date[1] <- as.Date(paste("01/01/",yeartemp),format="%d/%m/%y")
BIRTHDAY <<- as.numeric(julian(as.Date(paste("01/01/",yeartemp),format="%d/%m/%y"))-julian(datein$Date[1]))
days<-as.numeric(julian(datein$Date[datelength])-julian(datein$Date[1])) #set default number of da
moddays<- (365-days%%365)+days                                         #compute width of plot window in years...
days<<-moddays
datein<<-datein
## #print(c("BIRTHDAY_input",BIRTHDAY))
## assign("BIRTHDAY", BIRTHDAY, envir = .GlobalEnv)
## assign("date", date, envir = .GlobalEnv)
## assign("days", moddays, envir = .GlobalEnv)

}


lffilein <- function(){
fname2 <<- selectFile()
datain<<-read.table(fname2,head=TRUE,as.is=TRUE)  #read in date key for the length frequency data This should probably be documented
lfbin<<-length(data$ML)
## assign("data", data, envir = .GlobalEnv)
## assign("lfbin",length(data$ML),envir=.GlobalEnv)                   #get number of bins
}



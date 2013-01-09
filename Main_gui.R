library(gWidgetsRGtk2)
options("guiToolkit"="RGtk2")


updatePlot = function(h, ...) {#make the first plot
  x = do.call("rnorm", list(svalue(sampleSize),svalue(mean),svalue(var)))
  visible(histgraphic) <- TRUE #make correct picture 
  par(mar=c(2.0,2.0,2.0,2.0))
  hist(x,freq=FALSE,main="density test",svalue(numbin))
  lines(density(x, adjust = svalue(bandwidthAdjust)),col="blue")
  rug(x)
}

updatePlot2 = function(h, ...) {#make second sort of plot
  x <- 1:100
  visible(linegraphic) <- TRUE #make correct picture 
  y <- svalue(intercept)+svalue(slope)*x+rnorm(length(x),0,10)
  par(mar=c(2.1,2.1,2.1,2.1))
  plot(x,y,type="p",col="red")
  abline(lm(y~x),col="blue")
}

mean = gslider(from = 0, to = 20, by = 0.01, value = 1)
var = gslider(from=1,to=10,by=1,value=5)

slope = gslider(from = 0, to = 20, by = 0.01, value = 1)
intercept = gslider(from=1,to=10,by=1,value=5)

sampleSize = gslider(from=1,to=10^4,by=1,value=250)
bandwidthAdjust = gslider(from=1,to=10,by=.1,value=2)
numbin=gradio(c(10,100,1000,2000))
plot1=gbutton("Plot hist",handler=updatePlot)
plot2=gbutton("Plot lm",handler=updatePlot2)
size=1000
window = gwindow("New ELEFAN",height=size,width=1.618*size)
biggroup <- ggroup(container=window,expand=TRUE,horizontal=FALSE,visible=TRUE)

#make big note book!
nb <- gnotebook(container=biggroup,expand=TRUE,horizontal=TRUE,visible=TRUE)

#Make the Entry page!

Entry <- ggroup(container = nb,label="LF", expand=TRUE,horizontal=TRUE,visible=TRUE)#make entry group
Entrylittle <- ggroup(container=Entry,expand=TRUE,horizontal=FALSE,visible=TRUE)# make little entry group
Entrypic<- gnotebook(container=Entry,expand=TRUE,visible=FALSE)
Entrygraphic<- ggraphics(container = Entrypic,width=500,height=500,label="Main Page")
graphHandlerEntry <- addHandlerChanged(nb,function(h,...){
  print("Entrygraphic")

})

EntryPlot = function(h, ...) {#make the first plot
  visible(histgraphic) <- TRUE #make correct picture 
}

gimage("ubc.gif", dirname=getwd(), size=c(.01,.01), container=histgraphic)



## LF <- ggroup(container = nb,label="LF", expand=TRUE,horizontal=TRUE,visible=TRUE)
## lflittle <- ggroup(container=LF,expand=TRUE,horizontal=FALSE,visible=TRUE)


## gimage("ubc.gif", dirname=getwd(), size=c(.01,.01), container=lflittle)
## Line <- ggroup(container = nb,label="Line", expand=TRUE,horizontal=TRUE,visible=TRUE)
## linelittle <- ggroup(container=Line,expand=TRUE,horizontal=FALSE,visible=TRUE)
## gimage("ubc.gif", dirname=getwd(), size=c(.01,.01), container=linelittle)
## Linepic<- gnotebook(container=Line,expand=TRUE,visible=FALSE)
## LFpic<- gnotebook(container=LF,expand=TRUE,visible=FALSE)

## histgraphic<- ggraphics(container = LFpic,width=500,height=500,label="LF")
## linegraphic<- ggraphics(container = Linepic,width=500,height=500,label="Line Pic")
## graphHandler1 <- addHandlerChanged(nb,function(h,...){
##   print("linegraphic")
## #  visible(linegraphic) <- TRUE #make correct picture
## #  updatePlot2()
##   })

## graphHandler2 <- addHandlerChanged(nb,function(h,...){
##   print("histgraphic")
## #  visible(histgraphic) <- TRUE #make correct picture 
## #  updatePlot()
##   })

## #gimage("ubc.gif", dirname="/home/daft/ELEFAN", size=c(.1,.1), container=LFpic)

## tmp = gframe("Sample size", container = lflittle)
## add(tmp, sampleSize,expand=TRUE)

## tmp = gframe("Mean", container = lflittle)
## add(tmp, mean,expand=TRUE)

## tmp = gframe("Var", container = lflittle)
## add(tmp, var,expand=TRUE)


## tmp = gframe("Bandwidth adjust", container = lflittle)
## add(tmp, bandwidthAdjust, expand = TRUE)
## tmp = gframe("Numbin", container = lflittle)
## add(tmp, numbin, expand = TRUE)
## tmp = gframe("Plot LF", container = lflittle,handler=graphHandler1)
## add(tmp, plot1, expand = TRUE)


## tmp = gframe("intecept", container = linelittle)
## add(tmp, intercept,expand=TRUE)

## tmp = gframe("slope", container = linelittle)
## add(tmp, slope,expand=TRUE)

## tmp = gframe("Plot Lm", container = linelittle,handler=graphHandler2)
## add(tmp,plot2, expand = TRUE)



`Plot2` <- function(func, Myhscale, Myvscale, title){

x <- func
winPlot <- tktoplevel()
tkwm.title(winPlot , title)
buttonFramePlot <- tkframe(winPlot, borderwidth=2)

scr <- tkscrollbar(winPlot, repeatinterval=1, command=function(...) tkyview(tl,...))
tl <-tklistbox(winPlot, height=2, selectmode="single", yscrollcommand=function(...) tkset(scr,...), background="white")

type <- c("ps","png","tiff","bmp","jpeg")

for (i in (1:length(type))){
  tkinsert(tl,"end",type [i])
}
tkselection.set(tl,0)

img <- tkrplot(winPlot ,fun=x,hscale=Myhscale,vscale=Myvscale)

CopyToClip <- function(){
 tkrreplot(img)
 tkmessageBox(message="The graph have been copied into clipboard",icon="info",type="ok")
}

SaveGraph <- function(){
fileName<- tclvalue(tkgetSaveFile())
if (!nchar(fileName)){
    tkmessageBox(message="No graph to copy!")
  } else { 
    fileTypeChoice <- type[as.numeric(tkcurselection(tl))+1]
    split <- strsplit(fileName,".",fixed=TRUE)
    fileName <- paste(split[[1]][1],".",fileTypeChoice ,sep="")
    if(fileTypeChoice == "ps") fileTypeChoice <- "postscript"
    eval(parse(text=paste(fileTypeChoice ,"(file=fileName)",sep="")))
    x()
    dev.off()
  }
}


copy.but <- tkbutton(buttonFramePlot, text="Copy to Clipboard", command=CopyToClip)
save.but <- tkbutton(buttonFramePlot, text="Browse", command=SaveGraph)

tkgrid(img)
tkgrid(copy.but, tklabel(buttonFramePlot, text="Save image as:"), tl, scr, save.but)

tkgrid(buttonFramePlot)

}


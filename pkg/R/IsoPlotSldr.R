`IsoPlotSldr` <-
function() {

if (!exists("SAMRes"))  
   {  
        tkmessageBox(title="Error Message",message="The SAM statistics have not calculated yet!",icon="error",type="ok")
     } 
   else {


ChooseStat ()
statSAMplot <<- statUsed
pos <<-1
winPlot <- tktoplevel()
tkwm.title(winPlot ,"SAM Analysis" )
tkwm.geometry(winPlot , "+10+10")
sliderFramePlot <-tkframe(winPlot ,borderwidth=2)
sliderFramePlot1 <-tkframe(sliderFramePlot ,borderwidth=2,relief="groove")
sliderFramePlot2 <-tkframe(sliderFramePlot ,borderwidth=2,relief="groove")

buttonFramePlot <-tkframe(winPlot ,borderwidth=2)
showbutFrame <-tkframe(winPlot ,borderwidth=2)

AllFDR <<- Isoallfdr(SAMRes, , stat=statSAMplot )
Delta.table <<- data.frame(AllFDR )
delta1 <- Delta.table [1,1]

labelText <<- tclVar(paste("Delta value:",delta1 ))

SliderValue <- tclVar("1" ) 

SliderValueLabel <- tklabel(sliderFramePlot1 ,text=labelText )
tkconfigure(SliderValueLabel,textvariable=labelText )


slider <- tkscale(sliderFramePlot1 , from=1, to=nrow(Delta.table),
                   showvalue=F, variable=SliderValue,
                   resolution=1, orient="horizontal",length=400)

slidertxt <- tklabel(sliderFramePlot1 ,text="Delta Slider:" )
tkgrid(slidertxt )
tkgrid.configure(slidertxt ,sticky="w")

tkgrid(SliderValueLabel,tklabel(sliderFramePlot1 ,text="" ))

inputtxt <- tklabel(sliderFramePlot2 ,text="Input delta/FDR:" )
tkgrid(inputtxt )
tkgrid.configure(inputtxt ,sticky="w")


      rb1 <- tkradiobutton( sliderFramePlot2 )
      rb2 <- tkradiobutton( sliderFramePlot2 )
      rbValue1 <- tclVar("delta")
      tkconfigure(rb1,variable=rbValue1,value="delta",text="Delta:")
 tkconfigure(rb2,variable=rbValue1,value="fdr", text="FDR:")

deltaval <- tclVar("  ")
deltatxt<-tkentry(sliderFramePlot2 ,width="5",textvariable=deltaval )
tkgrid.configure(rb1 ,deltatxt,sticky="w")
tkgrid(rb1 , deltatxt)

FDR <- tclVar("0.05")
FDRtxt <-tkentry(sliderFramePlot2 ,width="5",textvariable=FDR )
tkgrid.configure(rb2 ,FDRtxt  ,sticky="w")

inputDelta <- function() {
  rbVal1 <- as.character(tclvalue(rbValue1))
  
  if (rbVal1 == "delta") {
         Deltaval <- as.numeric(tclvalue(deltaval ))
    pos1 <- match(Deltaval,Delta.table[,1])
    if (is.na(pos1)) { 
  tkmessageBox(message="The delta value is not valid! Check the delta table!",icon="error",type="ok")
  }
    else { 
  pos <<- pos1 
 delta.now <<- Deltaval
 }
    }
        else {
			FDRval <- as.numeric(tclvalue(FDR))
			if (FDRval < min(Delta.table [, 5])) { 
				tkmessageBox(message="The FDR value is not valid: too small! Check the delta table!",icon="error",type="ok")
			} else {
				Deltaval <<- min(na.exclude(Delta.table [Delta.table [, 5] <= FDRval , 1]))
				delta.now <<- Deltaval 
				pos <<- match(Deltaval,Delta.table[,1])
			}
			img <- tkrreplot(img,fun=samplot2 , 1.5,1.5)
				
			}   

}

input.but <- tkbutton(sliderFramePlot2 ,text="Display",command=inputDelta )

tkgrid(rb2 , FDRtxt, input.but   )


showSigSAM <- function() {

      ResSAM <- get.ResSAM ()
siggeneSAM <<-ResSAM [[2]]
showData(siggeneSAM,title= paste("List of significant genes - ",statSAMplot, "-SAM ", sep="") )
}


savegene <- function() {
     ResSAM <- get.ResSAM ()
siggeneSAM <<-ResSAM [[2]]
save.result("siggeneSAM" )
}

saveAllgene <- function() {
     ResSAM <- get.ResSAM ()
AllgeneSAM<<-ResSAM [[1]]
save.result("AllgeneSAM" )
}

showDelta <- function() {
   showData(Delta.table,title="Delta table")
}

savedeltaTab<- function() {
save.result("Delta.table")
}


siggene.but <- tkbutton(showbutFrame ,text="Display significant genes",command=showSigSAM )
savegene.but <- tkbutton(showbutFrame ,text="Save significant genes",command=savegene)
saveAllgene.but <- tkbutton(showbutFrame ,text="Save all genes results",command=saveAllgene )

savedeltaTab.but <- tkbutton(showbutFrame ,text="Save Delta table",command=savedeltaTab)


delta.but <- tkbutton(showbutFrame ,text="Display delta table",command=showDelta )

tkgrid(siggene.but,tklabel(showbutFrame ,text="  "),savegene.but,tklabel(showbutFrame ,text="  "),
    saveAllgene.but, tklabel(showbutFrame ,text="  "), delta.but,tklabel(showbutFrame ,text="  "),savedeltaTab.but)


scr <- tkscrollbar(buttonFramePlot , repeatinterval=1, command=function(...)tkyview(tl,...))
tl <-tklistbox(buttonFramePlot ,height=2,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")

type <- c("ps","png","tiff","bmp","jpeg")

for (i in (1:length(type )))
{
    tkinsert(tl,"end",type [i])
}
tkselection.set(tl,0)


plotFrame <- tkframe(winPlot ,relief="groove",borderwidth=2)

img <- tkrplot(plotFrame,fun=samplot2 ,1.5,1.5)
tkgrid(img)


draw <- function(...){

pos <<- as.numeric(tclvalue(SliderValue))

img <- tkrreplot(img,fun=samplot2 , 1.5,1.5)

delta.now <<- Delta.table[pos,1]
tclvalue(labelText) <<- paste("Delta value:",delta.now)

}



CopyToClip <- function()
{
 ReturnVal <- tkmessageBox(message="The graph have been copied into clipboard",icon="info",type="ok")
 if (tclvalue(ReturnVal)== "ok") tkfocus(winPlot )
}


SaveGraph <- function()
{
fileName<- tclvalue(tkgetSaveFile())
if (!nchar(fileName))
    tkmessageBox(message="No graph to copy!")
else
{ 
  fileTypeChoice <- type[as.numeric(tkcurselection(tl))+1]
            split <- strsplit(fileName,".",fixed=TRUE)
            fileName <- paste(split[[1]][1],".",fileTypeChoice ,sep="")
            if(fileTypeChoice == "ps") fileTypeChoice <- "postscript"
            eval(parse(text=paste(fileTypeChoice ,"(file=fileName)",sep="")))
samplot2 ()
            dev.off()
       }
}


copy.but <- tkbutton(buttonFramePlot,text="Copy the graph to Clipboard",command=CopyToClip)
save.but <- tkbutton(buttonFramePlot,text="Browse",command=SaveGraph)


tkconfigure (slider, command = draw )
tkgrid(img)
tkgrid(slider)


onExit <- function()
  {
    ReturnVal <<- 0
      tkgrab.release(winPlot )
tkdestroy(winPlot )
       
 }


tkgrid(tklabel(buttonFramePlot,text="                             "))
Exit.but <-tkbutton(buttonFramePlot,text=" Exit SAM analysis ",command=onExit )
tkgrid(copy.but,tklabel(buttonFramePlot,text="save the graph as:"),tl,scr,save.but,tklabel(buttonFramePlot,text="     "),Exit.but)

tkgrid(plotFrame) 
tkgrid(sliderFramePlot)
tkgrid(sliderFramePlot1,  sliderFramePlot2)
tkgrid(showbutFrame )
tkgrid(buttonFramePlot)

}
}


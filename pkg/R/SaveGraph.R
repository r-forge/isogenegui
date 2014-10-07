`SaveGraph` <-
function()
{
fileName<- tclvalue(tkgetSaveFile(initialfile="isoplot.ps",filetypes="{{Postscript} {.ps}} {{JPEG Files} {.jpg .jpeg}} {{All files} *}"))
if (!nchar(fileName))
    tkmessageBox(message="No graph to copy!")
else
{ 
splitted <- strsplit(fileName,".",fixed=TRUE)
if(length(unlist(splitted)) == 1)
{
fileName <- paste(fileName,".ps")
postscript(file=fileName)
x()
dev.off()}

if(unlist(splitted)[2] == "ps")
{
postscript(file=fileName)
x()
dev.off()

}

if(unlist(splitted)[2] == "jpeg")
{
jpeg(filename=fileName)
x()
dev.off()
}
if(unlist(splitted)[2] == "png")
{
png(filename=fileName)
x()
dev.off()
}
       }



copy.but <- tkbutton(buttonFramePlot,text="Copy to Clipboard",command=CopyToClip)
save.but <- tkbutton(buttonFramePlot,text="Save as....",command=SaveGraph)

tkgrid(img)
tkgrid(copy.but,save.but )
tkgrid(buttonFramePlot)

}


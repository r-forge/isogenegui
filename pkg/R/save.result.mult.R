`save.result.mult` <-
function(objt) {
x <- objt
saveWin <- tktoplevel()
tkwm.title(saveWin ,"Save the results" )
frame1 <- tkframe(saveWin , borderwidth=2)
frame2 <- tkframe(saveWin , borderwidth=2)
But.frame <- tkframe(saveWin , borderwidth=2)

 labelSave<- tklabel(saveWin ,text="Save the list of significant genes: ")
 labelSaveXls<- tklabel(frame1,text="Excel files into folder: ")
 labelSaveR<- tklabel(frame2,  text="R dataset                : ")

cb1 <- tkcheckbutton(frame1)
cbValue1 <- tclVar("1")
tkconfigure(cb1,variable=cbValue1,text="Excel files")

cb2 <- tkcheckbutton(frame2)
cbValue2 <- tclVar("0")
tkconfigure(cb2,variable=cbValue2,text="R dataset")

 SaveDirName<<- tclVar("")
 SaveDir.but <-tkbutton(frame1,text="browse",command=saveDir )
 SaveDirEdit <-tkentry(frame1,width="40",textvariable=SaveDirName )

Save.but <-tkbutton(frame2,text="Browse",command=savingRwd )
      SavefileName <<- tclVar("")
SavefileEdit <-tkentry(frame2,width="40",textvariable=SavefileName)

tkgrid.configure(labelSave,sticky="w")

tkgrid(labelSave)

tkgrid(cb1 ,SaveDirEdit ,SaveDir.but )
tkgrid(cb2 ,SavefileEdit ,Save.but  )

tkgrid.configure(frame1,sticky="w")
tkgrid.configure(frame2,sticky="w")


saveSigGenes <- function () {

cbVal1 <- as.character(tclvalue(cbValue1))
cbVal2 <- as.character(tclvalue(cbValue2))

if (cbVal1 =="0" & cbVal2 =="0") {
  tkmessageBox(title="Error..!!",message="No saving option was selected!",icon="error",type="ok")
 }
     else {

  if (cbVal1 =="1") { 
       for (i in 1:length(x)) {
      xlsfilename <- paste(DirName,"/",x[i],".xls",sep="")
      WriteXLS(x , ExcelFileName = xlsfilename )

 }   
   }
  if (cbVal2 =="1") { 
     try(save(list = x, file =fileRwd))

   }
  
  tkmessageBox(title="Info",message="The objects have been saved!",icon="info",type="ok")
  tkdestroy(saveWin )
       }
}

cancel.ok <- function () 
{
tkdestroy(saveWin )
} 

saveSigGene.but <-tkbutton(But.frame ,text=" Save",command=saveSigGenes)
cancel.but <-tkbutton(But.frame ,text=" Cancel",command=cancel.ok)

tkgrid(saveSigGene.but,cancel.but)
tkgrid(frame1)
  tkgrid.configure(frame1,sticky="w")
  
tkgrid(frame2)
  tkgrid.configure(frame2,sticky="w")
      tkgrid(But.frame )

}


`save.result` <-
function(object.res) {
x <- object.res
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

 fileXls <<- tclVar("")
 saveXls.but <-tkbutton(frame1,text="browse",command=saveXls)
 saveXlsEdit <-tkentry(frame1,width="40",textvariable=fileXls)

Save.but <-tkbutton(frame2,text="Browse",command=savingRwd )
      SavefileName <<- tclVar("")
SavefileEdit <-tkentry(frame2,width="40",textvariable=SavefileName)

tkgrid.configure(labelSave,sticky="w")

tkgrid(labelSave)

tkgrid(cb1 ,saveXlsEdit , saveXls.but )
tkgrid(cb2 ,SavefileEdit ,Save.but  )

tkgrid.configure(frame1,sticky="w")
tkgrid.configure(frame2,sticky="w")

saveSigGene <- function () {

cbVal1 <- as.character(tclvalue(cbValue1))
cbVal2 <- as.character(tclvalue(cbValue2))

if (cbVal1 =="0" & cbVal2 =="0") {
  tkmessageBox(title="Error..!!",message="No saving option was selected!",icon="error",type="ok")
 }
     else {

  if (cbVal1 =="1") { 
     #eval(parse(text=paste("write.xls(",x, ",file =fileXls2 )",sep="")))
     #eval(parse(text=paste("WriteXLS(",x, ",ExcelFileName =fileXls2 )",sep="")))
     WriteXLS(x, ExcelFileName =fileXls2 )

   }
  if (cbVal2 =="1") { 
     eval(parse(text=paste("save(",x, ",file =fileRwd)",sep="")))
   }
  
  tkmessageBox(title="Info",message="The objects have been saved!",icon="info",type="ok")
  tkdestroy(saveWin )
       }
}

cancel.ok <- function () 
{
tkdestroy(saveWin )
} 

saveSigGene.but <-tkbutton(But.frame ,text=" Save",command=saveSigGene)
cancel.but <-tkbutton(But.frame ,text=" Cancel",command=cancel.ok)

tkgrid(saveSigGene.but,cancel.but)
tkgrid(frame1)
  tkgrid.configure(frame1,sticky="w")
  
tkgrid(frame2)
  tkgrid.configure(frame2,sticky="w")
      tkgrid(But.frame )

}


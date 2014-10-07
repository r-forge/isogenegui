`save.result.ORICC` <-
function(object.res) {
x <- object.res
      saveWin <- tktoplevel()
      tkwm.title(saveWin ,"Save the results" )
frame2 <- tkframe(saveWin , borderwidth=2)
But.frame <- tkframe(saveWin , borderwidth=2)

 labelSave<- tklabel(saveWin, text="Save the resulting clusters as R dataset: ")
 labelSaveR<- tklabel(frame2,  text="R dataset                : ")


Save.but <- tkbutton(frame2, text="Browse", command=savingRwd)
SavefileName <<- tclVar("")
SavefileEdit <- tkentry(frame2, width="40", textvariable=SavefileName)


tkgrid.configure(labelSave,sticky="w")

tkgrid(labelSave)

tkgrid(SavefileEdit, Save.but)

tkgrid.configure(frame2, sticky="w")

saveSigGene <- function () {

  save(x, file =fileRwd)
  
  tkmessageBox(title="Info",message="The objects have been saved!",icon="info",type="ok")
  tkdestroy(saveWin)
       }


cancel.ok <- function () 
{
tkdestroy(saveWin )
} 

saveSigGene.but <-tkbutton(But.frame, text=" Save", command=saveSigGene)
cancel.but <-tkbutton(But.frame, text=" Cancel", command=cancel.ok)

tkgrid(saveSigGene.but,cancel.but)

tkgrid(frame2)
  tkgrid.configure(frame2,sticky="w")
      tkgrid(But.frame )

	  #tkfocus(saveWin)
}


`EditData` <-
function() {
Edat<-tktoplevel()
tkwm.deiconify(Edat)
tkwm.title(Edat,"Edit Data") 
buttonFrame <-tkframe(Edat,borderwidth=2)

tkgrab.set(Edat)
tkfocus(Edat)
scr <- tkscrollbar(Edat, repeatinterval=5, command=function(...)tkyview(tl1,...))

#####
tl1<-tklistbox(Edat,height=4,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
tkgrid(tklabel(Edat,text="Choose the object you want to edit: "))
tkgrid(tl1,scr)
tkgrid.configure(scr,rowspan=4,sticky="nsw")
for (i in (1:length(dataSet)))
{
    tkinsert(tl1,"end",dataSet[i])
}
tkselection.set(tl1,2)  

OnOK <- function()

{
    loc <- as.numeric(tkcurselection(tl1))+1
if (loc ==1) {     
tkdestroy(Edat)
     fix(dose)
}
 else {
    tkdestroy(Edat)
    fix(exprs )
    }




}

onCancel <- function()
  {
    ReturnVal <<- 0
      tkgrab.release(Edat)
tkdestroy(Edat)
tkwm.deiconify(tt ) 
       tkfocus(tt )
 }

OK.but <-tkbutton(buttonFrame,text="   OK   ",command=OnOK)
Cancel.but <-tkbutton(buttonFrame,text=" Cancel ",command=onCancel)

tkgrid(OK.but,Cancel.but)
tkgrid(buttonFrame)

tkfocus(Edat)

}


`showdata` <-
function() {

if (!exists("exprs") | !exists("dose"))  
   {  
      ReturnVal <- tkmessageBox(title="Error Message",message="Load the data first!",icon="error",type="ok")
if (tclvalue(ReturnVal)== "ok") tkfocus(tt)
} 
else {

shdat<-tktoplevel()
tkwm.deiconify(shdat)
tkwm.title(shdat,"Show Data") 
buttonFrame <-tkframe(shdat,borderwidth=2)

tkgrab.set(shdat)
tkfocus(shdat)
scr <- tkscrollbar(shdat, repeatinterval=5, command=function(...)tkyview(tl1,...))

#####
tl1<-tklistbox(shdat,height=4,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
tkgrid(tklabel(shdat,text="Choose the object you want to see: "))
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
tkdestroy(shdat)
     showData(dose)}
  else {
     tkdestroy(shdat)
     showData(exprs)
     }
}

onCancel <- function()
  {
    ReturnVal <<- 0
      tkgrab.release(shdat)
tkdestroy(shdat)
tkwm.deiconify(tt ) 
       tkfocus(tt )
 }

OK.but <-tkbutton(buttonFrame,text="   OK   ",command=OnOK)
Cancel.but <-tkbutton(buttonFrame,text=" Cancel ",command=onCancel)

tkgrid(OK.but,Cancel.but)
tkgrid(buttonFrame)

tkfocus(shdat)
     }
}


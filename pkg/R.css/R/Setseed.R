`Setseed` <-
function() {
seed <-tktoplevel()
tkwm.deiconify(seed)
tkwm.title(seed ,"Set random seed number") 
tkgrab.set(seed)
tkfocus(seed)
Name <- tclVar("1234")
entry.Name <-tkentry(seed ,width="10",textvariable=Name)
tkgrid(tklabel(seed ,text="Enter the random seed number"))
tkgrid(entry.Name)
OnOK <- function()
{
SeedNum <<- as.numeric(tclvalue(Name))
    assign("SeedNum",SeedNum, envir=.GlobalEnv)
tkdestroy(seed )
tkfocus(tt )
}
OK.but <-tkbutton(seed ,text="   OK   ",command=OnOK)
tkbind(entry.Name, "<Return>",OnOK)
tkgrid(OK.but)
tkfocus(seed )
}


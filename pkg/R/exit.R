`exit` <-
function()
{
QuitMsg <- tkmessageBox(message="Are you sure want to quit?",icon="question",type="yesnocancel",default="yes")
  if ( tclvalue(QuitMsg )== "yes") {
tkdestroy(tt)
     }
  else {tkfocus(tt)}
}


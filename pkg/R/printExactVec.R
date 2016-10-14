`printExactVec` <-
function(FDR,result)
{
  
infoBox  <- tktoplevel()
tkwm.title(infoBox,"Global Likelihood Ratio Test (E2)") 

       xscr <- tkscrollbar(infoBox, repeatinterval=5,orient="horizontal",
                       command=function(...)tkxview(txt,...))
       yscr <- tkscrollbar(infoBox, repeatinterval=5,
                       command=function(...)tkyview(txt,...))

      txt <- tktext(infoBox,bg="white",font="courier", xscrollcommand=function(...)tkset(xscr,...),yscrollcommand=function(...)tkset(yscr,...),
       wrap="none")
tkgrid(txt,yscr)
      tkgrid(xscr)
      tkgrid.configure(yscr,sticky="ns")
      tkgrid.configure(xscr,sticky="ew")
       
      tkinsert(txt,"end","                               \n")
tkinsert(txt,"end", paste("Results for E2 using exact distribution p-value \n"))
tkinsert(txt,"end","                               \n")
     # tkinsert(txt,"end", paste("Number of Genes analyzed  :", num ," \n"))

tkinsert(txt,"end", paste("Direction :", result[,2] ," \n"))

      tkinsert(txt,"end", paste("Overall significant level :", FDR ," \n"))
tkinsert(txt,"end","                         \n")

  tkinsert(txt,"end", paste("Calculated P-value:", result[,4]," \n"))

 # tkinsert(txt,"end", paste("Number Significant Genes  :", result[,5]," \n"))


     
      tkconfigure(txt, state="disabled")
tkfocus(txt)
tkwm.deiconify(infoBox  )
tkfocus(infoBox  )
}


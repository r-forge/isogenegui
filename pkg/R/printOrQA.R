`printOrQA` <-
function(num,FDR,result)
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
tkinsert(txt,"end", paste("Results for E2 using permutation p-value (orQA) \n"))
tkinsert(txt,"end","                               \n")
      tkinsert(txt,"end", paste("Number of Genes analyzed  :", num ," \n"))
      tkinsert(txt,"end", paste("Overall significant level :", FDR ," \n"))
tkinsert(txt,"end","                         \n")
for (i in (1:nrow(result)))
{
      tkinsert(txt,"end", paste("P-value adjustment:", result[i,1]," \n"))
      tkinsert(txt,"end", paste("Number Significant Genes  :", result[i,2]," \n"))
}

     
      tkconfigure(txt, state="disabled")
tkfocus(txt)
tkwm.deiconify(infoBox  )
tkfocus(infoBox  )
}


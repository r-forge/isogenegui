`printsummary` <-
function(x,y,range)
{
  
     z <- range
infoBox  <- tktoplevel()
tkwm.title(infoBox,"Summary Statistics for gene") 
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
      summary<-NULL
      for (i in (1:length(z)))
{
      rownum <- z[i]
      summary <- round (summ (x ,y[rownum, ]),4)
      
      digit<- diginum (summary[,1])

   dosename <- format (summary[,1] , digits=digit) 
      summary <- format (summary , digits=4) 

      name <- rownames(y[rownum, ])

      stat <- IsoGene1(x,as.matrix(y[rownum, ]))
stat.all <- round(as.vector(unlist(stat [-11])),2)
      stat.all2 <- format (stat.all , digits=2) 

      tkinsert(txt,"end","                               \n")
tkinsert(txt,"end", paste("Summary Statistics for gene:",name ," \n"))
tkinsert(txt,"end","                               \n")
tkinsert(txt,"end","Dose     N  Observed Mean     S.e      IsoMean Up  IsoMean Down \n")

      for (i in (1:nrow(summary  ))) tkinsert(txt,"insert",paste(dosename[i] ,summary [i,2],summary [i,3],summary [i,4],summary [i,5],summary [i,6],"\n",sep="       "))
tkinsert(txt,"end","                               \n")

tkinsert(txt,"end", paste("E2 Up        :", stat.all2[1], " | ",  "E2 Down    :", stat.all2[6],  " \n"))

tkinsert(txt,"end","                               \n")
      
}

      tkconfigure(txt, state="disabled")
tkfocus(txt)
}


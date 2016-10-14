`ExactE2Vec` <-
  function() 
    
    ## Asymp in this function corresponds to exact pvalues ##
  {
    
    if (!exists("exprs") | !exists("dose"))  {  
      ReturnVal <- tkmessageBox(title="Error Message",message="Load the data first!",icon="error",type="ok")
      if (tclvalue(ReturnVal)== "ok") tkfocus(tt)
    } else {
      cat(class(exprs))
      assign("test",class(exprs) ,envir=.GlobalEnv)

  
        #print("hahaha")
        
        asym<-tktoplevel()
        tkwm.title(asym,"Global Likelihood Ratio Test (E2) Analysis: Asymptotic") 
        
        spec.frm <- tkframe(asym,borderwidth=2)
        frame1 <- tkframe(spec.frm, relief="groove", borderwidth=2)
        buttonFrameasym <-tkframe(asym,borderwidth=2)
        
        ### Select genes ##
        rb1 <- tkradiobutton( frame1 )
        #rb2 <- tkradiobutton( frame1 )
        
        rbValue1 <- tclVar("all")
        
        tkconfigure(rb1,variable=rbValue1,value="all",text="Single Dimensional Data")
        #tkconfigure(rb2,variable=rbValue1,value="range", text="Genes Range")
        
        #fromgene<- tclVar("")
        #FromNum<-tkentry(frame1 ,width="7",textvariable=fromgene)
        
        #togene <- tclVar("")
        #ToNum<-tkentry(frame1 ,width="7",textvariable=togene )
      
      FDR <- tclVar("0.05")
      FDRtxt <-tkentry(frame1 ,width="5",textvariable=FDR)
      tkgrid(tklabel(frame1,text="Overall Significant Level: "),FDRtxt)
      
        
        
        Calc.Asymp <- function ()
        {
          if (!exists("exprs")&!exists("dose"))  
          {  
            ReturnVal <- tkmessageBox(title="Error Message",message="No data set loaded, please load the dataset first!",icon="info",type="ok")
            if (tclvalue(ReturnVal)== "ok") tkfocus(asym)
          } else {
            
            tkconfigure(asym,cursor="watch")
            rbVal1 <- as.character(tclvalue(rbValue1))
            
            if (rbVal1=="all") {
              if(is.matrix(exprs) == TRUE){
                
                exprs2Asymp <<- exprs 
              } else {
                
                exprs_mat <- rbind(exprs,exprs)
                temp1 <- exprs_mat[1,,drop=FALSE]
                exprs2Asymp <<- unname(temp1)
                
              }
              
              matchID  <- c(1:nrow(exprs2Asymp ))
            }
            
            assign("matchIDAsymp",matchID  ,envir=.GlobalEnv)
            
            # foldchange ##
            res.Asymp <<-ExactE2Val(dose,exprs2Asymp)
            pval.Asymp  <<- res.Asymp[,2]
            #ProbeID <<- 1
            Mu.Diff.Asymp <<-  MeanDiff (dose,exprs2Asymp)
            E2Val.Asymp <<- res.Asymp[,1]
            N <- 1 #length(ProbeID)
            
            E2Val <- E2Val.Asymp
            FDRval <<- as.numeric(tclvalue(FDR))
            Direction  <- res.Asymp[,3]
            
            Mu.Diff <-  Mu.Diff.Asymp
            row.num <- 1:nrow(exprs2Asymp)
            Res.Asymtot <<- data.frame(Mu.Diff,Direction, E2Val, pval.Asymp)
            rownames(Res.Asymtot) <- NULL
            
            sig <- ifelse(Res.Asymtot[,4] < FDRval, 1, 0)
            result2 <- data.frame(Res.Asymtot,sig)
                        
            ReturnVal <- tkmessageBox(message="The calculation of the p-values have been finished",icon="info",type="ok")
            if (tclvalue(ReturnVal)== "ok") tkfocus(asym)
            tkconfigure(asym,cursor="arrow")
            
            try(tkdelete(treeWidget,"nodeAsymp") , silent = T )
            try( tkdelete(treeWidget,"AsymPvalNode"), silent = T )
            
            printExactVec(FDRval,result2)
            
            tkinsert(treeWidget,"end","Record2Node","AsymPvalNode",text="Raw P-values")
            tkinsert(treeWidget,"end","Record2Node","AdjPvalNode",text= paste("Adjusted P-values, FDR: ", FDRval)  )
            tkinsert(treeWidget,"end","AsymPvalNode","NumGeneNode",text=paste("Number gene analyzed: ",N))
            
          }
        }
      
      
#       
#       savesiggene <- function() {
#         save.result(SigGenes.AsymptotE2)
#       }
#       savesiggene.but <- tkbutton(frame1,text="Save Results", command=savesiggene)
#   
      saveallgene <- function() {
        #Pval.AllGenes.Asymp <<- Res.Asymtot
        #save.result("Pval.AllGenes.Asymp")
        save.result(Res.Asymtot)
      }
      saveallgene.but <- tkbutton(frame1,text="Save results",command=saveallgene )
      
      
      
      
        
        Calculate.but <-tkbutton(frame1,text=" Calculate ",command=Calc.Asymp)
        
        lab1 <- tklabel(frame1,text="Calculate the p-values for E2 ")
        #lab2 <- tklabel(frame1,text="Select the genes: ")





        
        tkgrid.configure(lab1,sticky="w")
        #tkgrid.configure(lab2,sticky="w")
        
        tkgrid(lab1)
        #tkgrid(lab2)
        
        tkgrid.configure(rb1,sticky="w")
        tkgrid(rb1)
#         tkgrid(rb2,tklabel(frame1 ,text="From"),FromNum,tklabel(frame1 ,text="To"),ToNum,
#                tklabel(frame1,text="                        "),Calculate.but)
#         tkgrid.configure(rb2,sticky="w")
        
      tkgrid.configure(Calculate.but,saveallgene.but, sticky="w")

        
        tkgrid(frame1)
        
        tkgrid.configure(frame1,sticky="nsw")
        
        onExit <- function()
        {
          ReturnVal <<- 0
          tkdestroy(asym)
        }
        
        
        tkgrid(tklabel(buttonFrameasym ,text="                             "))
        Exit.but <-tkbutton(buttonFrameasym ,text=" Exit the analysis ",command=onExit )
        tkgrid(Exit.but)
        tkgrid.configure(Exit.but,sticky="e")
        
        tkgrid(spec.frm)
        tkgrid(buttonFrameasym )
      }
      
        


 
    
    }


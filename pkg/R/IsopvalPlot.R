`IsopvalPlot` <-
function() {

if (!exists("exprs") | !exists("dose"))  
   {  
      ReturnVal <- tkmessageBox(title="Error Message",message="Load the data first!",icon="error",type="ok")
if (tclvalue(ReturnVal)== "ok") tkfocus(tt)
} 
  else {

isopvalplot <-tktoplevel()
tkwm.deiconify(isopvalplot )
tkwm.title(isopvalplot ,"Permutation p-values Plot ") 
tkgrab.set(isopvalplot )
tkfocus(isopvalplot )
spec.frm <- tkframe(isopvalplot ,borderwidth=2)
frame1 <- tkframe(spec.frm, relief="groove", borderwidth=2)

buttonFrameisoplot  <-tkframe(isopvalplot ,borderwidth=2)

rb1 <- tkradiobutton(frame1 )
rb2 <- tkradiobutton(frame1 )
rb3 <- tkradiobutton(frame1 )

rbValue1 <- tclVar("row")
tkconfigure(rb1,variable=rbValue1,value="gene",text="Gene Name(s), e.g., gene1,gene3")
tkconfigure(rb2,variable=rbValue1,value="row", text="Row Number(s),e.g., 2,3,4         ")
tkconfigure(rb3,variable=rbValue1,value="rowrange", text="Range of Row Number         From: ")

genename<- tclVar("")
GeneName<-tkentry(frame1 ,width="20",textvariable=genename)

rownum<- tclVar("")
RowNum<-tkentry(frame1 ,width="20",textvariable=rownum)

fromrow<- tclVar("")
FromNum<-tkentry(frame1 ,width="6",textvariable=fromrow)

torow <- tclVar("")
ToNum<-tkentry(frame1 ,width="6",textvariable=torow)

tkgrid(tklabel(frame1 ,text="Insert Gene name/ row name you want to draw:"))

tkgrid(rb1,GeneName)
tkgrid.configure(rb1,sticky="w")

tkgrid(rb2,RowNum)
tkgrid.configure(rb2,sticky="w")

tkgrid(rb3,FromNum,tklabel(frame1 ,text="To"),ToNum)
tkgrid.configure(rb3,sticky="w")

numperm <- tclVar("100")
NumPerm <-tkentry(frame1 ,width="5",textvariable=numperm)
label.numperm <- tklabel(frame1 ,text="      Number of Permutation: ")
tkgrid(label.numperm,NumPerm )
tkgrid.configure(label.numperm,sticky="w")

tkgrid(frame1)
tkgrid.configure(frame1,sticky="nsw")

### Statistics Check button ##

frame2 <- tkframe(spec.frm, relief="groove", borderwidth=2)

rb4 <- tkradiobutton(frame2 )
rb5 <- tkradiobutton(frame2 )
rb6 <- tkradiobutton(frame2 )
rb7 <- tkradiobutton(frame2 )
rb8 <- tkradiobutton(frame2 )

rbValue2 <- tclVar("M")
tkconfigure(rb4,variable=rbValue2,value="M",text="M")
tkconfigure(rb5,variable=rbValue2,value="E2", text="E2")
tkconfigure(rb6,variable=rbValue2,value="Marcus", text="Marcus")
tkconfigure(rb7,variable=rbValue2,value="Williams", text="Williams")
tkconfigure(rb8,variable=rbValue2,value="ModifM", text="M'")


label.stat <- tklabel(frame2,text="Statistic: ")
tkgrid(label.stat )
tkgrid.configure(label.stat ,sticky="w")

tkgrid(rb4,rb5,rb6,rb7,rb8)
tkgrid.configure(rb4,rb5,rb6,rb7,rb8,sticky="w")
tkgrid(frame2)
tkgrid.configure(frame2,sticky="nsw")


OnOK <- function()
{

  if (!exists("exprs"))  
{  
tkdestroy(isopvalplot)
ReturnVal <- tkmessageBox(title="Error Message",message="No data set loaded, please load the dataset first!",icon="info",type="ok")
if (tclvalue(ReturnVal)== "ok") tkfocus(tt)
} 
 else {
      tkdestroy(isopvalplot)
rbVal1 <- as.character(tclvalue(rbValue1))
rbVal2 <- as.character(tclvalue(rbValue2))

NumPerm <- as.numeric(tclvalue(numperm))

stat <- rbVal2 
 
      if (rbVal1=="rowrange") {

range <- as.numeric(tclvalue(fromrow):tclvalue(torow))
nrange <- length(range)
      plotIso <- function() 
{
nrange [nrange > 3 & nrange <= 12] <- 3
nrange [nrange > 12] <- 6
lengthplot <- round ((length(range)+1)/nrange)
lengthplot [nrange <= 2] <- 1
      par(mfrow=c((lengthplot*2),nrange ))
      for (i in (1:length(range )))
    {
    params <- par(bg="white")
    IsopvaluePlot(dose,as.matrix(exprs[range[i],]), niter=NumPerm , seed=SeedNum, stat = stat)
           }
}
Myhscale <- 2
Myvscale <- 2 

 Myvscale[nrange > 2 & nrange < 6] <- 2.5
Myhscale[nrange > 1 & nrange <12] <- 3
Myvscale[nrange >  6 ] <- 2.5
Myhscale[nrange > 12] <- 4
Plot2(plotIso,Myhscale , Myvscale, title="Windows Graph Output: Permutation p-values Plot" )
      }

     if (rbVal1=="row") {
a <- tclvalue(rownum)
b <- strsplit(a,",")
e <-0
for (i in (1:length(b[[1]])))
{
c <- strsplit(b[[1]][i]," ")
d <- as.numeric(c[[1]])
e[i] <- na.omit(d)
}

range <- as.numeric(e)
nrange <- length(range)
      plotIso <- function() 
{
nrange [nrange > 3 & nrange < 13] <- 3
nrange [nrange > 12] <- 6
lengthplot <- round ((length(range)+1)/nrange)
lengthplot [nrange <= 2] <- 1
      par(mfrow=c(2*lengthplot,nrange ))
      for (i in (1:length(range )))
    {
    params <- par(bg="white")
    IsopvaluePlot(dose,as.matrix(exprs[range[i],]), niter=NumPerm , seed=SeedNum, stat = stat)
          }
}
Myhscale <- 2
Myvscale <- 2 

 Myvscale[nrange > 2 & nrange < 6] <- 2.5
Myhscale[nrange > 1 & nrange <12] <- 3
Myvscale[nrange >  6 ] <- 3
Myhscale[nrange > 12] <-4
Plot2(plotIso,Myhscale , Myvscale,title="Windows Graph Output: Permutation p-values Plot" )

      }


     if (rbVal1=="gene") {
genenameVal <- as.character(tclvalue(genename))

b <- strsplit(genenameVal  ,",")
f <-0
for (i in (1:length(b[[1]])))
{
c <- unlist(strsplit(b[[1]][i]," "))
loc <- which(nchar(c)!=0)
d <-c[loc]
      e <- which(rownames(exprs)== d)
try( f[i] <- na.omit(e),silent=T)
}
g <- which(is.na(f))
if ( length(g) > 0) tkmessageBox(title="Error Message",message=" Some genes are not found or wrong input. Follow the instructions!! ",icon="warning",type="ok")

range <- na.omit(f)

        if (length(range)!= 0){
        nrange <- length(range)

        plotIso <- function() 
    {
nrange [nrange > 3 & nrange < 13] <- 3
nrange [nrange > 12] <- 6
lengthplot <- round ((length(range)+1)/nrange)
lengthplot [nrange <= 2] <- 1
      par(mfrow=c(2*lengthplot,nrange ))
      for (i in (1:length(range )))
    {
    params <- par(bg="white")
    IsopvaluePlot(dose,as.matrix(exprs[range[i],]), niter=NumPerm , seed=SeedNum, stat = stat)
          }
          }
  Myhscale <- 2
  Myvscale <- 2 

   Myvscale[nrange > 2 & nrange < 6] <- 2.5
  Myhscale[nrange > 1 & nrange <12] <- 3
  Myvscale[nrange >  6 ] <- 3
  Myhscale[nrange > 12] <-4
   Plot2(plotIso,Myhscale , Myvscale,title="Windows Graph Output: Permutation p-values Plot" )

    

     }
        if (length(range) == 0){
        tkmessageBox(message = "The gene is not found!")
           }
     }
      }
}

onCancel <- function()
  {
      tkgrab.release(isopvalplot )
tkdestroy(isopvalplot )
      
 }

OK.but <-tkbutton(buttonFrameisoplot  ,text="   OK   ",command=OnOK)
Cancel.but <-tkbutton(buttonFrameisoplot  ,text=" Cancel ",command=onCancel)
tkgrid(spec.frm)
tkgrid(OK.but,Cancel.but)
tkgrid(buttonFrameisoplot )

}
}


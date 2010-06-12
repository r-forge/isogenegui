`Isoplot` <-
function() {

if (!exists("exprs") | !exists("dose"))  
   {  
      ReturnVal <- tkmessageBox(title="Error Message",message="Load the data first!",icon="error",type="ok")
if (tclvalue(ReturnVal)== "ok") tkfocus(tt)
} 
else {
 isoplot <-tktoplevel()
tkwm.deiconify(isoplot )
tkwm.title(isoplot ,"Isotonic Regression Plot ") 
tkfocus(isoplot )
spec.frm <- tkframe(isoplot,borderwidth=2)
frame1 <- tkframe(spec.frm, relief="groove", borderwidth=2)

buttonFrameisoplot  <-tkframe(isoplot,borderwidth=2)

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

tkgrid(frame1)
tkgrid.configure(frame1,sticky="nsw")

frame2 <- tkframe(spec.frm, relief="groove", borderwidth=2)

cb1 <- tkcheckbutton(frame2 )
cbValue1 <- tclVar("0")
tkconfigure(cb1,variable=cbValue1,text="Dose as ordinal")
tkgrid(cb1)
tkgrid.configure(cb1,sticky="w")


cb3 <- tkcheckbutton(frame2 )
cbValue3 <- tclVar("0")
tkconfigure(cb3,variable=cbValue3,text="Show isotonic regression curve for both directions")
tkgrid(cb3)
tkgrid.configure(cb3,sticky="w")


cb2 <- tkcheckbutton(frame2 )
cbValue2 <- tclVar("0")
tkconfigure(cb2,variable=cbValue2,text="Show summary of the data")
tkgrid(cb2)
tkgrid.configure(cb2,sticky="w")

tkgrid(frame2)
tkgrid.configure(frame2,sticky="nsw")

OnOK <- function()
          {

  if (!exists("exprs"))  
{  
tkdestroy(isoplot )
ReturnVal <- tkmessageBox(title="Error Message",message="No data set loaded, please load the dataset first!",icon="info",type="ok")
if (tclvalue(ReturnVal)== "ok") tkfocus(tt)
} 
 else {
      tkdestroy(isoplot )
rbVal1 <- as.character(tclvalue(rbValue1))
cbVal1 <- as.character(tclvalue(cbValue1))
cbVal2 <- as.character(tclvalue(cbValue2))
cbVal3 <- as.character(tclvalue(cbValue3))

xaxis <- "continuous"

if (cbVal1 =="1") xaxis <- "ordinal"

      if (rbVal1=="rowrange") {

range <- as.numeric(tclvalue(fromrow):tclvalue(torow))
nrange <- length(range)
      plotIso <- function() 
{
nrange [nrange > 3 & nrange <= 12] <- 3
nrange [nrange > 12] <- 6
lengthplot <- round ((length(range)+1)/nrange)
lengthplot [nrange <= 2] <- 1
      par(mfrow=c(lengthplot,nrange ))
      for (i in (1:length(range )))
    {
    params <- par(bg="white")
              if (cbVal3 =="1") IsoPlot.mod(dose,exprs[range[i],],type=xaxis,add.curve=TRUE)
    else {
         IsoPlot(dose,exprs[range[i],],type=xaxis,add.curve=T)
         }
          }
}
Myhscale <- 1
Myvscale <- 1 

 Myvscale[nrange > 2 & nrange < 6] <- 1.7
Myhscale[nrange > 1 & nrange <12] <- 2
Myvscale[nrange >  6 ] <- 1.7
Myhscale[nrange > 12] <- 3
Plot2(plotIso,Myhscale , Myvscale, title="Windows Graph Output: IsoPlot" )
    if (cbVal2 =="1") printsummary(dose,exprs,range )

        }

     if (rbVal1=="row") {
a <- tclvalue(rownum)
b <- strsplit(a,",")
e <-0
length(b[[1]])
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
      par(mfrow=c(lengthplot,nrange ))
      for (i in (1:length(range )))
    {
    params <- par(bg="white")
    if (cbVal3 =="1") IsoPlot.mod(dose,exprs[range[i],],type=xaxis,add.curve=TRUE)
    else {
         IsoPlot(dose,exprs[range[i],],type=xaxis,add.curve=T)
         }

          }
}
Myhscale <- 1
Myvscale <- 1 

 Myvscale[nrange > 2 & nrange < 6] <- 1.7
Myhscale[nrange > 1 & nrange <12] <- 2
Myvscale[nrange >  6 ] <- 1.7
Myhscale[nrange > 12] <- 3
Plot2(plotIso,Myhscale , Myvscale,title="Windows Graph Output: IsoPlot" )

    if (cbVal2 =="1") printsummary(dose,exprs,range )

        }

      if (rbVal1=="gene") {
genenameVal <<- as.character(tclvalue(genename))

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

if ( f==0  ) {
tkmessageBox(title="Error Message",message=" Wrong gene name !! ",icon="warning",type="ok")
tkfocus(tt)
}
      else {
g <- which(is.na(f))

if ( length(g) > 0 ) tkmessageBox(title="Error Message",message=" Some genes are not found or wrong input. Follow the instructions!! ",icon="warning",type="ok")

range <- na.omit(f)
  #if (length (range) == 0) tkmessageBox(message = "The Gene(s) is(are) not found!")
           

      if (length (range)!= 0){
nrange <- length(range)
      plotIso <- function() 
{
nrange [nrange > 3 & nrange < 13] <- 3
nrange [nrange > 12] <- 6
lengthplot <- round ((length(range)+1)/nrange)
lengthplot [nrange <= 2] <- 1
      par(mfrow=c(lengthplot,nrange ))
      for (i in (1:length(range )))
    {
    params <- par(bg="white")
    if (cbVal3 =="1") IsoPlot.mod(dose,exprs[range[i],],type=xaxis,add.curve=TRUE)
    else {
         IsoPlot(dose,exprs[range[i],],type=xaxis,add.curve=T)
         }

         }
       }
Myhscale <- 1
Myvscale <- 1 

 Myvscale[nrange > 2 & nrange < 6] <- 1.7
Myhscale[nrange > 1 & nrange <12] <- 2
Myvscale[nrange >  6 ] <- 1.7
Myhscale[nrange > 12] <- 3
Plot2(plotIso,Myhscale , Myvscale,title="Windows Graph Output: IsoPlot" )
      if (cbVal2 =="1") printsummary(dose,exprs,range )
     }
    }
         }
     }
}

onCancel <- function()
  {
      tkgrab.release(isoplot )
tkdestroy(isoplot )
tkwm.deiconify(tt ) 
       tkfocus(tt )
 }

OK.but <-tkbutton(buttonFrameisoplot  ,text="   OK   ",command=OnOK)
Cancel.but <-tkbutton(buttonFrameisoplot  ,text=" Cancel ",command=onCancel)
tkgrid(spec.frm)
tkgrid(OK.but,Cancel.but)
tkgrid(buttonFrameisoplot )
}
}


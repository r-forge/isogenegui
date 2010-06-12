`getExprs` <-
function() {
 fileName <- tclvalue(tkgetOpenFile(filetypes=
        gettext('{"text" {".txt"}} {"Excel Files" {".xls"}}
{"All Files" {"*"}}')))
tclvalue(fileExprs) <<- fileName
if (!nchar(fileName)) 
{
    tkmessageBox(message = "No file was selected!")
} 
else 
{
splitted <- strsplit(fileName,".",fixed=TRUE)
if(unlist(splitted)[2] == "txt")
{
exprs <- read.table(file=fileName,header=TRUE)
assign("exprs",exprs,envir=.GlobalEnv)
}
else
{ 
if(unlist(splitted)[2] == "xls")
{
exprs.temp <- read.xls(fileName)
exprs <- data.frame(exprs.temp [,-1])
rownames(exprs ) <- exprs.temp [,1]
assign("exprs",exprs,envir=.GlobalEnv)
}
else {
tkmessageBox(message = "The files is not either txt or excel file")
}
}
   }
    }


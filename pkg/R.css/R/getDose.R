`getDose` <-
function() {
 fileName <- tclvalue(tkgetOpenFile(filetypes=
        gettext('{"text" {".txt"}} {"Excel Files" {".xls"}}
{"All Files" {"*"}}')))
tclvalue(fileDose) <<- fileName
if (!nchar(fileName)) 
{
    tkmessageBox(message = "No file was selected!")
} 
else 
{
splitted <- strsplit(fileName,".",fixed=TRUE)
if(unlist(splitted)[2] == "txt")
{
dose <- read.table(file=fileName,header=FALSE)
assign("dose",dose,envir=.GlobalEnv)
}
else
{ 
if(unlist(splitted)[2] == "xls")
{
dose <- unlist(read.xls(fileName))
assign("dose",dose,envir=.GlobalEnv)
}
else {
tkmessageBox(message = "The files is not either txt or excel file")
}
}
      }
  }


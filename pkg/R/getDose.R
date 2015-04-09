`getDose` <-
function() {
 fileName <- tclvalue(tkgetOpenFile(filetypes=
        gettext('{"text" {".txt"}} {"Excel Files" {".xlsx"}}
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
if(unlist(splitted)[2] == "xls" | unlist(splitted)[2] == "xlsx")
{
dose <- unlist(read.xlsx2(fileName, 1, header =FALSE, colClasses="numeric"))
assign("dose",dose,envir=.GlobalEnv)
}
else {
tkmessageBox(message = "The files is not either txt or Excel file")
}
}
      }
  }


`saveXls` <-
function() {

fileName <- tclvalue(tkgetSaveFile(filetypes="{{Excel Files} {.xls}} {{All files} *}"))
split <- strsplit(fileName,".",fixed=TRUE)
fileXls2 <<- paste(split[[1]][1],".xls",sep="")
assign("fileXls",fileXls,envir=.GlobalEnv)
tclvalue(fileXls) <<- fileXls2 
if (!nchar(fileName)) {
   tclvalue(fileXls) <<- c("")  
   tkmessageBox(message="No file was selected!")
   }
}


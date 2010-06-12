`savingRwd` <-
function() {

fileName <- tclvalue(tkgetSaveFile(filetypes="{{R images} {*.RData}} {{All files} *}"))
split <- strsplit(fileName,".",fixed=TRUE)
fileRwd <- paste(split[[1]][1],".Rdata",sep="")
assign("fileRwd",fileRwd,envir=.GlobalEnv)
tclvalue(SavefileName) <<- fileRwd 

if (!nchar(SavefileName)) {
    tclvalue(SavefileName) <<- c("")
    tkmessageBox(message="No file was selected!")
    }
}


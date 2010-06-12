`saveDir` <-
function() {
Dir <- tclvalue(tkchooseDirectory(title="Please specify the folder"))
assign("DirName",Dir ,envir=.GlobalEnv)
tclvalue(SaveDirName ) <<- Dir 
if (!nchar(Dir ))
    tkmessageBox(message="No folder was selected!")
}


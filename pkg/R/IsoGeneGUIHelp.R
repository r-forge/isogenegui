`IsoGeneGUIHelp` <-
function() {
          tkgrab.release(window)
    	    helpIndex <- file.path(system.file(package="IsoGeneGUI"),"index.html")
          browseURL(helpIndex)
            }


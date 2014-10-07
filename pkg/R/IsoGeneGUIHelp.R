`IsoGeneGUIHelp` <-
function() {
          tkgrab.release(window)
    	    helpIndex <- file.path(system.file("doc/manual", package="IsoGeneGUI"),"index.html")
          browseURL(helpIndex)
            }


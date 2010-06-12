`IsoGeneGUIHelp` <-
function() {
          tkgrab.release(window)
    	    helpIndex <- file.path(system.file("doc",package="IsoGeneGUI"),"index.html")
          browseURL(helpIndex)
            }


`IsoGeneGUIVignette` <-
function() {
          tkgrab.release(window)
    	    ViggIndex <- file.path(system.file("doc",package="IsoGeneGUI"),"IsoGeneGUI.pdf")
          openPDF (ViggIndex) # opens the pdf
            }
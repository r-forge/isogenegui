`onHelpIsoGeneGUI` <-
function() {
                tkgrab.release(window)
    helpIndex <- file.path(system.file("doc",package="IsoGeneGUI2"),"index.html")
   #helpIndex <- ("C:\\Documents and Settings\\lucp1898\\My Documents\\helpGUIhtml\\help\\index.html")

                 browseURL(helpIndex)
                }


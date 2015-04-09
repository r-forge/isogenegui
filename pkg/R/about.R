`about` <-
function() {

fontinfo <- tkfont.create(family="times",size=11)
fontinfo2 <- tkfont.create(family="times",size=13,weight="bold")

abt <- tktoplevel()
text1 <-tkframe(abt ,borderwidth=2)
text2 <-tkframe(abt ,borderwidth=2)
text3 <-tkframe(abt ,borderwidth=2)


tkwm.title(abt ," About   ")
tkgrid(tklabel(abt ,text="    About   ",font=fontinfo2))

tkgrid(tklabel(text1 ,text=" The IsoGene package is a package developed by Dan Lin et.al (dan.lin@uhasselt.be) 
 aimed to perform analysis of dose-response studies in microarray experiments.    ",font=fontinfo))

tkgrid(tklabel(text2 ,text=" The IsoGene GUI package is developed for users with no or limited knowledge 
about R programming so they can perform the analysis of dose-response in 
a microarray setting  easily. The package is developed and maintained by Setia Pramana 
(setia.pramana@ki.se)",font=fontinfo))


tkgrid(tklabel(text3 ,text="             More detail information of the IsoGene project can be found in:  
                http://ibiostat.be/online-resources/online-resources/isogenegui/isogenegui-package     ",font=fontinfo))

tkgrid.configure(text1 ,sticky="nsw")
tkgrid.configure(text2 ,sticky="nsw")
tkgrid.configure(text3 ,sticky="nsw")

}

`about` <-
function() {

fontinfo <- tkfont.create(family="times",size=10)
fontinfo2 <- tkfont.create(family="times",size=10,weight="bold")

abt <- tktoplevel()
text1 <-tkframe(abt ,borderwidth=2)
text2 <-tkframe(abt ,borderwidth=2)


tkwm.title(abt ," About   ")
tkgrid(tklabel(abt ,text="    About   ",font=fontinfo2))

tkgrid(tklabel(text1 ,text=" The IsoGene package is a package developed by Dan Lin(dan.lin@uhasselt.be) 
 aimed to perform analysis of dose-response studies in microarray experiments",font=fontinfo))

tkgrid(tklabel(text2 ,text=" The IsoGene GUI package is developed for users with no or limited knowledge 
about R programming so they can perform the analysis of dose-response in 
a microarray setting  easily. This is developed and maintained by Setia Pramana 
(setia.pramana@uhasselt.be)",font=fontinfo))

tkgrid.configure(text1 ,sticky="w")
tkgrid.configure(text2 ,sticky="nsw")

}


`IsoStat` <-
function (x,y) {
  StatVal  <- IsoGenemMod (x,y)
dire <-StatVal$direction
up.gene <- which(dire =="u")
down.gene <- which(dire =="d")
## E2 #

       E2.up <- cbind(StatVal$E2.up [up.gene ],up.gene )

   E2.down <-cbind( StatVal$E2.dn [down.gene ],down.gene)
         E2bind <- rbind(E2.up,  E2.down )
   E2Val <<-E2bind [order(E2bind[,2] ),1]
   SeE2Val <<- StatVal$SeE2

         ## M ##
    M.up <- cbind(StatVal$M.up [up.gene ],up.gene )
   M.down <-cbind( StatVal$M.dn [down.gene ],down.gene)
         Mbind <- rbind(M.up,  M.down )
   MVal <<-Mbind [order(Mbind [,2] ),1]

   SeM.up <- cbind(StatVal$SeM.up [up.gene ],up.gene )
   SeM.down <-cbind( StatVal$SeM.dn [down.gene ],down.gene)
         SeMbind <- rbind(SeM.up,  SeM.down )
   SeMVal <<-SeMbind [order(SeMbind[,2] ),1]

       ## Williams ##
         Williams.up <- cbind(StatVal$Williams.up [up.gene ],up.gene )
   Williams.down <-cbind( StatVal$Williams.dn [down.gene ],down.gene)
         Williamsbind <- rbind(Williams.up,  Williams.down )
   WilliamsVal <<-Williamsbind [order(Williamsbind[,2] ),1]

    SeWilliams.up <- cbind(StatVal$SeW.up [up.gene ],up.gene )
   SeWilliams.down <-cbind( StatVal$SeW.dn [down.gene ],down.gene)
         SeWilliamsbind <- rbind(SeWilliams.up,  SeWilliams.down )
   SeWilliamsVal <<-SeWilliamsbind [order(SeWilliamsbind [,2] ),1]


         ## Marcus##
         Marcus.up <- cbind(StatVal$Marcus.up [up.gene ],up.gene )
   Marcus.down <-cbind( StatVal$Marcus.dn [down.gene ],down.gene)
         Marcusbind <- rbind(Marcus.up,  Marcus.down )
   MarcusVal <<-Marcusbind [order(Marcusbind[,2] ),1]

   SeMarcus.up <- cbind(StatVal$SeMarc.up [up.gene ],up.gene )
   SeMarcus.down <-cbind( StatVal$SeMarc.dn [down.gene ],down.gene)
         SeMarcusbind <- rbind(SeMarcus.up,  SeMarcus.down )
   SeMarcusVal <<-SeMarcusbind [order(SeMarcusbind [,2] ),1]


         ## ModifM##
         ModM.up <- cbind(StatVal$ModM.up [up.gene ],up.gene )
   ModM.down <- cbind( StatVal$ModM.dn [down.gene ],down.gene)
         ModMbind <- rbind(ModM.up,  ModM.down )
   ModMVal <<- ModMbind [order(ModMbind[,2] ),1]

       SeModM.up <- cbind(StatVal$SeModM.up [up.gene ],up.gene )
   SeModM.down <- cbind( StatVal$SeModM.dn [down.gene ],down.gene)
         SeModMbind <- rbind(SeModM.up,  SeModM.down )
   SeModMVal <<- SeModMbind [order(SeModMbind [,2] ),1]
}


`CalcStat` <-
function() {  

 Direction <<-StatVal$direction
         up.gene <- which(Direction =="u")
         down.gene <- which(Direction =="d")

         ## E2 #

         E2.up <- cbind(StatVal$E2.up [up.gene ],up.gene )

   E2.down <-cbind( StatVal$E2.dn [down.gene ],down.gene)
         E2bind <- rbind(E2.up,  E2.down )
   E2Val <<- E2bind [order(E2bind[,2] ),1]

         ## M ##
    M.up <- cbind(StatVal$M.up [up.gene ],up.gene )
   M.down <-cbind( StatVal$M.dn [down.gene ],down.gene)
         Mbind <- rbind(M.up,  M.down )
   MVal <<-Mbind [order(Mbind[,2] ),1]

       ## Williams ##
         Williams.up <- cbind(StatVal$Williams.up [up.gene ],up.gene )
   Williams.down <-cbind( StatVal$Williams.dn [down.gene ],down.gene)
         Williamsbind <- rbind(Williams.up,  Williams.down )
   WilliamsVal <<-Williamsbind [order(Williamsbind[,2] ),1]

         ## Marcus##
         Marcus.up <- cbind(StatVal$Marcus.up [up.gene ],up.gene )
   Marcus.down <-cbind( StatVal$Marcus.dn [down.gene ],down.gene)
         Marcusbind <- rbind(Marcus.up,  Marcus.down )
   MarcusVal <<-Marcusbind [order(Marcusbind[,2] ),1]

         ## ModifM##
         ModM.up <- cbind(StatVal$ModM.up [up.gene ],up.gene )
   ModM.down <- cbind( StatVal$ModM.dn [down.gene ],down.gene)
         ModMbind <- rbind(ModM.up,  ModM.down )
   ModMVal <<- ModMbind [order(ModMbind[,2] ),1]

}


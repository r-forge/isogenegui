require("RUnit", quietly=TRUE) || stop("RUnit package not found")
require("IsoGeneGUI")

#testsuite.IsoGeneGUI <- defineTestSuite("IsoGeneGUI ",
#runitDirs <- c(".")
#dirs = runitDirs
#testFileRegexp = "^runit.+\\.r",
#testFuncRegexp = "^test.+",
#rngKind = "Marsaglia-Multicarry",
#rngNormalKind = "Kinderman-Ramage")

runitPat <- ".*_test\\.[rR]$"
runitDirs <- c(".")
suite.IsoGeneGUI  <- defineTestSuite(name="IsoGeneGUI",
                         dirs=runitDirs,
                         testFileRegexp=runitPat,
                         rngKind="default",
                         rngNormalKind="default")

testResult <- runTestSuite(testsuite.IsoGeneGUI )
printTextProtocol(testResult)








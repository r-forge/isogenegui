require("RUnit", quietly=TRUE) || stop("RUnit package not found")
require("IsoGeneGUI")

testsuite.IsoGeneGUI <- defineTestSuite("IsoGeneGUI ",
dirs = "c:\\Rtest",
testFileRegexp = "^runit.+\\.r",
testFuncRegexp = "^test.+",
rngKind = "Marsaglia-Multicarry",
rngNormalKind = "Kinderman-Ramage")


testResult <- runTestSuite(testsuite.IsoGeneGUI )
printTextProtocol(testResult)








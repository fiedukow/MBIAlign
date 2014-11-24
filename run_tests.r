library('RUnit')

source("DynAlign.r")

test.suite <- defineTestSuite("mbiTests",dirs = file.path("tests"),testFileRegexp = '^\\d+\\.r')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)

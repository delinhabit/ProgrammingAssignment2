library('RUnit')

test.suite <- defineTestSuite(
    "Week 2",
    dirs = file.path(getwd()),
    testFileRegexp = '^\\w+_test\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)

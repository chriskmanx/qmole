cmake_minimum_required(VERSION 2.8.9)

# Settings:
set(CTEST_DASHBOARD_ROOT                "/home/chris/cmake-2.8.10.2/Tests/CTestTestMemcheck")
set(CTEST_SITE                          "Chriss-iPad")
set(CTEST_BUILD_NAME                    "CTestTest-Darwin-g++-MemcheckDummyValgrindFailPre")

set(CTEST_SOURCE_DIRECTORY              "/home/chris/cmake-2.8.10.2/Tests/CTestTestMemcheck/DummyValgrindFailPre")
set(CTEST_BINARY_DIRECTORY              "/home/chris/cmake-2.8.10.2/Tests/CTestTestMemcheck/DummyValgrindFailPre")
set(CTEST_CVS_COMMAND                   "CVSCOMMAND-NOTFOUND")
set(CTEST_CMAKE_GENERATOR               "Unix Makefiles")
set(CTEST_BUILD_CONFIGURATION           "$ENV{CMAKE_CONFIG_TYPE}")
set(CTEST_COVERAGE_COMMAND              "/usr/bin/gcov")
set(CTEST_NOTES_FILES                   "${CTEST_SCRIPT_DIRECTORY}/${CTEST_SCRIPT_NAME}")

set(CTEST_MEMORYCHECK_COMMAND "${PSEUDO_VALGRIND}")

set(CTEST_CUSTOM_PRE_MEMCHECK "${ERROR_COMMAND}")

#CTEST_EMPTY_BINARY_DIRECTORY(${CTEST_BINARY_DIRECTORY})

CTEST_START(Experimental)
CTEST_CONFIGURE(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
#CTEST_BUILD(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
CTEST_TEST(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
CTEST_MEMCHECK(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)

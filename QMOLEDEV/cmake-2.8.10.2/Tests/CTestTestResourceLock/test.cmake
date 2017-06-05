cmake_minimum_required(VERSION 2.1)

# Settings:
set(CTEST_DASHBOARD_ROOT                "/home/chris/cmake-2.8.10.2/Tests/CTestTest")
set(CTEST_SITE                          "Chriss-iPad")
set(CTEST_BUILD_NAME                    "CTestTest-Darwin-g++-ResourceLock")

set(CTEST_SOURCE_DIRECTORY              "/home/chris/cmake-2.8.10.2/Tests/CTestTestResourceLock")
set(CTEST_BINARY_DIRECTORY              "/home/chris/cmake-2.8.10.2/Tests/CTestTestResourceLock")
set(CTEST_CVS_COMMAND                   "CVSCOMMAND-NOTFOUND")
set(CTEST_CMAKE_GENERATOR               "Unix Makefiles")
set(CTEST_BUILD_CONFIGURATION           "$ENV{CMAKE_CONFIG_TYPE}")
set(CTEST_COVERAGE_COMMAND              "/usr/bin/gcov")
set(CTEST_NOTES_FILES                   "${CTEST_SCRIPT_DIRECTORY}/${CTEST_SCRIPT_NAME}")

CTEST_START(Experimental)
CTEST_CONFIGURE(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
CTEST_BUILD(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
CTEST_TEST(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res PARALLEL_LEVEL 4)

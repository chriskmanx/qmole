# CMake generated Testfile for 
# Source directory: /home/chris/cmake-2.6.0/Tests/CMakeTests
# Build directory: /home/chris/cmake-2.6.0/Tests/CMakeTests
# 
# This file replicates the SUBDIRS() and ADD_TEST() commands from the source
# tree CMakeLists.txt file, skipping any SUBDIRS() or ADD_TEST() commands
# that are excluded by CMake control structures, i.e. IF() commands.
ADD_TEST(CMake.List "/home/chris/cmake-2.6.0/bin/cmake" "-P" "/home/chris/cmake-2.6.0/Tests/CMakeTests/ListTest.cmake")
ADD_TEST(CMake.VariableWatch "/home/chris/cmake-2.6.0/bin/cmake" "-P" "/home/chris/cmake-2.6.0/Tests/CMakeTests/VariableWatchTest.cmake")
ADD_TEST(CMake.Include "/home/chris/cmake-2.6.0/bin/cmake" "-P" "/home/chris/cmake-2.6.0/Tests/CMakeTests/IncludeTest.cmake")
ADD_TEST(CMake.FindBase "/home/chris/cmake-2.6.0/bin/cmake" "-P" "/home/chris/cmake-2.6.0/Tests/CMakeTests/FindBaseTest.cmake")
ADD_TEST(CMake.Toolchain "/home/chris/cmake-2.6.0/bin/cmake" "-P" "/home/chris/cmake-2.6.0/Tests/CMakeTests/ToolchainTest.cmake")
ADD_TEST(CMake.GetPrerequisites "/home/chris/cmake-2.6.0/bin/cmake" "-DCTEST_CONFIGURATION_TYPE:STRING=${CTEST_CONFIGURATION_TYPE}" "-P" "/home/chris/cmake-2.6.0/Tests/CMakeTests/GetPrerequisitesTest.cmake")

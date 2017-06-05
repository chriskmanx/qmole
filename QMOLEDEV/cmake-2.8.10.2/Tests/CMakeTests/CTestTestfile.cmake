# CMake generated Testfile for 
# Source directory: /home/chris/cmake-2.8.10.2/Tests/CMakeTests
# Build directory: /home/chris/cmake-2.8.10.2/Tests/CMakeTests
# 
# This file includes the relevent testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
ADD_TEST(CMake.List "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/ListTest.cmake")
ADD_TEST(CMake.VariableWatch "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/VariableWatchTest.cmake")
ADD_TEST(CMake.Include "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/IncludeTest.cmake")
ADD_TEST(CMake.FindBase "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/FindBaseTest.cmake")
ADD_TEST(CMake.Toolchain "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/ToolchainTest.cmake")
ADD_TEST(CMake.GetFilenameComponentRealpath "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/GetFilenameComponentRealpathTest.cmake")
ADD_TEST(CMake.Version "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/VersionTest.cmake")
ADD_TEST(CMake.Message "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/MessageTest.cmake")
ADD_TEST(CMake.File "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/FileTest.cmake")
ADD_TEST(CMake.ConfigureFile "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/ConfigureFileTest.cmake")
ADD_TEST(CMake.SeparateArguments "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/SeparateArgumentsTest.cmake")
ADD_TEST(CMake.ImplicitLinkInfo "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/ImplicitLinkInfoTest.cmake")
ADD_TEST(CMake.ModuleNotices "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/ModuleNoticesTest.cmake")
ADD_TEST(CMake.GetProperty "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/GetPropertyTest.cmake")
ADD_TEST(CMake.If "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/IfTest.cmake")
ADD_TEST(CMake.String "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/StringTest.cmake")
ADD_TEST(CMake.Math "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/MathTest.cmake")
ADD_TEST(CMake.CMakeMinimumRequired "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/CMakeMinimumRequiredTest.cmake")
ADD_TEST(CMake.CompilerIdVendor "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/CompilerIdVendorTest.cmake")
ADD_TEST(CMake.ProcessorCount "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/ProcessorCountTest.cmake")
ADD_TEST(CMake.PushCheckState "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/PushCheckStateTest.cmake")
ADD_TEST(CMake.While "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/WhileTest.cmake")
ADD_TEST(CMake.FileDownload "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/FileDownloadTest.cmake")
SET_TESTS_PROPERTIES(CMake.FileDownload PROPERTIES  PASS_REGULAR_EXPRESSION "file already exists with expected MD5 sum")
ADD_TEST(CMake.FileUpload "/home/chris/cmake-2.8.10.2/bin/cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/FileUploadTest.cmake")
ADD_TEST(CMake.EndStuff "/home/chris/cmake-2.8.10.2/bin/cmake" "-Ddir:STRING=/home/chris/cmake-2.8.10.2/Tests/CMakeTests/EndStuffTest" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/EndStuffTest.cmake")
ADD_TEST(CMake.GetPrerequisites "/home/chris/cmake-2.8.10.2/bin/cmake" "-DCTEST_CONFIGURATION_TYPE:STRING=${CTEST_CONFIGURATION_TYPE}" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/GetPrerequisitesTest.cmake")
ADD_TEST(CMake.CheckSourceTree "/home/chris/cmake-2.8.10.2/bin/cmake" "-DCMake_BINARY_DIR:PATH=/home/chris/cmake-2.8.10.2" "-DCMake_SOURCE_DIR:PATH=/home/chris/cmake-2.8.10.2" "-DGIT_EXECUTABLE:STRING=/usr/bin/git" "-DHOME:STRING=/home/chris" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeTests/CheckSourceTreeTest.cmake")

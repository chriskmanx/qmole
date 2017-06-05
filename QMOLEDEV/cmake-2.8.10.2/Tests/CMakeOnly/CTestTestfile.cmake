# CMake generated Testfile for 
# Source directory: /home/chris/cmake-2.8.10.2/Tests/CMakeOnly
# Build directory: /home/chris/cmake-2.8.10.2/Tests/CMakeOnly
# 
# This file includes the relevent testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
ADD_TEST(CMakeOnly.LinkInterfaceLoop "/home/chris/cmake-2.8.10.2/bin/cmake" "-DTEST=LinkInterfaceLoop" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeOnly/Test.cmake")
SET_TESTS_PROPERTIES(CMakeOnly.LinkInterfaceLoop PROPERTIES  TIMEOUT "90")
ADD_TEST(CMakeOnly.CheckSymbolExists "/home/chris/cmake-2.8.10.2/bin/cmake" "-DTEST=CheckSymbolExists" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeOnly/Test.cmake")
ADD_TEST(CMakeOnly.CheckCXXSymbolExists "/home/chris/cmake-2.8.10.2/bin/cmake" "-DTEST=CheckCXXSymbolExists" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeOnly/Test.cmake")
ADD_TEST(CMakeOnly.CheckCXXCompilerFlag "/home/chris/cmake-2.8.10.2/bin/cmake" "-DTEST=CheckCXXCompilerFlag" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeOnly/Test.cmake")
ADD_TEST(CMakeOnly.CheckLanguage "/home/chris/cmake-2.8.10.2/bin/cmake" "-DTEST=CheckLanguage" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeOnly/Test.cmake")
ADD_TEST(CMakeOnly.CompilerIdC "/home/chris/cmake-2.8.10.2/bin/cmake" "-DTEST=CompilerIdC" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeOnly/Test.cmake")
ADD_TEST(CMakeOnly.CompilerIdCXX "/home/chris/cmake-2.8.10.2/bin/cmake" "-DTEST=CompilerIdCXX" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeOnly/Test.cmake")
ADD_TEST(CMakeOnly.AllFindModules "/home/chris/cmake-2.8.10.2/bin/cmake" "-DTEST=AllFindModules" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeOnly/Test.cmake")
ADD_TEST(CMakeOnly.SelectLibraryConfigurations "/home/chris/cmake-2.8.10.2/bin/cmake" "-DTEST=SelectLibraryConfigurations" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeOnly/Test.cmake")
ADD_TEST(CMakeOnly.TargetScope "/home/chris/cmake-2.8.10.2/bin/cmake" "-DTEST=TargetScope" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeOnly/Test.cmake")
ADD_TEST(CMakeOnly.find_library "/home/chris/cmake-2.8.10.2/bin/cmake" "-DTEST=find_library" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeOnly/Test.cmake")
ADD_TEST(CMakeOnly.ProjectInclude "/home/chris/cmake-2.8.10.2/bin/cmake" "-DTEST=ProjectInclude" "-DCMAKE_ARGS=-DCMAKE_PROJECT_ProjectInclude_INCLUDE=/home/chris/cmake-2.8.10.2/Tests/CMakeOnly/ProjectInclude/include.cmake" "-P" "/home/chris/cmake-2.8.10.2/Tests/CMakeOnly/Test.cmake")

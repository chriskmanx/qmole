# CMake generated Testfile for 
# Source directory: /home/chris/cmake-2.8.10.2/Utilities
# Build directory: /home/chris/cmake-2.8.10.2/Utilities
# 
# This file includes the relevent testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
ADD_TEST(CMake.HTML "/usr/local/bin/xmllint" "--valid" "--noout" "--nonet" "--path" "/home/chris/cmake-2.8.10.2/Utilities/xml/xhtml1" "/home/chris/cmake-2.8.10.2/Docs/cmake.html" "/home/chris/cmake-2.8.10.2/Docs/cmake-policies.html" "/home/chris/cmake-2.8.10.2/Docs/cmake-properties.html" "/home/chris/cmake-2.8.10.2/Docs/cmake-variables.html" "/home/chris/cmake-2.8.10.2/Docs/cmake-modules.html" "/home/chris/cmake-2.8.10.2/Docs/cmake-commands.html" "/home/chris/cmake-2.8.10.2/Docs/cmake-compatcommands.html" "/home/chris/cmake-2.8.10.2/Docs/ctest.html" "/home/chris/cmake-2.8.10.2/Docs/cpack.html" "/home/chris/cmake-2.8.10.2/Docs/ccmake.html")
ADD_TEST(CMake.DocBook "/usr/local/bin/xmllint" "--valid" "--noout" "--nonet" "--path" "/home/chris/cmake-2.8.10.2/Utilities/xml/docbook-4.5" "/home/chris/cmake-2.8.10.2/Docs/cmake.docbook" "/home/chris/cmake-2.8.10.2/Docs/ctest.docbook" "/home/chris/cmake-2.8.10.2/Docs/cpack.docbook" "/home/chris/cmake-2.8.10.2/Docs/ccmake.docbook")
SUBDIRS(Doxygen)
SUBDIRS(KWStyle)

# Install script for directory: /home/chris/cmake-2.6.0/Templates

# Set the install prefix
IF(NOT DEFINED CMAKE_INSTALL_PREFIX)
  SET(CMAKE_INSTALL_PREFIX "/usr/local")
ENDIF(NOT DEFINED CMAKE_INSTALL_PREFIX)
STRING(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
IF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  IF(BUILD_TYPE)
    STRING(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  ELSE(BUILD_TYPE)
    SET(CMAKE_INSTALL_CONFIG_NAME "")
  ENDIF(BUILD_TYPE)
  MESSAGE(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
ENDIF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)

# Set the component getting installed.
IF(NOT CMAKE_INSTALL_COMPONENT)
  IF(COMPONENT)
    MESSAGE(STATUS "Install component: \"${COMPONENT}\"")
    SET(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  ELSE(COMPONENT)
    SET(CMAKE_INSTALL_COMPONENT)
  ENDIF(COMPONENT)
ENDIF(NOT CMAKE_INSTALL_COMPONENT)

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/cmake-2.6/Templates" TYPE FILE FILES
    "/home/chris/cmake-2.6.0/Templates/CPackConfig.cmake.in"
    "/home/chris/cmake-2.6.0/Templates/CTestScript.cmake.in"
    "/home/chris/cmake-2.6.0/Templates/TestDriver.cxx.in"
    "/home/chris/cmake-2.6.0/Templates/cygwin-package.sh.in"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/cmake-2.6/Templates" TYPE FILE FILES
    "/home/chris/cmake-2.6.0/Templates/CMakeLists.txt"
    "/home/chris/cmake-2.6.0/Templates/CPack.GenericDescription.txt"
    "/home/chris/cmake-2.6.0/Templates/CPack.GenericLicense.txt"
    "/home/chris/cmake-2.6.0/Templates/CPack.GenericWelcome.txt"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/cmake-2.6/Templates" TYPE FILE FILES "/home/chris/cmake-2.6.0/Templates/CMakeVisualStudio6Configurations.cmake")
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/cmake-2.6/Templates" TYPE FILE FILES
    "/home/chris/cmake-2.6.0/Templates/DLLFooter.dsptemplate"
    "/home/chris/cmake-2.6.0/Templates/DLLHeader.dsptemplate"
    "/home/chris/cmake-2.6.0/Templates/EXEFooter.dsptemplate"
    "/home/chris/cmake-2.6.0/Templates/EXEHeader.dsptemplate"
    "/home/chris/cmake-2.6.0/Templates/EXEWinHeader.dsptemplate"
    "/home/chris/cmake-2.6.0/Templates/UtilityFooter.dsptemplate"
    "/home/chris/cmake-2.6.0/Templates/UtilityHeader.dsptemplate"
    "/home/chris/cmake-2.6.0/Templates/staticLibFooter.dsptemplate"
    "/home/chris/cmake-2.6.0/Templates/staticLibHeader.dsptemplate"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/cmake-2.6/Templates" TYPE FILE FILES
    "/home/chris/cmake-2.6.0/Templates/CMakeVSMacros1.vsmacros"
    "/home/chris/cmake-2.6.0/Templates/CMakeVSMacros2.vsmacros"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")


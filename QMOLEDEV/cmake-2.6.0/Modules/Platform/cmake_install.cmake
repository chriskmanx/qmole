# Install script for directory: /home/chris/cmake-2.6.0/Modules/Platform

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
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/cmake-2.6/Modules/Platform" TYPE FILE FILES
    "/home/chris/cmake-2.6.0/Modules/Platform/AIX-VisualAge-Fortran.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/AIX.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/BSDOS.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/BeOS.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/BlueGeneL.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/CYGWIN-g77.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/CYGWIN.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Catamount.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Darwin-icc.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Darwin-icpc.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Darwin-xlc.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Darwin.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/DragonFly.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/FreeBSD.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/GNU.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Generic-ADSP-ASM.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Generic-ADSP-C.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Generic-ADSP-CXX.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Generic-ADSP-Common.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Generic-SDCC-C.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Generic.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/HP-UX.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/IRIX.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/IRIX64.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Linux-GNU-Fortran.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Linux-Intel-C.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Linux-Intel-CXX.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Linux-Intel-Fortran.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Linux-PGI-C.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Linux-PGI-CXX.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Linux-SunPro-C.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Linux-SunPro-CXX.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Linux-SunPro-Fortran.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Linux-VisualAge-C.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Linux-VisualAge-Fortran.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Linux-como.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Linux-icpc.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Linux-ifort.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Linux.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/MP-RAS.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/NetBSD.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/OSF1.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/OpenBSD.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/QNX.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/RISCos.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/SCO_SV.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/SINIX.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/SunOS-SunPro-Fortran.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/SunOS.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Tru64.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/ULTRIX.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/UNIX_SV.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/UnixPaths.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/UnixWare.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Windows-bcc32.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Windows-cl.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Windows-df.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Windows-g++.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Windows-g77.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Windows-gcc.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Windows-icl.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Windows-ifort.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Windows-wcl386.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Windows.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/WindowsPaths.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/Xenix.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/cl.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/eCos.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/g77.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/gas.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/gcc.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/kFreeBSD.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/syllable.cmake"
    "/home/chris/cmake-2.6.0/Modules/Platform/xlf.cmake"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/cmake-2.6/Modules/Platform" TYPE FILE FILES "/home/chris/cmake-2.6.0/Modules/Platform/Windows-cl.cmake.in")
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")


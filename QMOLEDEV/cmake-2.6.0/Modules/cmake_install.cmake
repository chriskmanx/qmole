# Install script for directory: /home/chris/cmake-2.6.0/Modules

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
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/cmake-2.6/Modules" TYPE FILE FILES
    "/home/chris/cmake-2.6.0/Modules/AddFileDependencies.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMake.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeASM-ATTInformation.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeASMInformation.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeBackwardCompatibilityC.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeBackwardCompatibilityCXX.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeBorlandFindMake.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeCInformation.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeCXXInformation.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeCommonLanguageInclude.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeDependentOption.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeDetermineASM-ATTCompiler.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeDetermineASMCompiler.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeDetermineCCompiler.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeDetermineCXXCompiler.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeDetermineCompilerABI.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeDetermineCompilerId.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeDetermineFortranCompiler.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeDetermineJavaCompiler.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeDetermineRCCompiler.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeDetermineSystem.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeExportBuildSettings.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeFindBinUtils.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeFindFrameworks.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeFindWMake.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeFindXCode.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeForceCompiler.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeFortranInformation.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeGenericSystem.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeImportBuildSettings.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeJavaInformation.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeMSYSFindMake.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeMinGWFindMake.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeNMakeFindMake.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakePrintSystemInformation.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeRCInformation.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeSystemSpecificInformation.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeTestASM-ATTCompiler.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeTestASMCompiler.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeTestCCompiler.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeTestCXXCompiler.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeTestFortranCompiler.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeTestJavaCompiler.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeTestRCCompiler.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeUnixFindMake.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeVS6BackwardCompatibility.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeVS6FindMake.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeVS71FindMake.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeVS7BackwardCompatibility.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeVS7FindMake.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeVS8FindMake.cmake"
    "/home/chris/cmake-2.6.0/Modules/CMakeVS9FindMake.cmake"
    "/home/chris/cmake-2.6.0/Modules/CPack.cmake"
    "/home/chris/cmake-2.6.0/Modules/CPackDeb.cmake"
    "/home/chris/cmake-2.6.0/Modules/CPackRPM.cmake"
    "/home/chris/cmake-2.6.0/Modules/CPackZIP.cmake"
    "/home/chris/cmake-2.6.0/Modules/CTest.cmake"
    "/home/chris/cmake-2.6.0/Modules/CTestTargets.cmake"
    "/home/chris/cmake-2.6.0/Modules/CheckCCompilerFlag.cmake"
    "/home/chris/cmake-2.6.0/Modules/CheckCSourceCompiles.cmake"
    "/home/chris/cmake-2.6.0/Modules/CheckCSourceRuns.cmake"
    "/home/chris/cmake-2.6.0/Modules/CheckCXXCompilerFlag.cmake"
    "/home/chris/cmake-2.6.0/Modules/CheckCXXSourceCompiles.cmake"
    "/home/chris/cmake-2.6.0/Modules/CheckCXXSourceRuns.cmake"
    "/home/chris/cmake-2.6.0/Modules/CheckFortranFunctionExists.cmake"
    "/home/chris/cmake-2.6.0/Modules/CheckFunctionExists.cmake"
    "/home/chris/cmake-2.6.0/Modules/CheckIncludeFile.cmake"
    "/home/chris/cmake-2.6.0/Modules/CheckIncludeFileCXX.cmake"
    "/home/chris/cmake-2.6.0/Modules/CheckIncludeFiles.cmake"
    "/home/chris/cmake-2.6.0/Modules/CheckLibraryExists.cmake"
    "/home/chris/cmake-2.6.0/Modules/CheckSizeOf.cmake"
    "/home/chris/cmake-2.6.0/Modules/CheckStructHasMember.cmake"
    "/home/chris/cmake-2.6.0/Modules/CheckSymbolExists.cmake"
    "/home/chris/cmake-2.6.0/Modules/CheckTypeSize.cmake"
    "/home/chris/cmake-2.6.0/Modules/CheckVariableExists.cmake"
    "/home/chris/cmake-2.6.0/Modules/Dart.cmake"
    "/home/chris/cmake-2.6.0/Modules/Documentation.cmake"
    "/home/chris/cmake-2.6.0/Modules/FLTKCompatibility.cmake"
    "/home/chris/cmake-2.6.0/Modules/FeatureSummary.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindASPELL.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindAVIFile.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindBLAS.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindBZip2.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindBoost.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindCABLE.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindCURL.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindCVS.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindCups.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindCurses.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindCygwin.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindDCMTK.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindDart.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindDoxygen.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindEXPAT.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindFLTK.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindFLTK2.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindFreetype.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindGCCXML.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindGDAL.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindGIF.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindGLU.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindGLUT.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindGTK.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindGettext.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindGnuplot.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindHSPELL.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindHTMLHelp.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindITK.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindImageMagick.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindJNI.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindJPEG.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindJasper.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindJava.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindKDE3.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindKDE4.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindLAPACK.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindLATEX.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindLibXml2.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindLibXslt.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindLua50.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindLua51.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindMFC.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindMPEG.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindMPEG2.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindMPI.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindMatlab.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindMotif.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindOpenAL.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindOpenGL.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindOpenSSL.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindOpenThreads.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindPHP4.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindPNG.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindPackageHandleStandardArgs.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindPackageMessage.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindPerl.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindPerlLibs.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindPhysFS.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindPike.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindPkgConfig.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindProducer.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindPythonInterp.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindPythonLibs.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindQt.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindQt3.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindQt4.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindQuickTime.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindRuby.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindSDL.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindSDL_image.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindSDL_mixer.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindSDL_net.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindSDL_sound.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindSDL_ttf.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindSWIG.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindSelfPackers.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindSubversion.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindTCL.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindTIFF.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindTclStub.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindTclsh.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindThreads.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindUnixCommands.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindVTK.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindWget.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindWish.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindX11.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindXMLRPC.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindZLIB.cmake"
    "/home/chris/cmake-2.6.0/Modules/Findosg.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindosgDB.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindosgFX.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindosgGA.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindosgIntrospection.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindosgManipulator.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindosgParticle.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindosgProducer.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindosgShadow.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindosgSim.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindosgTerrain.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindosgText.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindosgUtil.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindosgViewer.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindwxWidgets.cmake"
    "/home/chris/cmake-2.6.0/Modules/FindwxWindows.cmake"
    "/home/chris/cmake-2.6.0/Modules/GetPrerequisites.cmake"
    "/home/chris/cmake-2.6.0/Modules/ITKCompatibility.cmake"
    "/home/chris/cmake-2.6.0/Modules/InstallRequiredSystemLibraries.cmake"
    "/home/chris/cmake-2.6.0/Modules/KDE3Macros.cmake"
    "/home/chris/cmake-2.6.0/Modules/MacroAddFileDependencies.cmake"
    "/home/chris/cmake-2.6.0/Modules/SystemInformation.cmake"
    "/home/chris/cmake-2.6.0/Modules/TestBigEndian.cmake"
    "/home/chris/cmake-2.6.0/Modules/TestCXXAcceptsFlag.cmake"
    "/home/chris/cmake-2.6.0/Modules/TestForANSIForScope.cmake"
    "/home/chris/cmake-2.6.0/Modules/TestForANSIStreamHeaders.cmake"
    "/home/chris/cmake-2.6.0/Modules/TestForSSTREAM.cmake"
    "/home/chris/cmake-2.6.0/Modules/TestForSTDNamespace.cmake"
    "/home/chris/cmake-2.6.0/Modules/UseEcos.cmake"
    "/home/chris/cmake-2.6.0/Modules/UsePkgConfig.cmake"
    "/home/chris/cmake-2.6.0/Modules/UseQt4.cmake"
    "/home/chris/cmake-2.6.0/Modules/UseSWIG.cmake"
    "/home/chris/cmake-2.6.0/Modules/UseVTK40.cmake"
    "/home/chris/cmake-2.6.0/Modules/UseVTKBuildSettings40.cmake"
    "/home/chris/cmake-2.6.0/Modules/UseVTKConfig40.cmake"
    "/home/chris/cmake-2.6.0/Modules/Use_wxWindows.cmake"
    "/home/chris/cmake-2.6.0/Modules/UsewxWidgets.cmake"
    "/home/chris/cmake-2.6.0/Modules/VTKCompatibility.cmake"
    "/home/chris/cmake-2.6.0/Modules/ecos_clean.cmake"
    "/home/chris/cmake-2.6.0/Modules/kde3uic.cmake"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/cmake-2.6/Modules" TYPE FILE FILES "/home/chris/cmake-2.6.0/Modules/CMakeCXXCompilerABI.cpp")
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/cmake-2.6/Modules" TYPE FILE FILES
    "/home/chris/cmake-2.6.0/Modules/CMakeTestForFreeVC.cxx"
    "/home/chris/cmake-2.6.0/Modules/DummyCXXFile.cxx"
    "/home/chris/cmake-2.6.0/Modules/TestForANSIStreamHeaders.cxx"
    "/home/chris/cmake-2.6.0/Modules/TestForAnsiForScope.cxx"
    "/home/chris/cmake-2.6.0/Modules/TestForSSTREAM.cxx"
    "/home/chris/cmake-2.6.0/Modules/TestForSTDNamespace.cxx"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/cmake-2.6/Modules" TYPE FILE FILES
    "/home/chris/cmake-2.6.0/Modules/CMakeASMCompiler.cmake.in"
    "/home/chris/cmake-2.6.0/Modules/CMakeBuildSettings.cmake.in"
    "/home/chris/cmake-2.6.0/Modules/CMakeCCompiler.cmake.in"
    "/home/chris/cmake-2.6.0/Modules/CMakeCCompilerId.c.in"
    "/home/chris/cmake-2.6.0/Modules/CMakeCXXCompiler.cmake.in"
    "/home/chris/cmake-2.6.0/Modules/CMakeCXXCompilerId.cpp.in"
    "/home/chris/cmake-2.6.0/Modules/CMakeConfigurableFile.in"
    "/home/chris/cmake-2.6.0/Modules/CMakeFortranCompiler.cmake.in"
    "/home/chris/cmake-2.6.0/Modules/CMakeFortranCompilerId.F90.in"
    "/home/chris/cmake-2.6.0/Modules/CMakeJavaCompiler.cmake.in"
    "/home/chris/cmake-2.6.0/Modules/CMakePlatformId.h.in"
    "/home/chris/cmake-2.6.0/Modules/CMakeRCCompiler.cmake.in"
    "/home/chris/cmake-2.6.0/Modules/CMakeSystem.cmake.in"
    "/home/chris/cmake-2.6.0/Modules/CPack.DS_Store.in"
    "/home/chris/cmake-2.6.0/Modules/CPack.Description.plist.in"
    "/home/chris/cmake-2.6.0/Modules/CPack.Info.plist.in"
    "/home/chris/cmake-2.6.0/Modules/CPack.OSXScriptLauncher.in"
    "/home/chris/cmake-2.6.0/Modules/CPack.OSXX11.Info.plist.in"
    "/home/chris/cmake-2.6.0/Modules/CPack.RuntimeScript.in"
    "/home/chris/cmake-2.6.0/Modules/CPack.STGZ_Header.sh.in"
    "/home/chris/cmake-2.6.0/Modules/CPack.VolumeIcon.icns.in"
    "/home/chris/cmake-2.6.0/Modules/CPack.background.png.in"
    "/home/chris/cmake-2.6.0/Modules/CheckIncludeFile.c.in"
    "/home/chris/cmake-2.6.0/Modules/CheckIncludeFile.cxx.in"
    "/home/chris/cmake-2.6.0/Modules/CheckLibraryExists.lists.in"
    "/home/chris/cmake-2.6.0/Modules/CheckTypeSizeC.c.in"
    "/home/chris/cmake-2.6.0/Modules/DartConfiguration.tcl.in"
    "/home/chris/cmake-2.6.0/Modules/NSIS.InstallOptions.ini.in"
    "/home/chris/cmake-2.6.0/Modules/NSIS.template.in"
    "/home/chris/cmake-2.6.0/Modules/SystemInformation.in"
    "/home/chris/cmake-2.6.0/Modules/TestEndianess.c.in"
    "/home/chris/cmake-2.6.0/Modules/kde3init_dummy.cpp.in"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/cmake-2.6/Modules" TYPE FILE FILES
    "/home/chris/cmake-2.6.0/Modules/CMakeCCompilerABI.c"
    "/home/chris/cmake-2.6.0/Modules/CMakeTestGNU.c"
    "/home/chris/cmake-2.6.0/Modules/CMakeTestNMakeCLVersion.c"
    "/home/chris/cmake-2.6.0/Modules/CheckForPthreads.c"
    "/home/chris/cmake-2.6.0/Modules/CheckFunctionExists.c"
    "/home/chris/cmake-2.6.0/Modules/CheckVariableExists.c"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/cmake-2.6/Modules" TYPE FILE FILES "/home/chris/cmake-2.6.0/Modules/CMakeCompilerABI.h")
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/cmake-2.6/Modules" TYPE FILE FILES
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/cmake-2.6/Modules" TYPE FILE FILES "/home/chris/cmake-2.6.0/Modules/readme.txt")
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" MATCHES "^()$")

IF(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  INCLUDE("/home/chris/cmake-2.6.0/Modules/Platform/cmake_install.cmake")

ENDIF(NOT CMAKE_INSTALL_LOCAL_ONLY)


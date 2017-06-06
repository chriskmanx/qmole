@echo off

echo Setting installation variables...
set PythonEXE=C:\Python25\python.exe
set DistDir=dist
set GTKDir=C:\GTK
set WinInstallDir=install_scripts\windows
set Output=win_install.log
set UtilsDir=install_scripts\utils

IF EXIST %PythonEXE% GOTO GGTK
ECHO No Python 2.5 found!
EXIT 1

:GGTK
IF EXIST %GTKDir% GOTO GWin
ECHO No GTK found!
EXIT 1

:GWin
IF EXIST %WinInstallDir% GOTO GUtils
ECHO No Windows install scripts found!
EXIT 1

:GUtils
IF EXIST %UtilsDir% GOTO GWork
ECHO No utils directory found!
EXIT 1

:GWork

echo Writing output to %Output%

echo Removing old compilation...
IF EXIST %DistDir% rd %DistDir% /s /q > %Output%

echo Creating dist and dist\share directories...
mkdir %DistDir%\share
mkdir %DistDir%\share\gtk-2.0
mkdir %DistDir%\share\gtkthemeselector
mkdir %DistDir%\share\themes
mkdir %DistDir%\share\themes\Default
mkdir %DistDir%\share\themes\MS-Windows
mkdir %DistDir%\share\xml


echo Copying GTK's share to dist directory...
xcopy %GTKDir%\share\gtk-2.0\*.* %DistDir%\share\gtk-2.0\ /S >> %Output%
xcopy %GTKDir%\share\gtkthemeselector\*.* %DistDir%\share\gtkthemeselector\ /S >> %Output%
xcopy %GTKDir%\share\themes\Default\*.* %DistDir%\share\themes\Default /S >> %Output%
xcopy %GTKDir%\share\themes\MS-Windows\*.* %DistDir%\share\themes\MS-Windows /S >> %Output%
xcopy %GTKDir%\share\xml\*.* %DistDir%\share\xml\ /S >> %Output%
xcopy %GTKDir%\bin\*.dll %DistDir% /S >> %Output%

echo Compiling using py2exe...
%PythonEXE% setup.py py2exe >> %Output%

echo Copying some more GTK files to dist directory...
xcopy %GTKDir%\lib %DistDir%\lib /S /I >> %Output%
xcopy %GTKDir%\etc %DistDir%\etc /S /I >> %Output%


echo Removing the build directory...
rd build /s /q >> %Output%


echo Done!


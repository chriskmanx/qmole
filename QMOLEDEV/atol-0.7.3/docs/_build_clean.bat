@ECHO OFF
REM 
REM Script to clean Atol project tree on windows (delete temporary files)
REM 	NOTE: requires some recent Windows (NT based)
REM

IF NOT "%OS%"=="Windows_NT" GOTO :End

rmdir /S /Q ..\bin\debug
rmdir /S /Q ..\bin\release
rmdir /S /Q ..\bin\profile
rmdir /S /Q ..\locale
del ..\Atol.ncb
del ..\Atol.plg
del ..\Atol.opt
del ..\Atol.*.user
del /F /Q /A H ..\Atol.suo
del ..\res\Atol.aps

REM Mingw files
del /Q ..\bin\*.*
REM restore dummy file in bin directory
copy ..\docs\readme.txt ..\bin\readme.txt

rmdir /S /Q ..\plugins\bin\Debug
rmdir /S /Q ..\plugins\bin\Release
rmdir /S /Q ..\plugins\bin\Profile
rmdir /S /Q ..\plugins\src\ZipLib\ZipArchive\Release
rmdir /S /Q ..\plugins\src\ZipLib\ZipArchive\Debug

del ..\plugins\src\ArjLib\ArjLib.plg
del /Q ..\plugins\src\ArjLib\*.user
del ..\plugins\src\Bz2Lib\Bz2Lib.plg
del /Q ..\plugins\src\Bz2Lib\*.user
del ..\plugins\src\Bz2Lib\bzip2\libbz2_static.plg
del /Q ..\plugins\src\Bz2Lib\bzip2\*.user
del ..\plugins\src\GzLib\GzLib.plg
del /Q ..\plugins\src\GzLib\*.user
del ..\plugins\src\GzLib\zlib_static.lib
del ..\plugins\src\TarLib\TarLib.plg
del /Q ..\plugins\src\TarLib\*.user
del ..\plugins\src\ZipLib\ZipLib.plg
del /Q ..\plugins\src\ZipLib\*.user
del ..\plugins\src\ZipLib\ZipArchive\ZipArchive_STL.plg
del /Q ..\plugins\src\ZipLib\ZipArchive\*.user
del ..\plugins\src\ZLib\ZLib.plg
del /Q ..\plugins\src\ZLib\*.user

REM start script to prepare Windows C++ source files
cd ..\plugins\src\ZipLib\ZipArchive\
CALL _copy_from_Win-STL.bat

:End

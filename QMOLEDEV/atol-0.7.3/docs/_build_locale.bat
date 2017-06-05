@ECHO OFF
REM 
REM This is a batch script to rebuild /locale directory (used to prepare Win32 release)
REM 	NOTE: requires some recent Windows (NT based)
REM

IF NOT "%OS%"=="Windows_NT" GOTO :End

REM define some constants

SET program=atol
SET srcdir=../po/
SET target=locale
SET msgfmt="C:\\Program Files\PoEdit\bin\msgfmt.exe"
SET tmpmo="C:\\Program Files\PoEdit\bin\messages.mo"
SET tmpdir="C:\\Program Files\PoEdit\bin\"

REM Check if source directory exists

IF NOT EXIST .\%srcdir%\NUL GOTO :End

REM Remove and recreate target directory

PUSHD ..
IF EXIST .\%target%\NUL RMDIR /S /Q %target%
MKDIR %target%
POPD

REM  main loop

FOR /R "%srcdir%" %%A IN (*.po) DO CALL :ProcessFile %%A

GOTO :End

REM %%~n1 expands variable %%1 to file name only
REM %%~f1 expands variable %%1 to full path name

:ProcessFile
@echo %~n1
REM delete previously existing compiled message catalog (not to use junk one in case creating new one fails)
if exist %tmpmo% (
	PUSHD %tmpdir% 
	DEL /F messages.mo
	POPD
)
REM compile message catalog
%msgfmt%  %~f1 -o %tmpmo% 2> nul

if exist %tmpmo% (
	PUSHD ..
	MKDIR %target%\%~n1\LC_MESSAGES  3> nul
	copy %tmpmo% %target%\%~n1\LC_MESSAGES\%program%.mo > nul
	POPD
) ELSE (
	echo Error: failed to compile %~n1 package!
)

:End
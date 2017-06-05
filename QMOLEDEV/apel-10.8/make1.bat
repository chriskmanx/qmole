echo off
rem MAKE1.BAT for APEL.
rem
rem Version: $Id: make1.bat,v 1.1 2001-02-01 03:19:36 minakaji Exp $
rem Last Modified: $Date: 2001-02-01 03:19:36 $

rem --- argument
rem ---   elc : byte compile
rem ---   all, install : install
rem ---   clean : cleaning garbage file
rem ---   what-where : print where to install
rem ---

rem --- check calling from make.bat
if not "%SUBMAKEOK%"=="OK" goto prnusage
set SUBMAKEOK=

rem argument check

set arg1=%1

if "%arg1%"=="elc" goto compile
if "%arg1%"=="all" goto install
if "%arg1%"=="install" goto install
if "%arg1%"=="what-where" goto listing
if "%arg1%"=="clean" goto clean
echo Unrecognized argument: specify either 'elc', 'all',
echo 'install', 'clean' or 'what-where'.
goto pauseend

:compile
%EMACS% -q -batch -no-site-file -l APEL-MK -f compile-apel NONE %LISPDIR% %VLISPDIR%
goto end

:install
%EMACS% -q -batch -no-site-file -l APEL-MK -f install-apel NONE %LISPDIR% %VLISPDIR%
goto end

:listing
%EMACS% -batch -q -no-site-file -l APEL-MK -f what-where-apel
goto end

:clean
del *.elc

rem --- This file should not be executed by itself. Use make.bat.
:prnusage
echo This file should not be executed by itself. Use make.bat.

rem --- If error occurs, stay display until any key is typed.
:pauseend
echo Type any key when you're done reading the error message.
pause

:end


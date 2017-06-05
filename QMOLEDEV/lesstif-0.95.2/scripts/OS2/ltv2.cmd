@echo off
rem $Header: /cvsroot/lesstif/lesstif/scripts/OS2/ltv2.cmd,v 1.12 2002/08/18 11:04:42 amai Exp $
rem
rem This script toggles the default version of LessTif (Motif)
rem libraries, headers, etc. being used for compiling/building.
rem Also it's being used to install LessTif.
rem
rem Written by Alexander Mai <amai@lesstif.org>

setlocal

if "%1." == "1.2." goto :VERSIONOK
if "%1." == "2.0." goto :VERSIONOK
if "%1." == "2.1." goto :VERSIONOK

echo  %0%: Unknown version
echo  Syntax: %0% version
echo  version is either 1.2, 2.0 or 2.1
goto :ENDE


:VERSIONOK

if "%X11ROOT%." == "." goto XROOTBROKEN
if not exist %X11ROOT%\XFree86 goto XROOTBROKEN

rem Some checks
cd %X11ROOT%\XFree86\lib
if not exist Xm_21.a goto LTINSTBROKEN
cd %X11ROOT%\XFree86\include
if not exist Xm-2.1\Xm.h goto LTINSTBROKEN


cd %X11ROOT%\XFree86\lib

rem Take care of our backup directory structure!
if not exist X11\LessTif                   md X11\LessTif
if not exist X11\LessTif\last              md X11\LessTif\last
if not exist X11\LessTif\last\include      md X11\LessTif\last\include
if not exist X11\LessTif\last\include\Xm   md X11\LessTif\last\include\Xm
if not exist X11\LessTif\last\include\Mrm  md X11\LessTif\last\include\Mrm
if not exist X11\LessTif\last\include\uil  md X11\LessTif\last\include\uil
if not exist X11\LessTif\last\lib          md X11\LessTif\last\lib


echo Backing up previous default version's libs and headers
if exist Xm.a      copy Xm.a     X11\LessTif\last\lib  >nul
if exist Xm.lib    copy Xm.lib   X11\LessTif\last\lib  >nul
if exist Mrm.a     copy Mrm.a    X11\LessTif\last\lib  >nul
if exist Mrm.lib   copy Mrm.lib  X11\LessTif\last\lib  >nul
if exist Uil.a     copy Uil.a    X11\LessTif\last\lib  >nul
if exist Uil.lib   copy Uil.lib  X11\LessTif\last\lib  >nul

cd ..\include
if exist ..\lib\X11\LessTif\last\include\Xm\*.h   del ..\lib\X11\LessTif\last\include\Xm\*.h >nul
if exist  Xm\*.h move Xm\*.h  ..\lib\X11\LessTif\last\include\Xm   >nul
if not exist Xm  md Xm >nul
if exist ..\lib\X11\LessTif\last\include\Mrm\*.h  del ..\lib\X11\LessTif\last\include\Mrm\*.h >nul
if exist  Mrm\*.h move Mrm\*.h ..\lib\X11\LessTif\last\include\Mrm  >nul
if not exist Mrm  md Mrm >nul
if exist ..\lib\X11\LessTif\last\include\uil\*.h  del ..\lib\X11\LessTif\last\include\uil\*.h >nul
if exist  uil\*.h move uil\*.h ..\lib\X11\LessTif\last\include\uil  >nul
if not exist uil  md uil >nul

if "%1." == "1.2." goto :LIB12
if "%1." == "2.0." goto :LIB20
if "%1." == "2.1." goto :LIB21
goto :ENDE


:LIB12
echo Installing Version 1.2
cd ..\lib 
copy Xm_12.a     Xm.a         >nul
copy Xm_12.lib   Xm.lib       >nul
copy Mrm_12.a    Mrm.a        >nul
copy Mrm_12.lib  Mrm.lib      >nul
copy Uil_12.a    Uil.a        >nul
copy Uil_12.lib  Uil.lib      >nul
cd ..\include
copy Xm-1.2\*.h   Xm\         >nul
copy Mrm-1.2\*.h  Mrm\        >nul
copy uil-1.2\*.h  uil\        >nul
echo Done.
goto ENDE


:LIB20
echo Installing Version 2.0
cd ..\lib
copy Xm_20.a     Xm.a         >nul
copy Xm_20.lib   Xm.lib       >nul
copy Mrm_20.a    Mrm.a        >nul
copy Mrm_20.lib  Mrm.lib      >nul
copy Uil_20.a    Uil.a        >nul
copy Uil_20.lib  Uil.lib      >nul
cd ..\include
copy Xm-2.0\*.h   Xm\         >nul
copy Mrm-2.0\*.h  Mrm\        >nul
copy uil-2.0\*.h  uil\        >nul
echo Done.
goto ENDE


:LIB21
echo Installing Version 2.1
cd ..\lib
copy Xm_21.a     Xm.a         >nul
copy Xm_21.lib   Xm.lib       >nul
copy Mrm_21.a    Mrm.a        >nul
copy Mrm_21.lib  Mrm.lib      >nul
copy Uil_21.a    Uil.a        >nul
copy Uil_21.lib  Uil.lib      >nul
cd ..\include
copy Xm-2.1\*.h   Xm\         >nul
copy Mrm-2.1\*.h  Mrm\        >nul
copy uil-2.1\*.h  uil\        >nul
echo Done.
goto ENDE


:XROOTBROKEN
echo Environment variable X11ROOT is not set or invalid! Cannot continue.
goto ENDE

:LTINSTBROKEN
echo No complete LessTif package found
goto ENDE

:ENDE

endlocal

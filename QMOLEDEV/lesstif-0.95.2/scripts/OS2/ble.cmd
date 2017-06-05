/* REXX */
/* 
   $Header: /cvsroot/lesstif/lesstif/scripts/OS2/ble.cmd,v 1.13 2002/05/03 12:18:55 amai Exp $
   ble.cmd:    
   A tool to build test examples from within the LessTif source tree
   with OS/2 EMX and XFree86OS/2.
   
   Written by
      Alexander Mai  
      amai@lesstif.org
      amai@users.sf.net
*/

/* 
   TODO:
     - accept compiler, linker flags
     - ...
*/

TRUE=1
FALSE=0

/* Defaults: */
DoDebug=FALSE
OMF=FALSE
OVersion=3 /* now default! */
InTesttree=TRUE
LinkLTTest=TRUE
LinkStatic=FALSE
LinkDbmalloc=FALSE
LinkDmalloc=FALSE

Parse Arg param

if param="" then
   do
   say "ble.cmd: Missing argument."
   say "Syntax:"
   say "ble.cmd   [/o]|[/a] [/1|2|3] [/n] file[.extension]"
   say "   Options:"
   say "      /1     Build against 1.2 version"
   say "      /2     Build against 2.0 version"
   say "      /3     Build against 2.1 version"
   say "      /a     Build a.out objects"
   say "      /b     Link dbmalloc"
   say "      /d     Link dmalloc"
   say "      /o     Build OMF objects"
   say "      /n     Don't link against libLTTest"
   say "      /s     Link statically"
   say ""
   say "   The following extensions are stripped off .c, .exe, .o "
   exit 0
   end

worte=Words(param)
do i=1 to worte
   argv=SubWord(param, i, 1)
   if argv="/1" then
      do
      OVersion=1
      end
   else if argv="/2" then
      do
      OVersion=2
      end
   else if argv="/3" then
      do
      OVersion=3
      end
   else if argv="/a" then
      do
      OMF=FALSE
      end
   else if argv="/b" then
      do
      LinkDbmalloc=TRUE
      end
   else if argv="/d" then
      do
      LinkDmalloc=TRUE
      end
   else if argv="/o" then
      do
      OMF=TRUE
      end
   else if argv="/n" then
      do
      LinkLTTest=FALSE
      end
   else if argv="/s" then
      do
      LinkStatic=TRUE
      end
   else if argv="/x" then
      do
      DoDebug=TRUE
      end
   else
      do
      leave
      end
end /* do i */

files=SubWord(param, i)

/* configuration */
CC="gcc"
DEBUG="-g"
CFLAGS="-Zmt -O0 -Wall -W -Wno-unused"
   LDFLAGS="-Zmt -Zcrtdll -Zbsd-signals -Zmap"
if OMF=TRUE then
   do
   CFLAGS=CFLAGS" -Zomf"
   LDFLAGS=LDFLAGS" -Zlinker /M:FULL -Zomf"
   end
else
   do
   /* nothing here */
   end

if right(files, 2)=".c" then
   do
   base = Left(files, lastpos(".c", files)-1)
   end
else if right(files, 4)=".exe" then
   do
   base = Left(files, lastpos(".exe", files)-1)
   end
else if right(files, 4)=".o" then
   do
   base = Left(files, lastpos(".o", files)-1)
   end
else if right(files, 4)=".obj" then
   do
   base = Left(files, lastpos(".obj", files)-1)
   end
else
   do
   base = files
   end

srcfile    = base".c"
if OMF=TRUE then
   objectfile = base".obj"
else
   objectfile = base".o"
exefile    = base".exe"

include_21="-I../../../include/Motif-2.1  -I../../../include/Motif-2.0   -I../../../include/Motif-1.2 -I../../../lib"
include_20="-I../../../include/Motif-2.0  -I../../../include/Motif-1.2"
include_12="-I../../../include/Motif-1.2"
include_xbae="-I../../include"
include_xlt="-I../../include"

lib_xm21="-L../../../lib/Xm-2.1 -lXm_21"
lib_xm20="-L../../../lib/Xm-2.0 -lXm_20"
lib_xm12="-L../../../lib/Xm     -lXm_12"

lib_mrm21="-L../../../lib/Mrm-2.1 -lMrm_21"
lib_mrm20="-L../../../lib/Mrm-2.0 -lMrm_20"
lib_mrm12="-L../../../lib/Mrm     -lMrm_12"

lib_uil21="-L../../../lib/Uil-2.1 -lUil_21"
lib_uil20="-L../../../lib/Uil-2.0 -lUil_20"
lib_uil12="-L../../../lib/Uil     -lUil_12"

dtlib="-L../../../lib/Dt -lDtPrint"

if LinkStatic then
   do
   lib_xbae="-L../../src -lXbae_s"
   lib_xlt="-L../../lib -lXlt_s"
   end
else
   do
   lib_xbae="-L../../src -lXbae"
   lib_xlt="-L../../lib -lXlt"
   end


curdir = directory()
if      Pos("\test\Xm-2.1", curdir)>0 then
   tree = "Xm-2.1"
else if Pos("\test\Xm-2.0", curdir)>0 then
   tree = "Xm-2.0"
else if Pos("\test\Xm", curdir)>0 then
   tree = "Xm"
else if Pos("\test\Mrm-2.1", curdir)>0 then
   tree = "Mrm-2.1"
else if Pos("\test\Mrm-2.0", curdir)>0 then
   tree = "Mrm-2.0"
else if Pos("\test\Mrm", curdir)>0 then
   tree = "Mrm"
else if Pos("\Xbae", curdir)>0 then
   tree = "Xbae"
else if Pos("\Xlt", curdir)>0 then
   tree = "Xlt"
else
   do
   tree=""
   InTesttree=FALSE
   end

if DoDebug then
   do
   say "param:"param
   say "base:"base
   say "tree:"tree
   end
   
x11root = Value("X11ROOT",,"OS2ENVIRONMENT")
if DoDebug then
   do
   say "X11ROOT="x11root
   end

if (tree="Xbae") then
   do
   gen_includes=include_xbae
   gen_libs=lib_xbae
   end
else if (tree="Xlt") then
   do
   gen_includes=include_xlt
   gen_libs=lib_xlt
   end
else if (\InTesttree | \LinkLTTest) then
   do
   gen_includes=""
   gen_libs=""
   end
else
   do
   gen_includes="-I. -I../../common"
   gen_libs="-L../../common -lLtTest "
   end

libs= gen_libs
/* There's no 2.x Uil or Mrm test tree */
if (OVersion=3) | (tree="Xm-2.1") then
   do
   includes = gen_includes" "include_21
   if (tree="Mrm") then
      libs     = lib_mrm21
   else if (tree="Uil") then
      libs     = lib_uil21
   libs = libs" "lib_xm21
   end
else if (OVersion=2) | (tree="Xm-2.0") then
   do
   includes = gen_includes" "include_20
   if (tree="Mrm") then
      libs     = lib_mrm20
   else if (tree="Uil") then
      libs     = lib_uil20
   libs = libs" "lib_xm20
   end
else
   do
   includes = gen_includes" "include_12
   if (tree="Mrm") then
      libs     = lib_mrm12
   else if (tree="Uil") then
      libs     = lib_uil12
   libs = libs" "lib_xm12
   end

if LinkDbmalloc then
   /* requires X11 itself */
   do
   libs=libs" -ldbmalloc"
   end

/* Add non-LessTif stuff */
includes = includes" -I"x11root"/XFree86/include"
/* -lXp might be required at some time in the future */
libs     = libs" -L"x11root"/XFree86/lib -lXext -lXt -lX11"

if LinkDmalloc then
   do
   libs=libs" -ldmalloc"
   end

/* execute the commands */
rc_cc = ExecCmd(CC CFLAGS   DEBUG "-c -o "objectfile  includes  srcfile)
rc_ld = ExecCmd(CC LDFLAGS  DEBUG "   -o "exefile               objectfile  libs)

rc_tot=rc_cc+10*rc_ld

exit rc_tot

/* ========================================================================= */

ExecCmd: Procedure

/* Execute a command properly and return it's return value */
Parse Arg cmdstring
"cmd /c "cmdstring
return rc

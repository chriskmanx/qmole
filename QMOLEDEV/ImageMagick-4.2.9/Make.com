$!
$! Make ImageMagick X image utilities for VMS.
$!
$
$ option := 'p1'
$ if option .eqs. "CLEAN"
$ then
$    deletee/log [.magick]libmagick.olb;*
$    exit
$ endif
$ if option .eqs. "REALCLEAN"
$ then
$    deletee/log [.magick]libmagick.olb;*,[...]*.obj;*
$    exit
$ endif
$ if option .eqs. "DISTCLEAN"
$ then
$    deletee/log [.magick]libmagick.olb;*,[...]*.obj;*,*.exe;*,magickshr.olb;*
$    exit
$ endif
$ if option .eqs. "NOSHR"
$ then
$    share := n
$    option :=
$ endif
$ if option .nes. ""
$ then
$    write sys$error "Unknown option \", option, "\"
$    exit
$ endif
$ p1 :=
$link_options="/nodebug/notraceback"
$if (f$trnlnm("X11") .eqs. "") then define/nolog X11 decw$include:
$library_options=""
$compile_options="/nodebug/optimize"
$if (f$search("sys$system:decc$compiler.exe") .nes. "")
$then       ! VAX with DEC C compiler
$  compile_options="/decc/nodebug/optimize"
$  library_options="_decc"
$else       ! VAX with VAX C compiler, (GCC library needed for PNG format only)
$  define/nolog lnk$library sys$library:vaxcrtl
$  define/nolog sys sys$share
$  if (f$trnlnm("gnu_cc") .nes. "") then define/nolog lnk$library_1 gnu_cc:[000000]gcclib.olb
$endif
$if (f$getsyi("HW_MODEL") .gt. 1023)
$then       ! Alpha with DEC C compiler
$  define/nolog sys decc$library_include
$  compile_options="/nodebug/optimize/prefix=all"
$  library_options="_axp"
$  share := 'share'y
$else
$  share := n
$endif
$
$write sys$output "Making in [.magick]"
$set default [.magick]
$@make
$set default [-]
$
$ if share
$ then
$    write sys$output "Making shareable image"
$    link/share/exe=magickshr.exe   [.magick]libmagick.olb/lib, -
  [.jpeg]libjpeg.olb'library_options'/lib, -
  [.png]libpng.olb'library_options'/lib, -
  [.tiff]libtiff.olb'library_options'/lib, -
  [.ttf]libttf.olb'library_options'/lib, -
  [.zlib]libz.olb'library_options'/lib, -
  []magickshr.opt/opt
$ libr/crea/share/log magickshr.olb magickshr.exe
$    set file/trunc magickshr.olb
$    purge magickshr.olb
$    link_libraries := magickshr.olb/lib
$    define/nolog magickshr 'f$environment("default")'magickshr.exe
$    write sys$output "Shareable image logical MAGICKSHR defined:"
$    show logi magickshr
$ else
$    link_libraries := [.magick]libmagick.olb/lib, -
  [.jpeg]libjpeg.olb'library_options'/lib, -
  [.png]libpng.olb'library_options'/lib, -
  [.tiff]libtiff.olb'library_options'/lib, -
  [.ttf]libttf.olb'library_options'/lib, -
  [.zlib]libz.olb'library_options'/lib
$ endif
$if ((p1 .nes. "") .and. (p1 .nes. "DISPLAY")) then goto SkipDisplay
$write sys$output "Making Display..."
$call Make display.c
$
$link'link_options' display.obj, -
  'link_libraries',sys$input:/opt
  sys$share:decw$xlibshr.exe/share
$
$display:==$'f$environment("default")'display
$write sys$output "..symbol DISPLAY defined."
$
$SkipDisplay:
$if ((p1 .nes. "") .and. (p1 .nes. "IMPORT")) then goto SkipImport
$write sys$output "Making Import..."
$call Make import.c
$
$link'link_options' import.obj, -
  'link_libraries',sys$input:/opt
  sys$share:decw$xlibshr.exe/share
$
$import:==$'f$environment("default")'import
$write sys$output "..symbol IMPORT defined."
$SkipImport:
$
$if ((p1 .nes. "") .and. (p1 .nes. "ANIMATE")) then goto SkipAnimate
$write sys$output "Making Animate..."
$call Make animate.c
$
$link'link_options' animate.obj, -
  'link_libraries',sys$input:/opt
  sys$share:decw$xlibshr.exe/share
$
$animate:==$'f$environment("default")'animate
$write sys$output "..symbol ANIMATE defined."
$
$SkipAnimate:
$if ((p1 .nes. "") .and. (p1 .nes. "MONTAGE")) then goto SkipMontage
$write sys$output "Making Montage..."
$call Make montage.c
$
$link'link_options' montage.obj, -
  'link_libraries',sys$input:/opt
  sys$share:decw$xlibshr.exe/share
$
$montage:==$'f$environment("default")'montage
$write sys$output "..symbol MONTAGE defined."
$
$SkipMontage:
$if ((p1 .nes. "") .and. (p1 .nes. "MOGRIFY")) then goto SkipMogrify
$write sys$output "Making Mogrify..."
$call Make mogrify.c
$
$link'link_options' mogrify.obj, -
  'link_libraries',sys$input:/opt
  sys$share:decw$xlibshr.exe/share
$
$mogrify:==$'f$environment("default")'mogrify
$write sys$output "..symbol MOGRIFY defined."
$
$SkipMogrify:
$if ((p1 .nes. "") .and. (p1 .nes. "CONVERT")) then goto SkipConvert
$write sys$output "Making Convert..."
$call Make convert.c
$
$link'link_options' convert.obj, -
  'link_libraries',sys$input:/opt
  sys$share:decw$xlibshr.exe/share
$
$convert:==$'f$environment("default")'convert
$write sys$output "..symbol CONVERT defined."
$SkipConvert:
$if ((p1 .nes. "") .and. (p1 .nes. "IDENTIFY")) then goto SkipIdentify
$write sys$output "Making Identify..."
$call Make identify.c
$
$link'link_options' identify.obj, -
  'link_libraries',sys$input:/opt
  sys$share:decw$xlibshr.exe/share
$
$identify:==$'f$environment("default")'identify
$write sys$output "..symbol IDENTIFY defined."
$SkipIdentify:
$if ((p1 .nes. "") .and. (p1 .nes. "COMBINE")) then goto SkipCombine
$write sys$output "Making Combine..."
$call Make combine.c
$
$link'link_options' combine.obj, -
  'link_libraries',sys$input:/opt
  sys$share:decw$xlibshr.exe/share
$
$combine:==$'f$environment("default")'combine
$write sys$output "..symbol COMBINE defined."
$SkipCombine:
copy [.delegates]vms.mgk sys$login:delegates.mgk
$type sys$input

Use this command to specify which X server to contact:

  $set display/create/node=node_name::

or

  $set display/create/node=nodename/transport=tcpip

This can be done automatically from your LOGIN.COM with the following
command:

  $if (f$trnlmn("sys$rem_node") .nes. "") then -
  $  set display/create/node='f$trnlmn("sys$rem_node")'
$exit
$
$Make: subroutine
$!
$! Primitive MMS hack for DCL.
$!
$if (p1 .eqs. "") then exit
$source_file=f$search(f$parse(p1,".c"))
$if (source_file .nes. "")
$then
$  object_file=f$parse(source_file,,,"name")+".obj"
$  object_file=f$search( object_file )
$  if (object_file .nes. "")
$  then
$    object_time=f$file_attribute(object_file,"cdt")
$    source_time=f$file_attribute(source_file,"cdt")
$    if (f$cvtime(object_time).lts.f$cvtime(source_time)) then -
$      object_file=""
$  endif
$  if (object_file .eqs. "")
$  then
$    write sys$output "Compiling ",p1
$    cc'compile_options'/include_directory=[.magick] 'source_file'
$  endif
$endif
$exit
$endsubroutine

$!
$! Make ImageMagick X image utilities for VMS.
$!
$
$if (f$trnlnm("X11") .eqs. "") then define/nolog X11 decw$include:
$compile_options="/nodebug/optimize"
$if (f$search("sys$system:decc$compiler.exe") .nes. "") 
$then     ! VAX with DEC C
$  compile_options="/decc/nodebug/optimize"
$else     ! VAX with VAX C
$define/nolog lnk$library sys$library:vaxcrtl
$define/nolog sys sys$share
$endif
$if (f$getsyi("HW_MODEL") .gt. 1023)
$then     ! Alpha with DEC C
$  define/nolog sys decc$library_include
$  compile_options="/nodebug/optimize/prefix=all"
$endif
$
$write sys$output "Making Magick..."
$call Make PreRvIcccm.c
$call Make animate.c
$call Make annotate.c
$call Make avs.c
$call Make blob.c
$call Make bmp.c
$call Make cmyk.c
$call Make colors.c
$call Make compress.c
$call Make dcm.c
$call Make decorate.c
$call Make delegates.c
$call Make display.c
$call Make dps.c
$call Make draw.c
$call Make effects.c
$call Make enhance.c
$call Make ept.c
$call Make error.c
$call Make fax.c
$call Make fits.c
$call Make fpx.c
$call Make gems.c
$call Make gif.c
$call Make gradation.c
$call Make gray.c
$call Make hdf.c
$call Make histogram.c
$call Make html.c
$call Make icc.c
$call Make icon.c
$call Make image.c
$call Make iptc.c
$call Make jbig.c
$call Make jpeg.c
$call Make label.c
$call Make logo.c
$call Make magick.c
$call Make map.c
$call Make matte.c
$call Make memory.c
$call Make miff.c
$call Make monitor.c
$call Make mono.c
$call Make montage.c
$call Make mtv.c
$call Make null.c
$call Make pcd.c
$call Make pcl.c
$call Make pcx.c
$call Make pdf.c
$call Make pict.c
$call Make pix.c
$call Make plasma.c
$call Make png.c
$call Make pnm.c
$call Make preview.c
$call Make ps.c
$call Make ps2.c
$call Make ps3.c
$call Make psd.c
$call Make pwp.c
$call Make quantize.c
$call Make rgb.c
$call Make rla.c
$call Make rle.c
$call Make segment.c
$call Make sct.c
$call Make sfw.c
$call Make sgi.c
$call Make shear.c
$call Make signature.c
$call Make stegano.c
$call Make sun.c
$call Make tga.c
$call Make tiff.c
$call Make tile.c
$call Make tim.c
$call Make transform.c
$call Make ttf.c
$call Make txt.c
$call Make uil.c
$call Make utility.c
$call Make uyvy.c
$call Make vicar.c
$call Make vid.c
$call Make viff.c
$call Make vms.c
$call Make widget.c
$call Make x.c
$call Make xbm.c
$call Make xc.c
$call Make xpm.c
$call Make xwd.c
$call Make xwindows.c
$call Make yuv.c
$call Make zoom.c
$library/create libmagick.olb PreRvIcccm.obj,animate.obj,annotate.obj, -
  avs.obj,blob.obj,bmp.obj,cmyk.obj,colors.obj,compress.obj,dcm.obj, -
  decorate.obj,delegates.obj,display.obj,dps.obj,draw.obj, -
  effects.obj,enhance.obj,ept.obj,error.obj,fax.obj,fits.obj, -
  fpx.obj,gems.obj,gif.obj,gradation.obj,gray.obj,hdf.obj, -
  histogram.obj,html.obj,icc.obj,icon.obj,image.obj,iptc.obj, -
  jbig.obj,jpeg.obj,label.obj,logo.obj,magick.obj, -
  map.obj,matte.obj,memory.obj,miff.obj,monitor.obj,mono.obj, -
  montage.obj,mtv.obj,null.obj,pcd.obj,pcl.obj,pcx.obj, -
  pdf.obj,pict.obj,pix.obj,plasma.obj,png.obj,pnm.obj, -
  preview.obj,ps.obj,ps2.obj,ps3.obj,psd.obj,pwp.obj,quantize.obj, -
  rgb.obj,rla.obj,rle.obj,segment.obj,sct.obj,sfw.obj,sgi.obj,shear.obj, -
  signature.obj,stegano.obj,sun.obj,tga.obj,tiff.obj,tile.obj, -
  tim.obj,transform.obj,ttf.obj,txt.obj,uil.obj,utility.obj, -
  uyvy.obj,vicar.obj,vid.obj,viff.obj,vms.obj,widget.obj,x.obj, -
  xbm.obj,xc.obj,xpm.obj,xwd.obj,xwindows.obj,yuv.obj,zoom.obj
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
$    if (f$cvtime(object_time) .lts. f$cvtime(source_time)) then -
$      object_file=""
$  endif
$  if (object_file .eqs. "")
$  then
$    write sys$output "Compiling ",p1
$    cc'compile_options'/include_directory=([-],[-.jpeg],[-.png], -
       [-.tiff],[-.ttf],[-.zlib])/define=("HasX11","HasJPEG","HasLZW", -
       "HasPNG","HasTIFF","HasTTF","HasZLIB") 'source_file'  
$  endif
$endif
$exit
$endsubroutine

#!/usr/local/bin/perl
#
# Test writing formats supported directly by ImageMagick
#
# Currently supported tests are for formats that ImageMagick
# knows how to both read and write.
#
# Whenever a new test is added/removed, be sure to update the
# 1..n ouput.

BEGIN { $| = 1; $test=1; print "1..32\n"; }
END {print "not ok $test\n" unless $loaded;}
use Image::Magick;
$loaded=1;

require 't/subroutines.pl';

chdir 't' || die 'Cd failed';

#
# AVS X image file
#
testReadWrite( 'AVS:input.avs',
               'AVS:output.avs',
               q//,
               'e9b162b8431709942886f79b7b7c11bb' );

#
# Microsoft Windows bitmap image file
#
++$test;
testReadWrite( 'BMP:input.bmp',
               'BMP:output.bmp',
               q//,
               '7785d161ea66bd39394bb74ef704a0aa' );

#
# Microsoft Windows 24-bit bitmap image file
#
++$test;
testReadWrite( 'BMP24:input.bmp24',
               'BMP24:output.bmp24',
               q//,
               'e9b162b8431709942886f79b7b7c11bb');

#
# ZSoft IBM PC multi-page Paintbrush file
#
++$test;
testReadWrite( 'DCX:input.dcx',
               'DCX:output.dcx',
               q//,
               'e9b162b8431709942886f79b7b7c11bb' );

#
# Microsoft Windows bitmap image file
#
++$test;
testReadWrite( 'DIB:input.dib',
               'DIB:output.dib',
               q//,
               'e9b162b8431709942886f79b7b7c11bb' );

#
# Flexible Image Transport System
#
++$test;
testReadWrite( 'FITS:input.fits',
               'FITS:output.fits',
               q//,
               '402072120da1513decd3efc060473f60' );

#
# CompuServe graphics interchange format
#
++$test;
testReadWrite( 'GIF:input.gif',
               'GIF:output.gif',
               q//,
               'd5a97a3cec301d8848102f513ceb3f3c' );

#
# CompuServe graphics interchange format (1987)
#
++$test;
testReadWrite( 'GIF87:input.gif87',
               'GIF87:output.gif87',
               q//,
               'd5a97a3cec301d8848102f513ceb3f3c' );

#
# Magick image file format
#
++$test;
testReadWrite( 'MIFF:input.miff',
               'MIFF:output.miff',
               q//,
               '7785d161ea66bd39394bb74ef704a0aa' );

#
# MTV Raytracing image format
#
++$test;
testReadWrite( 'MTV:input.mtv',
               'MTV:output.mtv',
               q//,
               'e9b162b8431709942886f79b7b7c11bb' );

#
# Xv's visual schnauzer format.
#
++$test;
testReadWrite( 'P7:input_p7.p7',
               'P7:output_p7.p7',
               q//,
               '511e64f36f12cb8283b48bc3153aa98a' );

#
# Portable bitmap format (black and white), ASCII format
#
++$test;
testReadWrite( 'PBM:input_p1.pbm',
               'PBM:output_p1.pbm',
               q/compress=>'None'/,
               '61ed9c1e08dac993e5f020de8195e009' );

#
# Portable bitmap format (black and white), binary format
#
++$test;
testReadWrite( 'PBM:input_p4.pbm',
               'PBM:output_p4.pbm',
               q//,
               '61ed9c1e08dac993e5f020de8195e009' );

#
# ZSoft IBM PC Paintbrush file
#
++$test;
testReadWrite( 'PCX:input.pcx',
               'PCX:output.pcx',
               q//,
               'e9b162b8431709942886f79b7b7c11bb' );

#
# Portable graymap format (gray scale), ASCII format
#
++$test;
testReadWrite( 'PGM:input_p2.pgm',
               'PGM:output_p2.pgm',
               q/compress=>'None'/,
               '4d21ec5c0ed543eeb6d478515c66ff14' );

#
# Apple Macintosh QuickDraw/PICT file
#
++$test;
testReadWrite( 'PICT:input.pict',
               'PICT:output.pict',
               q//,
               '7785d161ea66bd39394bb74ef704a0aa' );

#
# Portable pixmap format (color), ASCII format
#
++$test;
testReadWrite( 'PPM:input_p3.ppm',
               'PPM:output_p3.ppm',
               q/compress=>'None'/,
               'e9b162b8431709942886f79b7b7c11bb' );

#
# Portable graymap format (gray scale), binary format
#
++$test;
testReadWrite( 'PGM:input_p5.pgm',
               'PGM:output_p5.pgm',
               q//,
               '4d21ec5c0ed543eeb6d478515c66ff14' );

#
# Portable pixmap format (color), binary format
#
++$test;
testReadWrite( 'PPM:input_p6.ppm',
               'PPM:output_p6.ppm',
               q//,
               'e9b162b8431709942886f79b7b7c11bb' );

#
# Adobe Photoshop bitmap file
#
++$test;
testReadWrite( 'PSD:input.psd',
               'PSD:output.psd',
               q//,
               '7785d161ea66bd39394bb74ef704a0aa' );

#
# Irix RGB image file
#
++$test;
testReadWrite( 'SGI:input.sgi',
               'SGI:output.sgi',
               q//,
               'e9b162b8431709942886f79b7b7c11bb' );

#
# SUN 1-bit Rasterfile
#
++$test;
testReadWrite( 'SUN:input.im1',
               'SUN:output.im1',
               q//,
               '61ed9c1e08dac993e5f020de8195e009' );

#
# SUN 8-bit Rasterfile
#
++$test;
testReadWrite( 'SUN:input.im8',
               'SUN:output.im8',
               q//,
               '7785d161ea66bd39394bb74ef704a0aa' );

#
# SUN True-Color Rasterfile
#
++$test;
testReadWrite( 'SUN:input.im24',
               'SUN:output.im24',
               q//,
               'e9b162b8431709942886f79b7b7c11bb' );

#
# Truevision Targa image file
#
++$test;
testReadWrite( 'TGA:input.tga',
               'TGA:output.tga',
               q//,
               'e9b162b8431709942886f79b7b7c11bb' );


#
# Khoros Visualization image file
#
++$test;
testReadWrite( 'VIFF:input.viff',
               'VIFF:output.viff',
               q//,
               'bba2e7661b8dd0b8006442bebcdf285d' );
#
# X Windows system bitmap (black and white only)
#
++$test;
testReadWrite( 'XBM:input.xbm',
               'XBM:output.xbm',
               q//,
               '61ed9c1e08dac993e5f020de8195e009' );

#
# X Windows system pixmap file (color)
#
++$test;
testReadWrite( 'XPM:input.xpm',
               'XPM:output.xpm',
               q//,
               '7785d161ea66bd39394bb74ef704a0aa' );

#
# X Windows system window dump file (color)
#
++$test;
testReadWrite( 'XWD:input.xwd',
               'XWD:output.xwd',
               q//,
               'e9b162b8431709942886f79b7b7c11bb' );

#
# GRAY format
#
++$test;
testReadWriteSized( 'GRAY:input_70x46.gray',
		    'GRAY:output_70x46.gray',
		    '70x46',
		    q//,
		    '17e5358049cdf0613e54f0f2eb42701d' );

#
# RGB format
#
++$test;
testReadWriteSized( 'RGB:input_70x46.rgb',
		    'RGB:output_70x46.rgb',
		    '70x46',
		    q//,
		    'e9b162b8431709942886f79b7b7c11bb' );

#
# RGBA format
#
++$test;
testReadWriteSized( 'RGBA:input_70x46.rgba',
		    'RGBA:output_70x46.rgba',
		    '70x46',
		    q//,
		    'f3e5ab88e328da3044c1fc177a7e2978' );

1;

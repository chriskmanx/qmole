#!/usr/local/bin/perl
#
# Test reading formats supported directly by ImageMagick
#
# Whenever a new test is added/removed, be sure to update the
# 1..n ouput.
#
BEGIN { $| = 1; $test=1; print "1..44\n"; }
END {print "not ok $test\n" unless $loaded;}
use Image::Magick;
$loaded=1;

require 't/subroutines.pl';

chdir 't' || die 'Cd failed';

#
# AVS X image file
#
testRead( 'input.avs', 'e9b162b8431709942886f79b7b7c11bb' );

#
# Microsoft Windows bitmap image file
#
++$test;
testRead( 'input.bmp', '7785d161ea66bd39394bb74ef704a0aa' );

#
# Microsoft Windows 24-bit bitmap image file
#
++$test;
testRead( 'input.bmp24', 'e9b162b8431709942886f79b7b7c11bb' );

#
# ZSoft IBM PC multi-page Paintbrush file
#
++$test;
testRead( 'input.dcx', 'e9b162b8431709942886f79b7b7c11bb' );

#
# Microsoft Windows bitmap image file
#
++$test;
testRead( 'input.dib', 'e9b162b8431709942886f79b7b7c11bb' );

#
# Flexible Image Transport System
#
++$test;
testRead( 'input.fits', '402072120da1513decd3efc060473f60' );

#
# CompuServe graphics interchange format
#
++$test;
testRead( 'input.gif', 'd5a97a3cec301d8848102f513ceb3f3c' );

#
# CompuServe graphics interchange format (1987)
#
++$test;
testRead( 'input.gif87', 'd5a97a3cec301d8848102f513ceb3f3c' );

#
# GRADATION (gradual passing from one shade to another)
# This is an internal, computed, format
#
++$test;
testRead( 'gradation:red-blue', '120b84fd035a74aac8769761e21a16b2' );

#
# GRANITE (granite texture)
# This is an internal, computed, format
#
++$test;
testRead( 'granite:', '30086994324339ccb531dbb82e46128b' );

#
# Microsoft icon
#
++$test;
testRead( 'input.ico', 'd6fb93a149f45c8002358d58cd2db2c7' );

#
# Magick image file format
#
++$test;
testRead( 'input.miff', '7785d161ea66bd39394bb74ef704a0aa' );

#
# MTV Raytracing image format
#
++$test;
testRead( 'input.mtv', 'e9b162b8431709942886f79b7b7c11bb' );

#
# Xv's visual schnauzer format.
#
++$test;
testRead( 'input_p7.p7', '3d772946085f47ed244c548d08e36579' );

#
# NULL (white image)
# This is an internal generated format
#
++$test;
testRead( 'NULL:white', 'ab0b2d4302f691199ab052eeb9de155b' );

#
# NULL (black image)
#
++$test;
testRead( 'NULL:black', 'cb18a5d28e77522dfec6a6255bc3847e' );

#
# NULL (DarkOrange image)
#
++$test;
testRead( 'NULL:DarkOrange', 'cbcd8c4b1122863d2e89149074881e15' );

#
# Portable bitmap format (black and white), ASCII format
#
++$test;
testRead( 'input_p1.pbm', '61ed9c1e08dac993e5f020de8195e009' );


#
# Portable bitmap format (black and white), binary format
#
++$test;
testRead( 'input_p4.pbm', '61ed9c1e08dac993e5f020de8195e009' );

#
# ZSoft IBM PC Paintbrush file
#
++$test;
testRead( 'input.pcx', 'e9b162b8431709942886f79b7b7c11bb' );

#
# Portable graymap format (gray scale), ASCII format
#
++$test;
testRead( 'input_p2.pgm', '4d21ec5c0ed543eeb6d478515c66ff14' );

#
# Portable graymap format (gray scale), binary format
#
++$test;
testRead( 'input_p5.pgm', '4d21ec5c0ed543eeb6d478515c66ff14' );

#
# Apple Macintosh QuickDraw/PICT file
#
++$test;
testRead( 'input.pict', '7785d161ea66bd39394bb74ef704a0aa' );

#
# Alias/Wavefront RLE image format
#
++$test;
testRead( 'input.rle', 'e9b162b8431709942886f79b7b7c11bb' );

#
# Portable pixmap format (color), ASCII format
#
++$test;
testRead( 'input_p3.ppm', 'e9b162b8431709942886f79b7b7c11bb' );

#
# Portable pixmap format (color), binary format
#
++$test;
testRead( 'input_p6.ppm', 'e9b162b8431709942886f79b7b7c11bb' );

#
# Adobe Photoshop bitmap file
#
++$test;
testRead( 'input.psd', '7785d161ea66bd39394bb74ef704a0aa' );

#
# Irix RGB image file
#
++$test;
testRead( 'input.sgi', 'e9b162b8431709942886f79b7b7c11bb' );

#
# SUN 1-bit Rasterfile
#
++$test;
testRead( 'input.im1', '61ed9c1e08dac993e5f020de8195e009' );

# SUN 8-bit Rasterfile
#
++$test;
testRead( 'input.im8', '7785d161ea66bd39394bb74ef704a0aa' );

# SUN TrueColor Rasterfile
#
++$test;
testRead( 'input.im24', 'e9b162b8431709942886f79b7b7c11bb' );

#
# Truevision Targa image file
#
++$test;
testRead( 'input.tga', 'e9b162b8431709942886f79b7b7c11bb' );

#
# PSX TIM file
#
++$test;
testRead( 'input.tim', '5f53f9f45afef1060449dd4f09c3706c' );

#
# Khoros Visualization image file
#
++$test;
testRead( 'input.viff', '703151c1beed89a570ae56f1670a486a' );

#
# X Windows system bitmap (black and white only)
#
++$test;
testRead( 'input.xbm', '61ed9c1e08dac993e5f020de8195e009' );

#
# XC: Constant image of X server color
#
++$test;
testRead( 'xc:black', 'cb18a5d28e77522dfec6a6255bc3847e' );

#
# X Windows system pixmap file (color)
#
++$test;
testRead( 'input.xpm', '7785d161ea66bd39394bb74ef704a0aa' );

#
# X Windows system window dump file (color)
#
++$test;
testRead( 'input.xwd', 'e9b162b8431709942886f79b7b7c11bb' );

#
# TILE (Tile image with a texture)
# This is an internal generated format
# We will tile using the default image and a MIFF file
#
++$test;
testReadSized( 'TILE:input.miff', '140x92', 'ea3df20e9ae6e1b6fc948d1ee64b571c');

#
# GRAY format
#
++$test;
testReadSized( 'input_70x46.gray', '70x46', '17e5358049cdf0613e54f0f2eb42701d' );

#
# RGB format
#
++$test;
testReadSized( 'input_70x46.rgb', '70x46', 'e9b162b8431709942886f79b7b7c11bb' );

#
# RGBA format
#
++$test;
testReadSized( 'input_70x46.rgba', '70x46', 'f3e5ab88e328da3044c1fc177a7e2978' );


#
# CMYK format
#
++$test;
testReadSized( 'input_70x46.cmyk', '70x46', '70b0c3d7fcc7a617481285a1e27345d5' );


#
# UYVY format
#
++$test;
testReadSized( 'input_70x46.uyvy', '70x46', '81990d39685ec85b67d606a4bd35fc9c' );

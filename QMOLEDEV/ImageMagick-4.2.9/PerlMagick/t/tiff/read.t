#!/usr/local/bin/perl
#
# Test reading TIFF images
#
# Contributed by Bob Friesenhahn <bfriesen@simple.dallas.tx.us>
#
BEGIN { $| = 1; $test=1; print "1..3\n"; }
END {print "not ok $test\n" unless $loaded;}

use Image::Magick;
$loaded=1;

require 't/subroutines.pl';

chdir 't/tiff' || die 'Cd failed';

#
# 1) Test Reading Monochrome
# 
testRead ( 'input_mono.tiff', 'f194ead12be5179c3930fa7a59e9af45' );

#
# 2) Test reading PseudoColor
#
++$test;
testRead( 'input_256.tiff', '6098df40d515cdc35541d05928c0ea1b' );

#
# 3) Test Reading TrueColor
# 
++$test;
testRead( 'input_truecolor.tiff', 'ae1ef2c41e1750186bc03d867a9b47bf' );


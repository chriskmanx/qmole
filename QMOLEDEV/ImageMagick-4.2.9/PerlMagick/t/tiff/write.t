#!/usr/local/bin/perl
#
# Test writing TIFF images
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
# 1) Test pseudocolor image
#
testReadWrite( 'input_256.tiff',
	       'output_256.tiff',
	       q/quality=>54/,
	       '6098df40d515cdc35541d05928c0ea1b' );

#
# 2) Test truecolor image
#
++$test;
testReadWrite( 'input_truecolor.tiff',
	       'output_truecolor.tiff',
	       q/quality=>55/,
	       'ae1ef2c41e1750186bc03d867a9b47bf' );

#
# 3) Test monochrome image
#
++$test;
testReadWrite(  'input_mono.tiff',
		'output_mono.tiff',
		q//,
		'f194ead12be5179c3930fa7a59e9af45' );

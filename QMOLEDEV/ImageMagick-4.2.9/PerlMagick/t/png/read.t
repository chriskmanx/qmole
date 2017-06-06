#!/usr/local/bin/perl
#
# Test reading PNG images
#
# Contributed by Bob Friesenhahn <bfriesen@simple.dallas.tx.us>
#

BEGIN { $| = 1; $test=1; print "1..4\n"; }
END {print "not ok $test\n" unless $loaded;}
use Image::Magick;
$loaded=1;

require 't/subroutines.pl';

chdir 't/png' || die 'Cd failed';

#
# 1) Test Monochrome PNG
# 
testRead( 'input_mono.png', '8d63ef0e910116f485da5a8b6a3cc7a9' );

#
# 2) Test 256 color pseudocolor PNG
# 
++$test;
testRead( 'input_256.png', '0acbc70bac502726c1b72d3c3ff4d0fe' );

#
# 3) Test TrueColor PNG
# 
++$test;
testRead( 'input_truecolor.png', '3ada2b040cb9b94b2426b2681cd39106' );

#
# 4) Test Multiple-image Network Graphics
# 
++$test;
testRead( 'input.mng', '2ab9b274742f981c30ff745cfc355ad8' );


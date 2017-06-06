#!/usr/local/bin/perl
#
# Test getting attributes.
#
# Contributed by Bob Friesenhahn <bfriesen@simple.dallas.tx.us>
#
BEGIN { $| = 1; $test=1, print "1..29\n"; }
END {print "not ok 1\n" unless $loaded;}
use Image::Magick;
$loaded=1;

require 't/subroutines.pl';

chdir 't' || die 'Cd failed';

testGetAttribute('input.miff','base_columns','70');

++$test;
testGetAttribute('input.miff','base_filename','input.miff');

++$test;
testGetAttribute('input.miff','base_rows','46');

++$test;
testGetAttribute('input.miff','class','PseudoClass');

++$test;
testGetAttribute('input.miff','colors','256');

++$test;
testGetAttribute('input.miff','columns','70');

++$test;
testGetAttribute('input.miff','comment',q/
This is a comment.
/);

++$test;
testGetAttribute('input.miff','directory',undef);

++$test;
testGetAttribute('input.miff','filesize','4145');

++$test;
testGetAttribute('input.miff','gamma','0');

++$test;
testGetAttribute('input.miff','geometry',undef);

++$test;
testGetAttribute('input.miff','height','46');

++$test;
# Retuns undef
testGetAttribute('input.miff','label',undef);

++$test;
testGetAttribute('input.miff','matte','0');

++$test;
testGetAttribute('input.miff','mean','0');

++$test;
testGetAttribute('input.miff','montage',undef);

++$test;
testGetAttribute('input.miff','normalized_max','0');

++$test;
testGetAttribute('input.miff','normalized_mean','0');

++$test;
testGetAttribute('input.miff','packetsize','1');

++$test;
testGetAttribute('input.miff','packets','2534');

++$test;
testGetAttribute('input.miff','rows','46');

++$test;
testGetAttribute('input.miff','signature','7785d161ea66bd39394bb74ef704a0aa');

++$test;
testGetAttribute('input.miff','text',undef);

++$test;
testGetAttribute('input.miff','type','Palette');

++$test;
testGetAttribute('input.miff','units','undefined units');

++$test;
testGetAttribute('input.miff','view',undef);

++$test;
testGetAttribute('input.miff','width','70');

++$test;
testGetAttribute('input.miff','x_resolution','0');

++$test;
testGetAttribute('input.miff','y_resolution','0');

1;

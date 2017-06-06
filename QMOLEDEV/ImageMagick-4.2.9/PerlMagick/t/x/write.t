#!/usr/local/bin/perl
#
# Test accessing X11 server
#
# Contributed by Bob Friesenhahn <bfriesen@simple.dallas.tx.us>
#

BEGIN { $| = 1; $test=1; print "1..1\n"; }
END {print "not ok $test\n" unless $loaded;}
use Image::Magick;
$loaded=1;

require 't/subroutines.pl';

chdir 't/x' || die 'Cd failed';

#
# 1) Test reading and displaying an image
#
$image=Image::Magick->new;
$x=$image->ReadImage('congrats.miff');
if( "$x" ) {
  print "not ok $test\n";
} else {
  $x = $image->Display(delay=>800);
  if( "$x" ) {
    print "not ok $test\n";
  } else {
    print "ok $test\n";
  }
}
undef $image;

$test = 0;  # Quench PERL compliaint


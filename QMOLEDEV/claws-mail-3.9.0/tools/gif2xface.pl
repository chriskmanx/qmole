#!/usr/bin/perl
#
# gif2xface -- converts a 48x48 GIF file to an X-Face mail header
#
# Author:  Ricardo Mones Lastra <mones@aic.uniovi.es>
#
# URL:     http://www.aic.uniovi.es/mones
#
#   This is a hack over the original xbm2face script. The xbm files generated
#   by some graphic tools (notably The Gimp version 1.2.1) aren't suitable to
#   feed the compface program. Starting with a GIF and using some filters does
#   the trick. A little help screen also added.
#   This requieres giftopnm and pbmtoxbm (both in libgr-progs package). 
#
#   The original xbm2face author's comment follows: 
#
# xbm2xface -- converts a 48x48 xbm file to an X-Face mail header
#
# Author:  Jonathan Stigelman <Stig@hackvan.com>
#
# URL:     http://hackvan.com/pub/stig/src/linux/
# FTP:	   hackvan.com:/pub/stig/src/linux/
#
#   This is a Perl script that I wrote to convert 48x48 xbm (X bitmap) files
#   into X-Face: headers suitable for inclusion in RFC822 internet
#   electronic mail.  A 48x48 bitmap is awfully small, but X-Faces are still
#   good enough for other people to visually establish your identity in
#   email without having to carefully read your name.
# 
#   Basically, it gets you noticed...either as the person with the cool
#   X-Face or as that jerk with modem noise in all of his email messages.
# 
#   People will start looking over your shoulder and say "Hey Cool!  How'd
#   you do that?"  When they do, you just send 'em to my URL for this
#   utility and tell 'em to upgrade to a real mail reader: XEmacs and VM.
#
# It also requires the 'compface' utility.
#

sub check_for_help {
  local($param) = @_;

  # is a filter, no args must be present
  if (defined($param))
  { 
    print "\n", 'gif2xface -- A filter for converting an 48x48 gif into a xface string', "\n\n" ;
    print 'Usage:  gif2xface < input.gif > output.xface', "\n\n";
    exit;
  }
}

sub reverse_byte {
    local($byte) = @_;
    local($n, $b);
    for ( $b= $n= 0; $b<8; ++$b) {
	$n |= (($byte & 1) << (7-$b));
	$byte >>= 1;
    }
    return($n);
}


&check_for_help($ARGV[0]);

# printf "0x%02x\n", &reverse_byte(0xF0);

$ra = rand;
$tf = "/tmp/gif2xface.$ra";
open(GP,"|giftopnm|pbmtoxbm>$tf");

while (<>) {
  print GP $_;
}
close(GP);
open(GP,"<$tf");
<GP>;
m/^#define \w+_width (\d+)/ && ($width=$1);
<GP>;
m/^#define \w+_height (\d+)/ && ($height=$1);
<GP>;
m/^static.* = \{/ && (( $width == 48 && $height == 48 )
		      || die $0, ": sorry, xfaces must be 48x48 pixels" );

$| = 1; print "X-Face:";

open(CF,"|compface");

while (<GP>) {
    $st="";
    while (s/(0x..)(,|\};)\s*//) {
	$st .= sprintf("0x%02x, ", &reverse_byte(eval($1)));
    }
    $_=$st;
    s/(0x..), 0x(..)/\1\2/g;
    s/\s*(0x...., 0x...., 0x....)(,|\};)\s/\1,\n/g;
    print CF $_;
}
close (CF);
close (GP);
unlink $tf;

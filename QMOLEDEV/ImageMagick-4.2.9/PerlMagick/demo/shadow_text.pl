#!/usr/local/bin/perl
#
# Make simple text with a shadow.
#
use Image::Magick;

$image=Image::Magick->new(size=>'300x100');
$image->Read('xc:white');
$image->Annotate(font=>'@Generic.ttf', pen=>'black',pointsize=>60,
  gravity=>'Center',text=>'Magick');
$mask=$image->Clone();
$mask->Negate();
$mask->Blur();
$image->Roll('+5+5');
$image->Composite(image=>$mask,compose=>'add');
$image->Write('shadow.gif');
$image->display();

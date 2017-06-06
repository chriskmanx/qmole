#!/usr/local/bin/perl
#
# Make simple beveled button.
#
use Image::Magick;

$q=Image::Magick->new;
$q->Set(size=>'30x105');
$q->Read('gradation:#00f685-#0083f8');
$q->Rotate(-90);
$q->Raise('6x6');
$q->Annotate(text=>'Push Me',font=>'@Generic.ttf',pen=>'black',
  gravity=>'Center');
$q->Write('button.gif');
$q->display();

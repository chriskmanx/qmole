#!/usr/local/bin/perl

use Image::Magick;

#
# Create watermark.
#
$watermark=Image::Magick->new;
$watermark->ReadImage('smile.gif');
($width, $height)=$watermark->Get('width','height');
#
# Hide watermark in image.
#
$image=Image::Magick->new;
$image->ReadImage('model.gif');
$image->SteganoImage(image=>$watermark,offset=>91);
$image->Write('model.png');
$image->Display();
#
# Extract watermark from image.
#
$size="$width" . "x" . "$height" . "+91";
$stegano=Image::Magick->new(size=>$size);
$stegano->ReadImage('stegano:model.png');
$stegano->Display();

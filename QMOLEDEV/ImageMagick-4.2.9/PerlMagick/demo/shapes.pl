#!/usr/local/bin/perl
# GD example using PerlMagick methods.

use Image::Magick;

#
# Create a 300x300 white canvas.
#
$image=Image::Magick->new;
$image->Set(size=>'300x300');
$image->Read('xc:white');
#
# Draw shapes.
#
$image->Draw(pen=>'@tile.gif',primitive=>'FillPolygon',
  points=>'30,30 100,10 190,290 30,290');
$image->Draw(pen=>'red',primitive=>'FillEllipse',
  points=>'100,100 100,150 0,360');
$image->Draw(pen=>'black',primitive=>'Ellipse',
  points=>'100,100 100,150 0,360',linewidth=>5);
$image->Draw(pen=>'black',primitive=>'Polygon',
  points=>'30,30 100,10 190,290 30,290 30,30',linewidth=>5);
$image->Set(fuzz=>80);
$image->ColorFloodfill(geometry=>'+132+62',pen=>'blue');
$image->Annotate(pen=>'red',geometry=>'+150+10',font=>'@Generic.ttf',
  pointsize=>12,text=>'Hello world!');
$image->Annotate(pen=>'blue',geometry=>'+150+28',font=>'@Generic.ttf',
  pointsize=>10,text=>'Goodbye cruel world!');
#
# Write rotated text.
#
$image->Rotate(90);
$image->Annotate(pen=>'black',geometry=>'+20+280',font=>'@Generic.ttf',
  pointsize=>10,text=>"I'm climbing the wall!");
$image->Rotate(-90);
#
# Write image.
#
print "Write image...\n";
$image->Write('shapes.gif');
print "Display image...\n";
$image->Display();

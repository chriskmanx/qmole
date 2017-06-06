#!/usr/local/bin/perl
# Piddle example using PerlMagick methods.

use Image::Magick;

#
# Create white canvas.
#
$image=Image::Magick->new(size=>'300x300');
$image->Read('xc:white');
#
# Draw blue grid
#
for ($i=0; $i < 300; $i+=10)
{
  $image->Draw(primitive=>'line',points=>"$i,0 $i,300",pen=>"#ccf");
  $image->Draw(primitive=>'line',points=>"0,$i 300,$i",pen=>"#ccf");
}
#
# Draw rectangle.
#
$image->Draw(primitve=>'fillrectangle',points=>'30,30 100,100',pen=>blue);
$image->Draw(primitve=>'rectangle',points=>'30,30 100,100',pen=>maroon,
  linewidth=>4);
#
# Draw line.
#
$image->Draw(primitive=>'line',points=>"10,200 20,190",pen=>red);
#
# Draw circle.
#
$image->Draw(primitive=>'fillcircle',points=>"165,70 200,70",pen=>yellow);
$image->Draw(primitive=>'circle',points=>"165,70 200,70",pen=>black,
  linewidth=>4);
#
# Draw Arc.
#
$image->Draw(primitve=>'line',points=>'165,65 165,35',pen=>blue);
$image->Draw(primitve=>'line',points=>'165,65 190,45',pen=>blue);
$image->Set(fuzz=>80);
$image->Draw(primitive=>'color',points=>'170,55',pen=>blue,method=>'floodfill');
$image->Set(fuzz=>0);
#
# Draw pentogram.
#
$image->Draw(primitive=>'polygon',
  points=>"160,120 130,190 210,145 110,145 190,190 160,120",pen=>red,
  linewidth=>3);
$image->Draw(primitive=>'color',points=>'160,140',pen=>LimeGreen,
  method=>'filltoborder',bordercolor=>red);
$image->Draw(primitive=>'color',points=>'190,155',pen=>LimeGreen,
  method=>'filltoborder',bordercolor=>red);
$image->Draw(primitive=>'color',points=>'140,170',pen=>LimeGreen,
  method=>'filltoborder',bordercolor=>red);
$image->Draw(primitive=>'color',points=>'130,155',pen=>LimeGreen,
  method=>'filltoborder',bordercolor=>red);
$image->Draw(primitive=>'color',points=>'180,170',pen=>LimeGreen,
  method=>'filltoborder',bordercolor=>red);
#
# Draw rectangle.
#
$image->Draw(primitve=>'rectangle',points=>'200,200 260,260',pen=>yellow,
  linewidth=>5);
$image->Draw(primitve=>'line',points=>'200,260 260,260',pen=>green,
  linewidth=>5);
$image->Draw(primitve=>'line',points=>'260,200 260,260',pen=>red,linewidth=>5);
#
# Draw text.
#
$image->Annotate(text=>'This is a test!',geometry=>'+30+130',
  font=>'@Generic.ttf',pen=>'green',degrees=>45.0);
$image->Display();

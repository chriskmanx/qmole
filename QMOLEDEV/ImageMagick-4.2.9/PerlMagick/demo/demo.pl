#!/usr/local/bin/perl
#
# Overall demo of the major PerlMagick methods.
#
use Image::Magick;

#
# Read model & smile image.
#
print "Read images...\n";
$null=Image::Magick->new;
$null->Set(size=>'70x70');
$x=$null->ReadImage('NULL:black');
warn "$x" if "$x";
$model=Image::Magick->new;
$x=$model->ReadImage('model.gif');
warn "$x" if "$x";
$model->Label('Magick');
$model->Set(bordercolor=>'black');
$model->Set(background=>'black');
$smile=Image::Magick->new;
$x=$smile->ReadImage('smile.gif');
warn "$x" if "$x";
$smile->Label('Smile');
$smile->Set(bordercolor=>'black');
#
# Create image stack.
#
print "Transform image...\n";
$images=Image::Magick->new;
$example=$null->Clone();
push(@$images,$example);
$example=$null->Clone();
push(@$images,$example);
$example=$null->Clone();
push(@$images,$example);
$example=$null->Clone();
push(@$images,$example);
$example=$null->Clone();
push(@$images,$example);
$example=$model->Clone();
$example->Label('Annotate');
$example->Annotate(text=>'Magick',geometry=>'+0+10',font=>'@Generic.ttf',
  pen=>'gold',gravity=>'North');
push(@$images,$example);
$example=$model->Clone();
$example->Label('Blur');
$example->Blur(60);
push(@$images,$example);
$example=$model->Clone();
$example->Label('Border');
$example->Border(color=>'gold');
push(@$images,$example);
$example=$model->Clone();
$example->Label('Charcoal');
$example->Charcoal();
push(@$images,$example);
$example=$model->Clone();
$example->Label('Composite');
$example->Composite(image=>$smile,compose=>'over',geometry=>'+35+65');
push(@$images,$example);
$example=$model->Clone();
$example->Label('Crop');
$example->Crop(geometry=>'80x80+25+50');
push(@$images,$example);
$example=$model->Clone();
$example->Label('Despeckle');
$example->Despeckle();
push(@$images,$example);
$example=$model->Clone();
$example->Label('Draw');
$example->Draw(pen=>'gold',primitive=>'circle',points=>'60,90 60,120',
  linewidth=>2);
push(@$images,$example);
$example=$model->Clone();
$example->Label('Emboss');
$example->Emboss();
push(@$images,$example);
$example=$model->Clone();
$example->Label('Equalize');
$example->Equalize();
push(@$images,$example);
$example=$model->Clone();
$example->Label('Explode');
$example->Implode(-10);
push(@$images,$example);
$example=$model->Clone();
$example->Label('Flip');
$example->Flip();
push(@$images,$example);
$example=$model->Clone();
$example->Label('Flop');
$example->Flop();
push(@$images,$example);
$example=$model->Clone();
$example->Label('Frame');
$example->Frame();
push(@$images,$example);
$example=$model->Clone();
$example->Label('Gamma');
$example->Gamma(1.6);
push(@$images,$example);
$gradation=Image::Magick->new;
$gradation->Set(size=>'130x194');
$x=$gradation->ReadImage('gradation:#20a0ff-#ffff00');
warn "$x" if "$x";
$gradation->Label('Gradation');
push(@$images,$gradation);
$example=$model->Clone();
$example->Label('Grayscale');
$example->Quantize(colorspace=>'gray');
push(@$images,$example);
$example=$model->Clone();
$example->Label('Implode');
$example->Implode(30);
push(@$images,$example);
$example=$model->Clone();
$example->Label('Modulate');
$example->Modulate(brightness=>-3,saturation=>-3,hue=>-3);
push(@$images,$example);
$example=$model->Clone();
$example->Label('Monochrome');
$example->Quantize(colorspace=>'gray',colors=>2,dither=>'false');
push(@$images,$example);
$example=$model->Clone();
$example->Label('Negate');
$example->Negate();
push(@$images,$example);
$example=$model->Clone();
$example->Label('Normalize');
$example->Normalize();
push(@$images,$example);
$example=$model->Clone();
$example->Label('Oil Paint');
$example->OilPaint();
push(@$images,$example);
$plasma=Image::Magick->new;
$plasma->Set(size=>'130x194');
$x=$plasma->ReadImage('plasma:fractal');
warn "$x" if "$x";
$plasma->Label('Plasma');
push(@$images,$plasma);
$example=$model->Clone();
$example->Label('Raise');
$example->Raise();
push(@$images,$example);
$example=$model->Clone();
$example->Label('Roll');
$example->Roll(geometry=>'+20+10');
push(@$images,$example);
$example=$model->Clone();
$example->Label('Rotate');
$example->Rotate(45);
$example->Transparent(color=>'black');
push(@$images,$example);
$example=$model->Clone();
$example->Label('Segment');
$example->Segment();
push(@$images,$example);
$example=$model->Clone();
$example->Label('Shade');
$example->Shade(geometry=>'30x30',color=>'false');
push(@$images,$example);
$example=$model->Clone();
$example->Label('Sharpen');
$example->Sharpen(60);
push(@$images,$example);
$example=$model->Clone();
$example->Label('Shear');
$example->Shear('45x45');
$example->Transparent(color=>'black');
push(@$images,$example);
$example=$model->Clone();
$example->Label('Spread');
$example->Spread();
push(@$images,$example);
$example=$model->Clone();
$example->Label('Solarize');
$example->Solarize();
push(@$images,$example);
$example=$model->Clone();
$example->Label('Swirl');
$example->Swirl(90);
push(@$images,$example);
$example=$model->Clone();
$example->Label('Wave');
$example->Wave('25x150');
push(@$images,$example);
$example=$model->Clone();
$example->Label('Zoom');
$example->Zoom('50%');
push(@$images,$example);
#
# Create title.
#
print "Annotate image...\n";
$background=Image::Magick->new;
$background->Set(size=>'550x90');
$x=$background->ReadImage('gradation:#20a0ff-#ffff00');
warn "$x" if "$x";
$title=Image::Magick->new;
$title->Set(size=>'550x90');
$x=$title->ReadImage('xc:black');
warn "$x" if "$x";
$title->Annotate(text=>'PerlMagick',geometry=>"+1+1",font=>'@Generic.ttf',
  pointsize=>18,density=>'300x300',pen=>'white',gravity=>'center');
$title->Draw(primitive=>'Matte',points=>'+0+0',method=>'Replace',pen=>'black');
$title->Composite(image=>$background,compose=>'Add');
#
# Create image montage.
#
print "Montage image...\n";
$montage=$images->montage(filename=>'PerlMagick',geometry=>'130x194+10+5>',
  gravity=>'Center',bordercolor=>'green',borderwidth=>1,tile=>'5x1000',
  compose=>'over',texture=>'granite:',font=>'@Generic.ttf',pen=>'#600');
$montage->Composite(image=>$title,geometry=>'+90+50',compose=>'Over');
$montage->Annotate(text=>'Every thing you see on this page was created ' .
  'with the PerlMagick and ImageMagick toolkits.',geometry=>"+20+175",
  font=>'@Generic.ttf',pointsize=>11,pen=>'#600');
print "Write image...\n";
$montage->Set(matte=>'false');
$montage->Write('demo.jpg');
print "Display image...\n";
$montage->display();

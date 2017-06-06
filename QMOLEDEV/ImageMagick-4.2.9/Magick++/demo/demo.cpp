// This may look like C code, but it is really -*- C++ -*-
//
// Copyright Bob Friesenhahn, 1999
//
// Simple demo program for Magick++
//
// Concept and algorithms lifted from PerlMagick demo script written
// by John Christy.
//

#include <string>
#include <iostream>
#include <list>

#include <Magick++.h>

using namespace std;

using namespace Magick;

int main( int /*argc*/, char ** /*argv*/)
{

  try {

    //
    // Read model & smile image.
    //
    cout << "Read images..." << endl;
    Image null;
    null.size( "70x70" );
    null.read( "null:black" );

    Image model( "model.gif" );
    model.label( "Magick++" );
    model.borderColor( "black" );
    model.backgroundColor( "black" );

    Image smile( "smile.gif" );
    smile.label( "Smile" );
    smile.borderColor( "black" );

    //
    // Create image stack.
    //
    cout << "Transform image..." << endl;

    // Construct list containing five copies of null image
    list<Image> images( 5, null );

    Image example = model;

    cout << "  annotate ..." << endl;
    example.label( "Annotate" );
    example.font( "@Generic.ttf" );
    example.penColor( "gold" );
    example.annotate( "Magick++", "+0+10", NorthGravity );
    images.push_back( example );

    cout << "  blur ..." << endl;
    example = model;
    example.label( "Blur" );
    example.blur( 60 );
    images.push_back( example );

    cout << "  border ..." << endl;
    example = model;
    example.label( "Border" );
    example.borderColor( "gold" );
    example.border( );
    images.push_back( example );

    cout << "  charcoal ..." << endl;
    example = model;
    example.label( "Charcoal" );
    example.charcoal( );
    images.push_back( example );

    cout << "  composite ..." << endl;
    example = model;
    example.label( "Composite" );
    example.composite( smile, "+35+65", OverCompositeOp);
    images.push_back( example );

    cout << "  crop ..." << endl;
    example = model;
    example.label( "Crop" );
    example.crop( "80x80+25+50" );
    images.push_back( example );

    cout << "  despeckle ..." << endl;
    example = model;
    example.label( "Despeckle" );
    example.despeckle( );
    images.push_back( example );

    cout << "  draw ..." << endl;
    example = model;
    example.label( "Draw" );
    example.penColor( "gold" );
    example.lineWidth( 2 );
    Drawable drawable;
    drawable.circle( 60,90, 60,120 );
    example.draw( drawable );
    images.push_back( example );

    cout << "  emboss ..." << endl;
    example = model;
    example.label( "Emboss" );
    example.emboss( );
    images.push_back( example );

    cout << "  equalize ..." << endl;
    example = model;
    example.label( "Equalize" );
    example.equalize( );
    images.push_back( example );

    cout << "  explode ..." << endl;
    example = model;
    example.label( "Explode" );
    example.implode( -10 );
    images.push_back( example );

    cout << "  flip ..." << endl;
    example = model;
    example.label( "Flip" );
    example.flip( );
    images.push_back( example );

    cout << "  flop ..." << endl;
    example = model;
    example.label( "Flop" );
    example.flop();
    images.push_back( example );

    cout << "  frame ..." << endl;
    example = model;
    example.label( "Frame" );
    example.frame( );
    images.push_back( example );

    cout << "  gamma ..." << endl;
    example = model;
    example.label( "Gamma" );
    example.gamma( 1.6 );
    images.push_back( example );

    cout << "  gradation ..." << endl;
    Image gradation;
    gradation.size( "130x194" );
    gradation.read( "gradation:#20a0ff-#ffff00" );
    gradation.label( "Gradation" );
    images.push_back( gradation );

    cout << "  grayscale ..." << endl;
    example = model;
    example.label( "Grayscale" );
    example.quantizeColorSpace( GRAYColorspace );
    example.quantize( );
    images.push_back( example );

    cout << "  implode ..." << endl;
    example = model;
    example.label( "Implode" );
    example.implode( 30 );
    images.push_back( example );

    cout << "  modulate ..." << endl;
    example = model;
    example.label( "Modulate" );
    example.modulate( -3, -3, -3 );
    images.push_back( example );

    cout << "  monochrome ..." << endl;
    example = model;
    example.label( "Monochrome" );
    example.quantizeColorSpace( GRAYColorspace );
    example.quantizeColors( 2 );
    example.quantizeDither( false );
    example.quantize( );
    images.push_back( example );

    cout << "  negate ..." << endl;
    example = model;
    example.label( "Negate" );
    example.negate( );
    images.push_back( example );

    cout << "  normalize ..." << endl;
    example = model;
    example.label( "Normalize" );
    example.normalize( );
    images.push_back( example );

    cout << "  oil paint ..." << endl;
    example = model;
    example.label( "Oil Paint" );
    example.oilPaint( );
    images.push_back( example );

    cout << "  plasma ..." << endl;
    Image plasma;
    plasma.size( "130x194" );
    plasma.read( "plasma:fractal" );
    plasma.label( "Plasma" );
    images.push_back( plasma );

    cout << "  raise ..." << endl;
    example = model;
    example.label( "Raise" );
    example.raise( );
    images.push_back( example );

    cout << "  roll ..." << endl;
    example = model;
    example.label( "Roll" );
    example.roll( "+20+10" );
    images.push_back( example );

    cout << "  rotate ..." << endl;
    example = model;
    example.label( "Rotate" );
    example.rotate( 45 );
    example.transparent( "black" );
    images.push_back( example );

    cout << "  segment ..." << endl;
    example = model;
    example.label( "Segment" );
    // Segment is *extremely* slow at 16-bit color so skip
    // it in that case.
    if ( example.depth() == 16 )
      {
	example.penColor( "gold" );
	example.annotate( "Skipped", CenterGravity );
      }
    else
      {
	example.segment( );
      }
    images.push_back( example );

    cout << "  shade ..." << endl;
    example = model;
    example.label( "Shade" );
    example.shade( 30, 30, false );
    images.push_back( example );

    cout << "  sharpen ..." << endl;
    example = model;
    example.label("Sharpen");
    example.sharpen( 60 );
    images.push_back( example );

    cout << "  shear ..." << endl;
    example = model;
    example.label( "Shear" );
    example.shear( 45, 45 );
    example.transparent( "black" );
    images.push_back( example );

    cout << "  spread ..." << endl;
    example = model;
    example.label( "Spread" );
    example.spread( );
    images.push_back( example );

    cout << "  solarize ..." << endl;
    example = model;
    example.label( "Solarize" );
    example.solarize( );
    images.push_back( example );

    cout << "  swirl ..." << endl;
    example = model;
    example.label( "Swirl" );
    example.swirl( 90 );
    images.push_back( example );

    cout << "  wave ..." << endl;
    example = model;
    example.label( "Wave" );
    example.wave( 25, 150 );
    images.push_back( example );

    cout << "  zoom ..." << endl;
    example = model;
    example.label( "Zoom" );
    example.zoom( "50%" );
    images.push_back( example );

//     {
//       list<Image> temp = images;
//       animateImages( temp.begin(), temp.end() );
//     }
//     {
//       list<Image> temp = images;
//       displayImages( temp.begin(), temp.end() );
//     }
//     {
//       list<Image> temp = images;
//       writeImages( temp.begin(), temp.end(), "demo-anim.gif", true );
//     }

    //
    // Create title.
    //
    cout << "Annotate image..." << endl;
    Image background;
    background.size( "550x90" );
    background.read( "gradation:#20a0ff-#ffff00" );

    Image title;
    title.size( "550x90" );
    title.read( "xc:black" );

    title.font( "@Generic.ttf" );
    title.fontPointsize( 18 );
    title.density( "300x300" );
    title.penColor( "white" );
    title.annotate( "Magick++", "+1+1", CenterGravity );

    title.penColor( "black" );
    drawable.matte( 0, 0, ReplaceMethod );
    title.draw( drawable );
    title.composite( background, 0, 0, AddCompositeOp );

    //
    // Create image montage.
    //
    cout <<  "Montage images..." << endl;

    list<Image> montage;

    Image& imageRef = images.front();
    imageRef.font( "@Generic.ttf" );
    imageRef.penColor( "#600" );

    MontageFramed montageOpts;
    montageOpts.borderColor( "green" );
    montageOpts.borderWidth( 1 );
    montageOpts.compose( OverCompositeOp );
    montageOpts.fileName( "Magick++" );
    montageOpts.font( "@Generic.ttf" );
    montageOpts.geometry( "130x194+10+5>" );
    montageOpts.gravity( CenterGravity );
    montageOpts.penColor( "#600" );
    montageOpts.texture( "granite:" );
    montageOpts.tile( "5x1000" );
    montageImages( &montage, images.begin(), images.end(), montageOpts );

    // Modify montage image to avoid extra copy.
    Image& final = montage.front();
    final.composite( title, "+90+50", OverCompositeOp );

    final.font( "@Generic.ttf" );
    final.fontPointsize( 11 );
    final.penColor( "#600" );
    final.annotate( "Every thing you see on this page was created with the "\
		    "Magick++ and ImageMagick libraries.",
		    "+20+175" );

    cout << "Write image \"demo.jpg\" ..." << endl;
    final.matte( false );
    final.write( "demo.jpg" );

//    cout <<  "Display image..." << endl;
//    final.display();

  }
  catch( Magick::Exception error_ )
    {
      cout << "Caught exception: " << error_.what() << endl;
      return 1;
    }
  catch( exception error_ )
    {
      cout << "Caught exception: " << error_.what() << endl;
      return 1;
    }
  
  return 0;
}

// This may look like C code, but it is really -*- C++ -*-
//
// Copyright Bob Friesenhahn, 1999
//
// GD/PerlMagick example using Magick++ methods.
//
// Concept and algorithms lifted from PerlMagick demo script
//

#include <string>
#include <iostream>

#include <Magick++.h>

using namespace std;

using namespace Magick;

int main( int /*argc*/, char ** /*argv*/)
{

  try {

    //
    // Create a 300x300 white canvas.
    //
    Image image( "300x300", "white" );

    //
    // Draw shapes.
    //
    Drawable drawable;
    std::list<Coordinate> poly;
    poly.push_back( Coordinate(30,30) );
    poly.push_back( Coordinate(100,10) );
    poly.push_back( Coordinate(190,290) );
    poly.push_back( Coordinate(30,290) );
    drawable.fillPolygon( poly );

    Image texture( "tile.gif" );
    image.penTexture( texture );
    image.draw( drawable );
    texture.isValid( false );
    image.penTexture( texture );  // Unset texture

    image.penColor( "red" );
    drawable.fillEllipse( 100,100, 100,150, 0,360 );
    image.draw( drawable );

    image.penColor( "black" );
    image.lineWidth( 5 );

    // Create drawables list
    list<Drawable> drawlist;

    // Add ellipse to drawables list
    drawable.ellipse( 100,100, 100,150, 0,360 );
    drawlist.push_back( drawable );

    // Add arbitrary polygon to drawables list
    poly.clear();
    poly.push_back( Coordinate(30,30) );
    poly.push_back( Coordinate(100,10) );
    poly.push_back( Coordinate(190,290) );
    poly.push_back( Coordinate(30,290) );
    poly.push_back( Coordinate(30,30) );
    drawable.polygon( poly );
    drawlist.push_back( drawable );

    // Draw using drawables list
    image.draw( drawlist );

    image.colorFuzz( 80 );
    image.floodFillColor( "+132+62", "blue" );

    image.penColor( "red" );
    image.font( "@Generic.ttf" );
    image.fontPointsize( 12 );
    image.annotate( "Hello world!", "+150+10" );

    image.penColor( "blue" );
    image.font( "@Generic.ttf" );
    image.fontPointsize( 10 );
    image.annotate( "Goodbye cruel world!", "+150+28" );

    //
    // Write rotated text.
    //
    image.rotate( 90 );
    image.penColor( "black" );
    image.font( "@Generic.ttf" );
    image.fontPointsize( 10 );
    image.annotate( "I'm climbing the wall!", "+20+280" );
    image.rotate( -90 );

    //
    // Write image.
    //

    cout << "Write image..." << endl;
    image.write( "shapes.gif" );

    cout << "Display image..." << endl;
    image.display( );

  }
  catch( Exception error_ )
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

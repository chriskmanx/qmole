// !/usr/local/bin/perl
//
// Magick++ demo to generate a simple text button
//
// Bob Friesenhahn, 1999
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
    // Options
    //

    string backGround = "xc:#CCCCCC"; // A solid color

    // Color to use for decorative border
    Color border = "#D4DCF3";

    // Button size
    string buttonSize = "120x20";

    // Button background texture
    string buttonTexture = "granite:";

    // Button text
    string text = "Button Text";

    // Button text color
    string textColor = "red";

    // Font to use for text
    string textFont = "@Generic.ttf";

    // Font point size
    int textFontPointSize = 10;

    //
    // Magick++ operations
    //

    Image button;

    // Set button size
    button.size( buttonSize );

    // Read background image
    button.read( backGround );

    // Set background to buttonTexture
    Image backgroundTexture( buttonTexture );
    button.texture( backgroundTexture );

    // Add some text
    button.penColor( textColor );
    button.font( textFont );
    button.fontPointsize( textFontPointSize );
    button.annotate( text, CenterGravity );

    // Add a decorative frame
    button.borderColor( border );
    button.frame( "6x6+3+3" );

    // Quantize to desired colors
    button.quantizeColors(16);
    button.quantize();

    // Save to file
    button.write("GIF:button.gif");

    // Display on screen
    button.display();

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

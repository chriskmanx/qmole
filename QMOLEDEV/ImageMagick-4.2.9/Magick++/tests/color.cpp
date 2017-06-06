// This may look like C code, but it is really -*- C++ -*-
//
// Copyright Bob Friesenhahn, 1999
//
// Test Magick::Color classes
//

#include <string>
#include <iostream>

#include <Magick++.h>

using namespace std;

using namespace Magick;

int main( int /*argc*/, char ** /*argv*/)
{

  int failures=0;

  try {

    //
    // Verify conversion from named colors as well as ColorRGB constructor
    //

    {
      struct colorStr
      {
	const char* color;
	double red;
	double green;
	double blue;
      };

      // Convert ratios from rgb.txt via value/255
      struct colorStr colorMap [] =
      { 
	{ "red", 1,0,0 },
	{ "green", 0,1,0 },
	{ "blue", 0,0,1 },
	{ "black", 0,0,0 },
	{ "white", 1,1,1 },
	{ "cyan", 0,1,1 },
	{ "magenta", 1,0,1 },
	{ "yellow", 1,1,0 },
	{ NULL, 0,0,0 }
      };

      for ( int i = 0; colorMap[i].color != NULL; i++ )
	{
	  {
	    Color color( colorMap[i].color );
	    ColorRGB colorMatch( colorMap[i].red,
				 colorMap[i].green,
				 colorMap[i].blue );
	    if ( color != colorMatch )
	      {
		++failures;
		cout << "Line: " << __LINE__ << " Color(\""
		     << colorMap[i].color << "\") is "
		     << string(color)
		     << " rather than "
		     << string(colorMatch)
		     << endl;
	      }
	  }
	}      
    }

    // Test conversion to/from X11-style color specifications
    {
      const char * colorStrings[] =
      {
	{ "#ABC" },
	{ "#AABBCC" },
	{ "#AAAABBBBCCCC" },
	{ NULL }
      };

      string expectedString;
      {
	Image image( "1x1", "white" );
	if ( image.depth() == 16 )
	  expectedString = "#AAAABBBBCCCC";
	else
	  expectedString = "#AABBCC";
      }

      for ( int i = 0; colorStrings[i] != NULL; ++i )
	{
	  if ( string(Color(colorStrings[i])) != expectedString )
	    {
	      ++failures;
	      cout << "Line: " << __LINE__
		   << " Conversion from " << colorStrings[i]
		   << " is "
		   << string(Color(colorStrings[i])) << " rather than "
		   << expectedString
		   << endl;
	    }
	}
    }

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
  
  if ( failures )
    {
      cout << failures << " failures" << endl;
      return 1;
    }

  return 0;
}

// This may look like C code, but it is really -*- C++ -*-
//
// Copyright Bob Friesenhahn, 1999
//
// Demonstration of unary function-object based operations
//
// Reads the multi-frame file "smile_anim.gif" and writes a
// flipped and morphed version to "flip_out.gif".
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

    // Read images into STL list
    list<Image> imageList;
    readImages( &imageList, "smile_anim.gif" );

    // Flip images
    for_each( imageList.begin(), imageList.end(), flipImage() );

    // Create a morphed version, adding three frames between each
    // existing frame.
    list<Image> morphed;
    morphImages( &morphed, imageList.begin(), imageList.end(), 3 );

    // Write out images
    writeImages( morphed.begin(), morphed.end(), "flip_out.gif" );

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

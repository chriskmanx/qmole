// This may look like C code, but it is really -*- C++ -*-
//
// Copyright Bob Friesenhahn, 1999
//
// Simple image manipulation test program for Magick++
//

#include <string>
#include <iostream>
#include <list>
#include <vector>

#include <Magick++.h>

using namespace std;

using namespace Magick;

int main( int /*argc*/, char ** /*argv*/)
{
  int failures=0;

  try {

    {
      //
      // Test appendImages
      //

      list<Image> imageList;
      readImages( &imageList, "test_image_anim.gif" );

      Image appended;

      // Horizontal
      appendImages( &appended, imageList.begin(), imageList.end() );
      // appended.display();
      if ( appended.signature() != "0e600860c5cb47cd5e2377c015753d3c" &&
	   appended.signature() != "eed68e3f06a225f24d8aa56d9a2848db" )
	{
	  ++failures;
	  cout << "Line: " << __LINE__
	       << "  Horizontal append failed, signature = "
	       << appended.signature() << endl;
	}

      // Vertical
      appendImages( &appended, imageList.begin(), imageList.end(), true );
      if ( appended.signature() != "b65606228af103f08d21d437725ba90d" &&
	   appended.signature() != "29b2e6300df902a177440a4e9c7dbbbc" )
	{
	  ++failures;
	  cout << "Line: " << __LINE__
	       << "  Vertical append failed, signature = "
	       << appended.signature() << endl;
	}
    }

    {
      //
      // Test averageImages
      //

      list<Image> imageList;
      readImages( &imageList, "test_image_anim.gif" );

      Image averaged;
      averageImages( &averaged, imageList.begin(), imageList.end() );
      // averaged.display();
      if ( averaged.signature() != "678f291ed463419f4cfe26289b99552f" &&
	   averaged.signature() != "990a6b8f3abca6d06936b29c7a5a41af")
	{
	  ++failures;
	  cout << "Line: " << __LINE__
	       << "  Averaging image failed, signature = "
	       << averaged.signature() << endl;
	}
    }

    {
      //
      // Test coalesceImages
      //

      list<Image> imageList;
      readImages( &imageList, "test_image_anim.gif" );

      coalesceImages( imageList.begin(), imageList.end() );
    }

    {
      //
      // Test montageImages
      //

      list<Image> imageList;
      readImages( &imageList, "test_image_anim.gif" );

      vector<Image> montage;
      MontageFramed montageOpts;

      // Default montage
      montageImages( &montage, imageList.begin(), imageList.end(), montageOpts );

      if ( montage[0].montageGeometry() != Geometry( 114, 112 ) )
	{
	  ++failures;
	  cout << "Line: " << __LINE__ 
	       << "  Montage geometry ("
	       << string(montage[0].montageGeometry())
	       << ") is incorrect."
	       << endl;
	}

      if ( montage[0].columns() != 684 || montage[0].rows() != 114 )
	{
	  ++failures;
	  cout << "Line: " << __LINE__ 
	       << "  Montage columns/rows ("
	       << montage[0].columns() << "x"
	       << montage[0].rows()
	       << ") incorrect." << endl;
	}

      // Montage with options set
      montage.clear();
      montageOpts.borderColor( "green" );
      montageOpts.borderWidth( 1 );
      montageOpts.compose( OverCompositeOp );
      montageOpts.fileName( "Montage" );
      montageOpts.frameGeometry( "6x6+3+3" );
      montageOpts.geometry("50x50+2+2>");
      montageOpts.gravity( CenterGravity );
      montageOpts.penColor( "yellow" );
      montageOpts.shadow( true );
      montageOpts.texture( "granite:" );
      montageOpts.tile("2x1");
      montageImages( &montage, imageList.begin(), imageList.end(), montageOpts );

      if ( montage.size() != 3 )
	{
	  ++failures;
	  cout << "Line: " << __LINE__ 
	       << "  Montage images failed, number of montage frames is "
	       << montage.size()
	       << " rather than 3 as expected." << endl;
	}

      if ( montage[0].montageGeometry() != Geometry( 66, 70 ) )
	{
	  ++failures;
	  cout << "Line: " << __LINE__ 
	       << "  Montage geometry ("
	       << string(montage[0].montageGeometry())
	       << ") is incorrect."
	       << endl;
	}

      if ( montage[0].columns() != 132 || montage[0].rows() != 72 )
	{
	  ++failures;
	  cout << "Line: " << __LINE__ 
	       << "  Montage columns/rows ("
	       << montage[0].columns() << "x"
	       << montage[0].rows()
	       << ") incorrect." << endl;
	}
    }

    {
      //
      // Test morphImages
      //

      list<Image> imageList;
      readImages( &imageList, "test_image_anim.gif" );

      list<Image> morphed;
      morphImages( &morphed, imageList.begin(), imageList.end(), 3 );

      if ( morphed.size() != 21 )
	{
	  ++failures;
	  cout << "Line: " << __LINE__ 
	       << "  Morph images failed, number of frames is "
	       << morphed.size()
	       << " rather than 21 as expected." << endl;
	}
    }

    {
      //
      // Test readImages and writeImages
      //
      
      list<Image> first;
      readImages( &first, "test_image_anim.gif" );
      
      if ( first.size() != 6 )
	{
	  ++failures;
	  cout << "Line: " << __LINE__ 
	       << "  Read images failed, number of frames is "
	       << first.size()
	       << " rather than 6 as expected." << endl;
	}
      
      writeImages( first.begin(), first.end(), "testmagick_anim_out.gif" );
      
      list<Image> second;
      readImages( &second, "testmagick_anim_out.gif" );
      
      list<Image>::iterator firstIter = first.begin();
      list<Image>::iterator secondIter = second.begin();
      while( firstIter != first.end() && secondIter != second.end() )
	{
	  if ( *firstIter != *secondIter )
	    {
	      ++failures;
	      cout << "Line: " << __LINE__
		   << "  Image scene: " << secondIter->scene()
		   << " is not equal to original" << endl;
	    }

	  if ( firstIter->scene() != secondIter->scene() )
	    {
	      ++failures;
	      cout << "Line: " << __LINE__ 
		   << "  Image scene: " << secondIter->scene()
		   << " is not equal to original "
		   << firstIter->scene()
		   << endl;
	    }

	  if ( firstIter->rows() != secondIter->rows() )
	    {
	      ++failures;
	      cout << "Line: " << __LINE__ 
		   << "  Image rows " << secondIter->rows()
		   << " are not equal to original "
		   << firstIter->rows()
		   << endl;
	    }

	  if ( firstIter->columns() != secondIter->columns() )
	    {
	      ++failures;
	      cout << "Line: " << __LINE__ 
		   << "  Image columns " << secondIter->columns()
		   << " are not equal to original "
		   << firstIter->rows()
		   << endl;
	    }

	  if ( firstIter->packets() != secondIter->packets() )
	    {
	      ++failures;
	      cout << "Line: " << __LINE__ 
		   << "  Image packets " << secondIter->packets()
		   << " are not equal to original "
		   << firstIter->packets()
		   << endl;
	    }

	  firstIter++;
	  secondIter++;
	}
    }
  }
  catch( ErrorFileOpen error_ )
    {
      cout << "Caught ExceptionFileOpen: " << error_ << endl;
      return 1;
    }
  catch( Exception error_ )
    {
      cout << "Caught exception: " << error_ << endl;
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

#if 0
//
// Hack for linking with libraries compiled with GNU C when using Sun Workshop compiler.
// This gets things compiled but that doesn't mean the code will run ...
//
extern "C" {
  int __eprintf( const char *format, /* args */ ... );
}
int __eprintf( const char */*format*/, /* args */ ... )
{
  return 0;
}
#endif // out

#if 0
//
// This particular hack allows compiling with support for namespaces enabled
// using egcs 1.1.1.
// In order for it to work, the compilation line must include:
//
//  -fhonor-std -U__HONOR_STD
//
// This *only* works because code in the library looks like:
//
// #ifdef __HONOR_STD
// namespace std {
// #endif
//
// As it turns out, only the exception code has namespace support at this time.
// The end result is that only namespaces defined by the local library
// will be observed.
//
namespace std {
  void terminate()
  {
    exit(1);
  }
}
#endif

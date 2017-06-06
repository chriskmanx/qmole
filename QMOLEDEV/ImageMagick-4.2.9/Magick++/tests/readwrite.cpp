// This may look like C code, but it is really -*- C++ -*-
//
// Copyright Bob Friesenhahn, 1999
//
// Test reading/writing images using Magick++
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

#include <magick/config.h>

  try {
    
    {
      //
      // Test reading and writing BLOBs
      //


      Image master;
      Blob blob;
      // Read image from file
      master.read( "test_image.miff" );
      Geometry size( master.columns(), master.rows() );

      // Formats to test
      string formats[] =
      {
	"AVS",
	"BIE",
	"BMP",
	"CMYK",
	"DCX",
	"DIB",
	"FITS",
	"GIF",
	"GIF87",
	"GRAY",
#ifdef HasHDF
	"HDF",
#endif // HasHDF
	"ICB",
#ifdef HasJBIG
	"JBG",
	"JBIG",
#endif // HasJBIG
#ifdef HasJPEG
	"JPG",
	"JPEG",
	"JPEG24",
#endif // HasJPEG
	"MIFF",
	"MNG",
 	"MONO",  // cores sometimes
	"MTV",
	"P7",
	"PBM",
 	"PCD",
 	"PCDS",
	"PCT",  // generates bad (almost random) image
	"PCX",
	"PIC",
	"PICT",
	"PGM",
	"PM",
#ifdef HasPNG
	"PNG",
#endif // HasPNG
	"PNM",
	"PPM",
	"PSD",
	"RAS",
	"RGB",
	"RGBA",
	"SGI",
	"SUN",
#ifdef HasTIFF
	// TIFF BLOBs are not supported yet.
//  	"FAX",
//  	"G3",
//  	"PTIFF",
//  	"TIFF",
//  	"TIF",
//  	"TIFF24",
#endif // HasTIFF
	"TGA",
	"UYVY",
	"VDA",
	"VICAR",
	"VIFF",
	"VST",
	"XBM",
	"XPM",
	"XV",
	"XWD",
	"YUV",
	""
      };

      for ( unsigned int i = 0 ; formats[i].length() > 0 ; ++i )
	{
	  try {
	    string format = formats[ i ];
	    cout << "Format \"" << format << "\"" << endl;

	    bool needSize = false;
	    if ( format == "CMYK" ||
		 format == "GRAY" ||
		 format == "MONO" ||
		 format == "RGB"  ||
		 format == "RGBA" ||
		 format == "UYVY" ||
		 format == "YUV"
		 )
	      {
		needSize = true;
	      }

	    cout << " write ..." << endl;
	    Image original = master;

	    // Write and read image initially to create repeatable image
	    original.quantizeDither( false );
	    original.magick( formats[ i ] );
	    original.animationDelay( 10 );
	    original.write( &blob );

	    cout << " read ..." << endl;
	    original.quantizeDither( false );
	    original.magick( formats[ i ] );
	    if ( needSize )
	      original.size( size );
	    original.read( blob );

	    // Now test writing and reading image
	    cout << " write ..." << endl;
	    original.quantizeDither( false );
	    original.magick( formats[ i ] );
	    original.animationDelay( 10 );
	    original.write( &blob );
	  
	    Image image;
	  
	    // Read image from BLOB
	    cout << " read ..." << endl;
	    image.quantizeDither( false );
	    image.magick( formats[ i ] );
	    if ( needSize )
	      image.size( size );
	    image.read( blob );


	    if ( 
 		 format != "JPEG" &&
 		 format != "JPG" &&
 		 format != "JPEG24" &&
		 format != "P7" &&
		 format != "PCD" &&
		 format != "PCDS" &&
		 format != "PIC" &&
		 format != "PICT" &&
		 format != "UYVY" &&
		 format != "VICAR" &&
		 format != "YUV"
		 )
	      {
		if ( image.signature(true) != original.signature() )
		  {
		    cout << "Line: " << __LINE__
			 << " Format \"" << image.magick()
			 << "\" BLOB signature " << image.signature() << " "
			 << "does not match original "
			 << original.signature()
			 << endl;
		    //Image errimage = image;
		    image.display();
		    image.write( string( "output." ) + format );
		    ++failures;
		  }
	      }
	    else
	      {
		// PhotoCD changes size to 768x512 by default
		if ( format == "PCD" ||
		     format == "PCDS" )
		  {
		    if ( image.columns() != 768 || image.rows() != 512 )
		      {
			cout << "Line: " << __LINE__
			     << " Format \"" << formats[ i ]
			     << "\" Image size " << image.columns() << "x" << image.rows() << " "
			     << "does not match expected 768x512"
			     << endl;
		      }
		  }
		else
		  {
		    // Lossy formats
		    if ( image.columns()*image.rows() != master.columns()*master.rows() )
		      {
			cout << "Line: " << __LINE__
			     << " Format \"" << formats[ i ]
			     << "\" Image size " << image.columns() << "x" << image.rows() << " "
			     << "does not match original "
			     << master.columns() << "x" << master.rows()
			     << endl;
			++failures;
		      }
		  }
	      }
	    image.write( string( "output." ) + format );
	  }
	  catch( Exception error_ )
	    {
	      cout << "Line: " << __LINE__
		   << " Format \"" << formats[ i ] << "\" "
		   << "Caught exception: " << error_.what() << endl;
	      ++failures;
	      continue;
	    }
	  catch( exception error_ )
	    {
	      cout << "Line: " << __LINE__
		   << " Format \"" << formats[ i ] << "\" "
		   << "Caught exception: " << error_.what() << endl;
	      ++failures;
	      continue;
	    }
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

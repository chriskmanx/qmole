// This may look like C code, but it is really -*- C++ -*-
//
// Copyright Bob Friesenhahn, 1999
//
// Simple C++ function wrappers for often used or otherwise
// inconvenient ImageMagick equivalents
//

#if !defined(MagickFunctions_header)
#define MagickFunctions_header

#include <string>

#include <Magick++/Include.h>

namespace Magick
{
  void CloneString( char **destination_, const std::string &source_ );
  void ColorizeImage( MagickLib::Image &image_, const std::string &opacity_,
		      const std::string &penColor_);
  void CommentImage( MagickLib::Image &image_, const std::string &comments_ );

  int GetGeometry( const std::string &geometry_, int &x, int &y,
		   unsigned int &width, unsigned int &height );
  bool IsGeometry( const std::string &geometry_ );
  void LabelImage( MagickLib::Image &image_, const std::string &label_);
  
  void OpaqueImage( MagickLib::Image &image_, const std::string &opaqueColor_,
		    const std::string &penColor_);
  int ParseImageGeometry( const std::string &geometry_, int &x, int &y,
			  unsigned int &width, unsigned int &height );
  std::string PostscriptGeometry( const std::string &page_ );
}
#endif // MagickFunctions_header

// This may look like C code, but it is really -*- C++ -*-
//
// Copyright Bob Friesenhahn, 1999
//
// Simple C++ function wrappers for ImageMagick equivalents
//

#define MAGICK_IMPLEMENTATION

#include <string>
#include <iostream>

using namespace std;

#include <Magick++/Functions.h>
#include <Magick++/Include.h>

// Clone C++ string as allocated C string, de-allocating any existing string
void Magick::CloneString( char **destination_, const std::string &source_ )
{
  MagickLib::CloneString( destination_, source_.c_str() );
}

void Magick::ColorizeImage( MagickLib::Image &image_, const std::string &opacity_,
			    const std::string &penColor_)
{
  MagickLib::ColorizeImage( &image_, opacity_.c_str(), penColor_.c_str() );
}

void Magick::CommentImage( MagickLib::Image &image_, const std::string &comments_ )
{
  MagickLib::CommentImage( &image_, comments_.c_str() );
}

// Get current image geometry
int Magick::GetGeometry( const std::string &geometry_, int &x_, int &y_,
			 unsigned int &width_, unsigned int &height_ )
{
  return MagickLib::GetGeometry( geometry_.c_str(), &x_, &y_,
				   &width_, &height_ );
}

// Test C++ string to see if it is a valid geometry specification
bool Magick::IsGeometry( const std::string &geometry_)
{
  if ( MagickLib::IsGeometry( geometry_.c_str() ) )
    return true;
  else
    return false;
}

void Magick::LabelImage( MagickLib::Image &image_, const std::string &label_)
{
  MagickLib::LabelImage( &image_, label_.c_str() );
}

void Magick::OpaqueImage( MagickLib::Image &image_, const std::string &opaqueColor_,
		  const std::string &penColor_)
{
  MagickLib::OpaqueImage( &image_, opaqueColor_.c_str(), penColor_.c_str() );
}

// Get scaled image geometry
int Magick::ParseImageGeometry( const std::string &geometry_, int &x_, int &y_,
				unsigned int &width_, unsigned int &height_ )
{
  return MagickLib::ParseImageGeometry( geometry_.c_str(), &x_, &y_,
					&width_, &height_ );
}

std::string Magick::PostscriptGeometry( const std::string &page_ )
{
  char *pageptr = MagickLib::PostscriptGeometry( page_.c_str() );

  if ( !pageptr )
    return std::string();

  std::string pagesize(pageptr);

  MagickLib::DestroyPostscriptGeometry( pageptr );

  return pagesize;
}


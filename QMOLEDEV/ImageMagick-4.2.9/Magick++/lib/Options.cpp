// This may look like C code, but it is really -*- C++ -*-
//
// Copyright Bob Friesenhahn, 1999
//
// Implementation of Options
//
// A wrapper around ImageInfo and QuantizeInfo
//

#define MAGICK_IMPLEMENTATION

#include <string>
#include <cstdlib>
#include <iostream>
#include <Magick++/Options.h>
#include <Magick++/Functions.h>
#include <Magick++/Include.h>

// Constructor
Magick::Options::Options( void )
  : _imageInfo((MagickLib::ImageInfo*)MagickLib::AllocateMemory( sizeof(MagickLib::ImageInfo) ) ),
    _quantizeInfo((MagickLib::QuantizeInfo*)MagickLib::AllocateMemory( sizeof(MagickLib::QuantizeInfo) )),
    _penTexture(0)
{
  // Initialize image info with defaults
  MagickLib::GetImageInfo( _imageInfo );
  
  // Initialize quantization info
  MagickLib::GetQuantizeInfo( _quantizeInfo );
}

// Copy constructor
Magick::Options::Options( const Magick::Options& options_ )
  : _imageInfo(MagickLib::CloneImageInfo( options_._imageInfo )),
    _quantizeInfo(MagickLib::CloneQuantizeInfo(options_._quantizeInfo)),
    _penTexture(0)
{
  // Copy pen texture
  if ( options_._penTexture )
    {
      _penTexture = MagickLib::CloneImage( options_._penTexture,
					   options_._penTexture->columns,
					   options_._penTexture->rows,
					   True );
    }
}

// Destructor
Magick::Options::~Options()
{
  // Destroy image info
  MagickLib::DestroyImageInfo( _imageInfo );
  _imageInfo = (MagickLib::ImageInfo*)NULL;
  
  // Destroy quantization info
  MagickLib::DestroyQuantizeInfo( _quantizeInfo );
  _quantizeInfo = (MagickLib::QuantizeInfo*)NULL;

  // Destroy pen texture
  if ( _penTexture )
    {
      MagickLib::DestroyImage( _penTexture );
      _penTexture = (MagickLib::Image *)NULL;
    }
}

/*
 * Methods for setting image attributes
 *
 */

void Magick::Options::animationDelay ( unsigned int delay_ )
{
  char tmpbuff[MaxTextExtent + 1];
  MagickLib::FormatString( tmpbuff, "%u", delay_ );
  Magick::CloneString( &_imageInfo->delay, tmpbuff );
}
unsigned int Magick::Options::animationDelay ( void ) const
{
  if ( _imageInfo->delay )
    return atoi( _imageInfo->delay );
  else
    return 0;
}

void Magick::Options::animationIterations ( unsigned int iterations_ )
{
  char tmpbuff[MaxTextExtent + 1];
  MagickLib::FormatString( tmpbuff, "%u", iterations_ );
  Magick::CloneString( &_imageInfo->iterations, tmpbuff );
}
unsigned int Magick::Options::animationIterations ( void ) const
{
  if ( _imageInfo->iterations )
    return atoi( _imageInfo->iterations );
  else
    return 0;
}

void Magick::Options::backgroundColor ( const Magick::Color &color_ )
{
  if ( !color_.isValid() )
    {
      if ( _imageInfo->background_color )
	{
	  MagickLib::FreeMemory( _imageInfo->background_color );
	  _imageInfo->background_color = (char *)NULL;
	}
      return;
    }
  Magick::CloneString( &_imageInfo->background_color, color_ );
}
Magick::Color Magick::Options::backgroundColor ( void ) const
{
  if ( _imageInfo->background_color )
      return Magick::Color( _imageInfo->background_color );

  return Magick::Color();
}

void Magick::Options::backgroundTexture ( const std::string &backgroundTexture_ )
{
  if ( backgroundTexture_.length() == 0 )
    {
      if ( _imageInfo->texture )
	{
	  MagickLib::FreeMemory( _imageInfo->texture );
	  _imageInfo->texture = (char *)NULL;
	}
      return;
    }

  Magick::CloneString( &_imageInfo->texture, backgroundTexture_ );
}
std::string Magick::Options::backgroundTexture ( void ) const
{
  if ( _imageInfo->texture )
    return std::string( _imageInfo->texture );
  else
    return std::string();
}

void Magick::Options::borderColor ( const Color &color_ )
{
  if ( !color_.isValid() )
    {
      if ( _imageInfo->border_color )
	{
	  MagickLib::FreeMemory( _imageInfo->border_color );
	  _imageInfo->border_color = (char *)NULL;
	}
      return;
    }

  Magick::CloneString( &_imageInfo->border_color, color_ );
}
Magick::Color Magick::Options::borderColor ( void ) const
{
    if ( _imageInfo->border_color )
      return Magick::Color( _imageInfo->border_color );

    return Magick::Color();
}

// Text bounding-box base color
void Magick::Options::boxColor ( const Magick::Color &boxColor_ )
{
  if ( !boxColor_.isValid() )
    {
      if ( _imageInfo->box )
	{
	  MagickLib::FreeMemory( _imageInfo->box );
	  _imageInfo->box = (char *)NULL;
	}
      return;
    }

  Magick::CloneString( &_imageInfo->box, boxColor_ );
}
Magick::Color Magick::Options::boxColor ( void ) const
{
  if ( _imageInfo->box )
    return Magick::Color( _imageInfo->box );

  return Magick::Color();
}

void Magick::Options::density ( const Magick::Geometry &density_ )
{
  if ( !density_.isValid() )
    {
      if ( _imageInfo->density )
	{
	  MagickLib::FreeMemory( _imageInfo->density );
	  _imageInfo->density = (char *)NULL;
	}
      return;
    }

  Magick::CloneString( &_imageInfo->density, density_ );
}
Magick::Geometry Magick::Options::density ( void ) const
{
  if ( _imageInfo->density )
    return Geometry( _imageInfo->density );

  return Geometry();
}


void Magick::Options::fileName ( const std::string &fileName_ )
{
  fileName_.copy( _imageInfo->filename, MaxTextExtent-1 );
  _imageInfo->filename[ fileName_.length() ] = 0;
}
std::string Magick::Options::fileName ( void ) const
{
  return std::string( _imageInfo->filename );
}

void Magick::Options::font ( const std::string &font_ )
{
  if ( font_.length() == 0 )
    {
      if ( _imageInfo->font )
	{
	  MagickLib::FreeMemory( _imageInfo->font );
	  _imageInfo->font = (char *)NULL;
	}
      return;
    }
      
  Magick::CloneString( &_imageInfo->font, font_ );
}
std::string Magick::Options::font ( void ) const
{
  if ( _imageInfo->font )
    return std::string( _imageInfo->font );
  
  return std::string();
}

std::string Magick::Options::format ( void ) const
{
  const MagickLib::MagickInfo * magick_info = 0;
  if ( _imageInfo->magick && ( *_imageInfo->magick != '\0' ))
    magick_info = MagickLib::GetMagickInfo( _imageInfo->magick );
  
  if (( magick_info != (MagickLib::MagickInfo *)0 ) && 
      ( *magick_info->description != '\0' ))
    return std::string( magick_info->description );
  
  return std::string();
}

void Magick::Options::gifDisposeMethod ( unsigned int disposeMethod_ )
{
  char tmpbuff[MaxTextExtent + 1];
  MagickLib::FormatString( tmpbuff, "%u", disposeMethod_ );
  Magick::CloneString( &_imageInfo->dispose, tmpbuff );
}
unsigned int Magick::Options::gifDisposeMethod ( void ) const
{
  // It would be better to return an enumeration
  if ( _imageInfo->dispose )
    return ( atoi( _imageInfo->dispose ) );
  
  return 0;
}

void Magick::Options::magick ( const std::string &magick_ )
{
  MagickLib::FormatString( _imageInfo->filename, "%.1024s:", magick_.c_str() );
  MagickLib::SetImageInfo( _imageInfo, 1 );
  if ( _imageInfo->magick == '\0' )
    MagickLib::MagickWarning( MagickLib::OptionWarning, "Unrecognized image format",
			      magick_.c_str() );
}
std::string Magick::Options::magick ( void ) const
{
  if ( _imageInfo->magick && *_imageInfo->magick )
    return std::string( _imageInfo->magick );
  
  return std::string();
}

void Magick::Options::matteColor ( const Magick::Color &matteColor_ )
{
  if ( !matteColor_.isValid() )
    {
      if ( _imageInfo->matte_color )
	{
	  MagickLib::FreeMemory( _imageInfo->matte_color );
	  _imageInfo->matte_color = (char *)NULL;
	}
      return;
    }

  Magick::CloneString( &_imageInfo->matte_color, matteColor_ );
}
Magick::Color Magick::Options::matteColor ( void ) const
{
  if ( _imageInfo->matte_color )
    return Magick::Color( _imageInfo->matte_color );

  return Magick::Color();
}

void Magick::Options::penColor ( const Color &penColor_ )
{
  if ( !penColor_.isValid() )
    {
      if ( _imageInfo->pen )
	{
	  MagickLib::FreeMemory( _imageInfo->pen );
	  _imageInfo->pen = (char *)NULL;
	}
      return;
    }

  Magick::CloneString( &_imageInfo->pen, penColor_ );
}
Magick::Color Magick::Options::penColor ( void  ) const
{
  if ( _imageInfo->pen )
    return Color( _imageInfo->pen );

  return Color( );
}

void Magick::Options::penTexture ( const MagickLib::Image *penTexture_ )
{
  if ( _penTexture )
    MagickLib::DestroyImage( _penTexture );

  _penTexture = (MagickLib::Image *)NULL;

  if ( penTexture_ && penTexture_->packets )
    _penTexture = MagickLib::CloneImage( penTexture_,
					 penTexture_->columns,
					 penTexture_->rows,
					 True );
}
const MagickLib::Image* Magick::Options::penTexture ( void  ) const
{
  return _penTexture;
}

void Magick::Options::psPageSize ( const Magick::Geometry &pageSize_ )
{
  if ( !pageSize_.isValid() )
    {
      if ( _imageInfo->page )
	{
	  MagickLib::FreeMemory( _imageInfo->page );
	  _imageInfo->page = (char *)NULL;
	}
      return;
    }

  Magick::CloneString( &_imageInfo->page, pageSize_ );
}
Magick::Geometry Magick::Options::psPageSize ( void ) const
{
  if ( _imageInfo->page )
    return std::string( _imageInfo->page );

    return std::string();
}

void Magick::Options::resolutionUnits ( Magick::ResolutionType resolutionUnits_ )
{
  _imageInfo->units = resolutionUnits_;
}
Magick::ResolutionType Magick::Options::resolutionUnits ( void ) const
{
  return (Magick::ResolutionType)_imageInfo->units;
}

void Magick::Options::size ( const Geometry &geometry_ )
{
  if ( _imageInfo->size )
    {
      MagickLib::FreeMemory( _imageInfo->size );
      _imageInfo->size = (char *)NULL;
    }

  if ( geometry_.isValid() )
    Magick::CloneString( &_imageInfo->size, geometry_ );
}
Magick::Geometry Magick::Options::size ( void ) const
{
  if ( _imageInfo->size )
    return Geometry( _imageInfo->size );

  return Geometry();
}

void Magick::Options::tileName ( const std::string &tileName_ )
{
  if ( tileName_.length() == 0 )
    {
      if ( _imageInfo->tile )
	{
	  MagickLib::FreeMemory( _imageInfo->tile );
	  _imageInfo->tile = (char *)NULL;
	}
      return;
    }

  Magick::CloneString( &_imageInfo->tile, tileName_ );
}
std::string Magick::Options::tileName ( void ) const
{
  if ( _imageInfo->tile )
    return std::string( _imageInfo->tile );
  
  return std::string();
}

void Magick::Options::view ( const std::string &view_ )
{
  if ( view_.length() == 0 )
    {
      if ( _imageInfo->view )
	{
	  MagickLib::FreeMemory( _imageInfo->view );
	  _imageInfo->view = (char *)NULL;
	}
      return;
    }

  Magick::CloneString( &_imageInfo->view, view_ );
}
std::string Magick::Options::view ( void ) const
{
  if ( _imageInfo->view )
    return std::string( _imageInfo->view );

  return std::string();
}

void Magick::Options::x11Display ( const std::string &display_ )
{
  if ( display_.length() == 0 )
    {
      if ( _imageInfo->server_name )
	{
	  MagickLib::FreeMemory( _imageInfo->server_name );
	  _imageInfo->server_name = (char *)NULL;
	}
      return;
    }

  Magick::CloneString( &_imageInfo->server_name, display_ );
}
std::string Magick::Options::x11Display ( void ) const
{
  if ( _imageInfo->server_name )
    return std::string( _imageInfo->server_name );

    return std::string();
}


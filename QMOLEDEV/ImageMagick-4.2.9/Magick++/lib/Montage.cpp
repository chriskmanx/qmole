// This may look like C code, but it is really -*- C++ -*-
//
// Copyright Bob Friesenhahn, 1999
//
// Implementation of Montage
//

#define MAGICK_IMPLEMENTATION

#include <string>

#include <Magick++/Montage.h>
#include <Magick++/Functions.h>


void Magick::Montage::updateMontageInfo ( MagickLib::MontageInfo &montageInfo_ ) const
{
  if ( _backgroundColor.isValid() )
    {
      Magick::CloneString( &montageInfo_.background_color, _backgroundColor );
    }

  if ( _compose != Magick::UndefinedCompositeOp )
    montageInfo_.compose = _compose;

  if ( _fileName.length() != 0 )
    {
      _fileName.copy( montageInfo_.filename, MaxTextExtent - 1 );
      montageInfo_.filename[ _fileName.length() ] = 0; // null terminate
    }

  if ( _font.length() != 0 )
    Magick::CloneString( &montageInfo_.font, _font );

  if ( _geometry.isValid() )
    Magick::CloneString( &montageInfo_.geometry, _geometry );

  if ( _gravity != 0 )
    montageInfo_.gravity = _gravity;

  if ( _pen.isValid() )
    Magick::CloneString( &montageInfo_.pen, _pen );

  if ( _pointSize != 0 )
    montageInfo_.pointsize = _pointSize;

  if ( _shadow == true )
    montageInfo_.shadow = True;

  if ( _texture.length() != 0 )
    Magick::CloneString( &montageInfo_.texture, _texture );

  if ( _tile.isValid() )
    Magick::CloneString( &montageInfo_.tile, _tile );

  if ( _title.length() != 0 )
    Magick::CloneString( &montageInfo_.title, _title );
}


//
// Implementation of MontageFramed
//


/* virtual */ void  Magick::MontageFramed::updateMontageInfo ( MagickLib::MontageInfo &montageInfo_ ) const
{
  // Do base updates
  Montage::updateMontageInfo ( montageInfo_ );

  if ( _borderColor.isValid() )
    Magick::CloneString( &montageInfo_.border_color, _borderColor );

  if ( _borderWidth != 0 )
    montageInfo_.border_width = _borderWidth;

  if ( _frame.isValid() )
    Magick::CloneString( &montageInfo_.frame, _frame );

  if ( _matteColor.isValid() )
    Magick::CloneString( &montageInfo_.matte_color, _matteColor );
}

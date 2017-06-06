// This may look like C code, but it is really -*- C++ -*-
//
// Copyright Bob Friesenhahn, 1999
//
// Geometry implementation
//

#define MAGICK_IMPLEMENTATION

#include <string>
#include <iostream>
#include <iomanip>
#include <strstream>

using namespace std;

#include <Magick++/Geometry.h>
#include <Magick++/Functions.h>

int Magick::operator == ( const Magick::Geometry& left_,
			  const Magick::Geometry& right_ )
{
  return (
	  ( left_.isValid()   == right_.isValid() ) &&
	  ( left_.width()     == right_.width() ) &&
	  ( left_.height()    == right_.height() ) &&
	  ( left_.xOff()      == right_.xOff() ) &&
	  ( left_.yOff()      == right_.yOff() ) &&
	  ( left_.xNegative() == right_.xNegative() ) &&
	  ( left_.yNegative() == right_.yNegative() ) &&
	  ( left_.percent()   == right_.percent() ) &&
	  ( left_.aspect()    == right_.aspect() ) &&
	  ( left_.greater()   == right_.greater() ) &&
	  ( left_.less()      == right_.less() )
	  );
}
int Magick::operator != ( const Magick::Geometry& left_,
			  const Magick::Geometry& right_ )
{
  return ( ! (left_ == right_) );
}
int Magick::operator >  ( const Magick::Geometry& left_,
			  const Magick::Geometry& right_ )
{
  return ( !( left_ < right_ ) && ( left_ != right_ ) );
}
int Magick::operator <  ( const Magick::Geometry& left_,
			  const Magick::Geometry& right_ )
{
  return (
	  ( left_.width() * left_.height() )
	  <
	  ( right_.width() * right_.height() )
	  );
}
int Magick::operator >= ( const Magick::Geometry& left_,
			  const Magick::Geometry& right_ )
{
  return ( ( left_ > right_ ) || ( left_ == right_ ) );
}
int Magick::operator <= ( const Magick::Geometry& left_,
			  const Magick::Geometry& right_ )
{
  return ( ( left_ < right_ ) || ( left_ == right_ ) );
}

Magick::Geometry::Geometry ( unsigned int width_,
			     unsigned int height_,
			     unsigned int xOff_,
			     unsigned int yOff_,
			     bool xNegative_,
			     bool yNegative_ )
  : _width( width_ ),
    _height( height_ ),
    _xOff( xOff_ ),
    _yOff( yOff_ ),
    _xNegative( xNegative_ ),
    _yNegative( yNegative_ ),
    _isValid( true ),
    _percent( false ),
    _aspect( false ),
    _greater( false ),
    _less( false )
{
}

Magick::Geometry::Geometry ( const std::string geometry_ )
  : _width( 0 ),
    _height( 0 ),
    _xOff( 0 ),
    _yOff( 0 ),
    _xNegative( false ),
    _yNegative( false ),
    _isValid( false ),
    _percent( false ),
    _aspect( false ),
    _greater( false ),
    _less( false )
{
  *this = geometry_; // Use assignment operator

}

Magick::Geometry::Geometry ( const char * geometry_ )
  : _width( 0 ),
    _height( 0 ),
    _xOff( 0 ),
    _yOff( 0 ),
    _xNegative( false ),
    _yNegative( false ),
    _isValid( false ),
    _percent( false ),
    _aspect( false ),
    _greater( false ),
    _less( false )
{
  *this = geometry_; // Use assignment operator

}

Magick::Geometry::Geometry ( void )
  : _width( 0 ),
    _height( 0 ),
    _xOff( 0 ),
    _yOff( 0 ),
    _xNegative( false ),
    _yNegative( false ),
    _isValid ( false ),
    _percent( false ),
    _aspect( false ),
    _greater( false ),
    _less( false )
{
}

/* virtual */ Magick::Geometry::~Geometry ( void )
{
  // Nothing to do
}

// Set value via geometry string
/* virtual */ const Magick::Geometry&
Magick::Geometry::operator = ( const std::string &geometry_ )
{
  std::string geometry(geometry_);

  // If argument does not start with digit, presume that it is a
  // page-size specification that needs to be converted to an
  // equivalent geometry specification using PostscriptGeometry()
  if ( geometry[0] != '-' &&
       geometry[0] != '+' &&
       !isdigit((int)geometry[0]) )
    {
      geometry =  Magick::PostscriptGeometry( geometry );
    }

  int x = 0;
  int y = 0;
  unsigned int width_val = 0;
  unsigned int height_val = 0;
  int flags = Magick::GetGeometry ( geometry, x, y, width_val, height_val );

  if ( ( flags & MagickLib::WidthValue ) != 0 )
    {
      _width = width_val;
      isValid( true );
    }

  if ( ( flags & MagickLib::HeightValue ) != 0 )
    _height = height_val;

  if ( ( flags & MagickLib::XValue ) != 0 )
    {
      _xOff = ::abs(x);
      isValid( true );
    }

  if ( ( flags & MagickLib::YValue ) != 0 )
    {
      _yOff = ::abs(y);
      isValid( true );
    }

  if ( ( flags & MagickLib::XNegative ) != 0 )	
    _xNegative = true;

  if ( ( flags & MagickLib::YNegative ) != 0 )
    _yNegative = true;

  if ( ( flags & MagickLib::PercentValue ) != 0 )
    _percent = true;

  if ( ( flags & MagickLib::AspectValue ) != 0 )
    _aspect = true;

  if ( ( flags & MagickLib::LessValue ) != 0 )
    _less = true;

  if ( ( flags & MagickLib::GreaterValue ) != 0 )
    _greater = true;

  return *this;
}


// Set value via geometry C string
/* virtual */ const Magick::Geometry& Magick::Geometry::operator = ( const char * geometry_ )
{
  *this = std::string(geometry_);
  return *this;
}

// Return geometry string
Magick::Geometry::operator std::string() const
{
  char buffer[32];
  ostrstream geomStr( buffer, sizeof( buffer ) - 1 );

  if ( _width )
    geomStr << _width;

  if ( _width && _height )
    geomStr << "x"
	    << _height;

  if ( _xOff || _yOff )
    {
      if ( _xNegative )
	geomStr << "-";
      else
	geomStr << "+";
      geomStr << _xOff;

      if ( _yNegative )
	geomStr << "-";
      else
	geomStr << "+";
      geomStr << _yOff;
    }

  if ( _percent )
    geomStr << "%";

  if ( _aspect )
    geomStr << "!";

  if ( _greater )
    geomStr << ">";

  if ( _less )
    geomStr << "<";

  geomStr << ends;

  std::string geometry = buffer;
  return geometry;
}

// print object to a stream
ostream& operator<<(ostream& stream_, const Magick::Geometry& geometry_)
{
  std::string geomStr( geometry_ );
  stream_ << geomStr;
  return stream_;
}

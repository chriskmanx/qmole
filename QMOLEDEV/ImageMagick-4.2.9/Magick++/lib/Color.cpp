// This may look like C code, but it is really -*- C++ -*-
//
// Copyright Bob Friesenhahn, 1999
//
// Color Implementation
//

#define MAGICK_IMPLEMENTATION

#include <string>
#include <iostream>
#include <iomanip>
#include <strstream>

using namespace std;

#include <Magick++/Color.h>
#include <Magick++/Functions.h>
#include <Magick++/LastError.h>

//
// Color operator fuctions
//
int Magick::operator == ( const Magick::Color& left_,
			  const Magick::Color& right_ )
{
  return ( ( left_.isValid()      == right_.isValid() ) && 
	   ( left_.redQuantum()   == right_.redQuantum() ) &&
	   ( left_.greenQuantum() == right_.greenQuantum() ) &&
	   ( left_.blueQuantum()  == right_.blueQuantum() )
	  );
}
int Magick::operator != ( const Magick::Color& left_,
			  const Magick::Color& right_ )
{
  return ( ! (left_ == right_) );
}
int Magick::operator >  ( const Magick::Color& left_,
			  const Magick::Color& right_ )
{
  return ( !( left_ < right_ ) && ( left_ != right_ ) );
}
// Compare color intensities (similar to ImageMagick Intensity macro)
int Magick::operator <  ( const Magick::Color& left_,
			  const Magick::Color& right_ )
{
  return (
	  (
	   (unsigned int)
	   ( left_.redQuantum()   * 77 +
	     left_.greenQuantum() * 150 +
	     left_.blueQuantum()  * 29
	     ) >> 8
	   )
	  <
	  (
	   (unsigned int)
	   ( right_.redQuantum()   * 77 +
	     right_.greenQuantum() * 150 +
	     right_.blueQuantum()  * 29
	     ) >> 8
	   )
	  );
}
int Magick::operator >= ( const Magick::Color& left_,
			  const Magick::Color& right_ )
{
  return ( ( left_ > right_ ) || ( left_ == right_ ) );
}
int Magick::operator <= ( const Magick::Color& left_,
			  const Magick::Color& right_ )
{
  return ( ( left_ < right_ ) || ( left_ == right_ ) );
}

//
// Color Implementation
//

// Copy constructor
Magick::Color::Color ( const Color & color_ )
  : _packet(0)
{
  if ( this != &color_ )
    {
      if ( color_._packet != 0 )
	{
	  _packet = new MagickLib::RunlengthPacket;
	  _packet->red    = color_._packet->red;
	  _packet->green  = color_._packet->green;
	  _packet->blue   = color_._packet->blue;
	  _packet->length = 1;
	  _packet->index  = color_._packet->index;
	}
    }
}

Magick::Color Magick::Color::operator = ( const Color& color_ )
{
  if ( this != &color_ )
    {
      if ( color_._packet != 0 )
	{
	  if ( ! _packet )
	    _packet = new MagickLib::RunlengthPacket;
	  _packet->red    = color_._packet->red;
	  _packet->green  = color_._packet->green;
	  _packet->blue   = color_._packet->blue;
	  _packet->length = 1;
	  _packet->index  = color_._packet->index;
	}
    }
  return *this;
}

// Set color via X11 color specification string
const Magick::Color& Magick::Color::operator = ( std::string x11color_ )
{
  if ( !_packet )
    initRep();

  MagickLib::ColorPacket target_color;
  if ( MagickLib::QueryColorDatabase( x11color_.c_str(), &target_color ) )
    {
      redQuantum( XDownScale(target_color.red) );
      greenQuantum( XDownScale(target_color.green) );
      blueQuantum( XDownScale(target_color.blue) );

      if ( target_color.flags & MagickLib::DoMatte )
	// MagickLib::DoMatte is set if alpha was set by XQueryColorDatabase()
	alphaQuantum(  XDownScale(target_color.index) );
      else
	// If alpha is not set, then set to MaxRGB (Opaque) since that
	// is the default when a color is specified.
	alphaQuantum( MaxRGB );
    }
  else
    {
      LastError err( MagickLib::OptionError, "Color not valid.", x11color_ );
      err.throwException();
    }

  return *this;
}

// Set color via X11 color specification C string
const Magick::Color& Magick::Color::operator = ( const char * x11color_ )
{
  *this = std::string(x11color_);
  return *this;
}

// Return X11 color specification string
Magick::Color::operator std::string() const
{

  if ( !isValid() )
    return std::string("");

  char colorbuf[17];
  ostrstream colorstr( colorbuf, sizeof(colorbuf ));
  colorstr.setf(ios::right | ios::uppercase );
  colorstr.fill('0');

#if defined(QuantumLeap)
  colorstr << "#"
	   << hex
	   << setw(4) << (unsigned int)redQuantum()
	   << setw(4) << (unsigned int)greenQuantum()
	   << setw(4) << (unsigned int)blueQuantum();
  if ( alphaQuantum() < MaxRGB )
    colorstr << hex << setw(4) << (unsigned int)alphaQuantum();
#else
  colorstr << "#"
	   << hex
	   << setw(2) << (unsigned int)redQuantum()
	   << setw(2) << (unsigned int)greenQuantum()
	   << setw(2) << (unsigned int)blueQuantum();
  if ( alphaQuantum() < MaxRGB )
    colorstr << hex << setw(2) << (unsigned int)alphaQuantum();
#endif

  colorstr << ends;

  return std::string(colorbuf);
}

//
// ColorHSL Implementation
//

Magick::ColorHSL::ColorHSL ( double hue_,
			     double saturation_,
			     double luminosity_ )
  : Color ()
{
  Quantum red, green, blue;

  MagickLib::HSLTransform ( hue_,
			    saturation_,
			    luminosity_,
			    &red,
			    &green,
			    &blue );

  redQuantum   ( red );
  greenQuantum ( green );
  blueQuantum  ( blue );
}

Magick::ColorHSL::ColorHSL ( )
  : Color ()
{
}

Magick::ColorHSL::~ColorHSL ( )
{
  // Nothing to do
}

void Magick::ColorHSL::hue ( double hue_ )
{
  double hue_val, saturation_val, luminosity_val;
  MagickLib::TransformHSL ( redQuantum(),
			    greenQuantum(),
			    blueQuantum(),
			    &hue_val,
			    &saturation_val,
			    &luminosity_val );

  hue_val = hue_;

  Quantum red, green, blue;
  MagickLib::HSLTransform ( hue_val,
			    saturation_val,
			    luminosity_val,
			    &red,
			    &green,
			    &blue
			    );

  redQuantum   ( red );
  greenQuantum ( green );
  blueQuantum  ( blue );
}

double Magick::ColorHSL::hue ( void ) const
{
  double hue_val, saturation_val, luminosity_val;
  MagickLib::TransformHSL ( redQuantum(),
			    greenQuantum(),
			    blueQuantum(),
			    &hue_val,
			    &saturation_val,
			    &luminosity_val );
  return hue_val;
}

void Magick::ColorHSL::saturation ( double saturation_ )
{
  double hue_val, saturation_val, luminosity_val;
  MagickLib::TransformHSL ( redQuantum(),
			    greenQuantum(),
			    blueQuantum(),
			    &hue_val,
			    &saturation_val,
			    &luminosity_val );
  
  saturation_val = saturation_;
  
  Quantum red, green, blue;
  MagickLib::HSLTransform ( hue_val,
			    saturation_val,
			    luminosity_val,
			    &red,
			    &green,
			    &blue
			    );

  redQuantum   ( red );
  greenQuantum ( green );
  blueQuantum  ( blue );
}

double Magick::ColorHSL::saturation ( void ) const
{
  double hue_val, saturation_val, luminosity_val;
  MagickLib::TransformHSL ( redQuantum(),
			    greenQuantum(),
			    blueQuantum(),
			    &hue_val,
			    &saturation_val,
			    &luminosity_val );
  return saturation_val;
}

void Magick::ColorHSL::luminosity ( double luminosity_ )
{
  double hue_val, saturation_val, luminosity_val;
  MagickLib::TransformHSL ( redQuantum(),
			    greenQuantum(),
			    blueQuantum(),
			    &hue_val,
			    &saturation_val,
			    &luminosity_val );
  
  luminosity_val = luminosity_;
  
  Quantum red, green, blue;
  MagickLib::HSLTransform ( hue_val,
			    saturation_val,
			    luminosity_val,
			    &red,
			    &green,
			    &blue
			    );
  
  redQuantum   ( red );
  greenQuantum ( green );
  blueQuantum  ( blue );
}

double Magick::ColorHSL::luminosity ( void ) const
{
  double hue_val, saturation_val, luminosity_val;
  MagickLib::TransformHSL ( redQuantum(),
			    greenQuantum(),
			    blueQuantum(),
			    &hue_val,
			    &saturation_val,
			    &luminosity_val );
  return luminosity_val;
}

//
// ColorGray Implementation
//
Magick::ColorGray::ColorGray ( double shade_ )
  : Color ( ScaleDoubleToQuantum( shade_ ),
	    ScaleDoubleToQuantum( shade_ ),
	    ScaleDoubleToQuantum( shade_ ) )
{
}

Magick::ColorGray::ColorGray ( void )
  : Color ()
{
}

Magick::ColorGray::~ColorGray ()
{
  // Nothing to do
}

void Magick::ColorGray::shade ( double shade_ )
{
  Quantum gray = ScaleDoubleToQuantum( shade_ );
  redQuantum   ( gray );
  greenQuantum ( gray );
  blueQuantum  ( gray );
}

double Magick::ColorGray::shade ( void ) const
{
  return ScaleQuantumToDouble ( greenQuantum() );
}

//
// ColorMono Implementation
//
Magick::ColorMono::ColorMono ( bool mono_  )
  : Color ( ( mono_ ? MaxRGB : 0 ),
	    ( mono_ ? MaxRGB : 0 ),
	    ( mono_ ? MaxRGB : 0 ) )
{
}

Magick::ColorMono::ColorMono ( void )
  : Color ()
{
}

Magick::ColorMono::~ColorMono ()
{
  // Nothing to do
}

void Magick::ColorMono::mono ( bool mono_ )
{
  redQuantum   ( mono_ ? MaxRGB : 0 );
  greenQuantum ( mono_ ? MaxRGB : 0 );
  blueQuantum  ( mono_ ? MaxRGB : 0 );
}

bool Magick::ColorMono::mono ( void ) const
{
  if ( greenQuantum() )
    return true;
  else
    return false;
}

//
// ColorRGB Implementation
//
Magick::ColorRGB::ColorRGB ( double red_,
			     double green_,
			     double blue_ )
  : Color ( ScaleDoubleToQuantum(red_),
	    ScaleDoubleToQuantum(green_),
	    ScaleDoubleToQuantum(blue_) )
{
}
Magick::ColorRGB::ColorRGB ( const std::string x11color_ )
  : Color( x11color_ )
{
}
Magick::ColorRGB::ColorRGB ( void )
  : Color ()
{
}
Magick::ColorRGB::~ColorRGB ( void )
{
  // Nothing to do
}


//
// ColorYUV Implementation
//

//           R = Y          +1.13980*V
//           G = Y-0.39380*U-0.58050*V
//           B = Y+2.02790*U
//
//         U and V, normally -0.5 through 0.5, must be normalized to the range 0
//         through MaxRGB.
//
//           Y =  0.29900*R+0.58700*G+0.11400*B
//           U = -0.14740*R-0.28950*G+0.43690*B
//           V =  0.61500*R-0.51500*G-0.10000*B
//
//         U and V, normally -0.5 through 0.5, are normalized to the range 0
//         through MaxRGB.  Note that U = 0.493*(B-Y), V = 0.877*(R-Y).
//
Magick::ColorYUV::ColorYUV ( double y_,
			     double u_,
			     double v_ )
  : Color ( ScaleDoubleToQuantum(y_ + 1.13980 * v_ ),
	    ScaleDoubleToQuantum(y_ - 0.39380 * u_ - 0.58050 * v_ ),
	    ScaleDoubleToQuantum(y_ + 2.02790 * u_ ) )
{
}

Magick::ColorYUV::ColorYUV ( void )
  : Color ()
{
}

Magick::ColorYUV::~ColorYUV ( void )
{
  // Nothing to do
}

void Magick::ColorYUV::u ( double u_ )
{
  double V = v();
  double Y = y();

  redQuantum   ( ScaleDoubleToQuantum( Y + 1.13980 * V ) );
  greenQuantum ( ScaleDoubleToQuantum( Y - 0.39380 * u_ - 0.58050 * V ) );
  blueQuantum  ( ScaleDoubleToQuantum( Y + 2.02790 * u_ ) );
}

double Magick::ColorYUV::u ( void ) const
{
  return ScaleQuantumToDouble( -0.14740 * redQuantum() - 0.28950 *
			       greenQuantum() + 0.43690 * blueQuantum() );
}

void Magick::ColorYUV::v ( double v_ )
{
  double U = u();
  double Y = y();

  redQuantum   ( ScaleDoubleToQuantum( Y + 1.13980 * v_ ) );
  greenQuantum ( ScaleDoubleToQuantum( Y - 0.39380 * U - 0.58050 * v_ ) );
  blueQuantum  ( ScaleDoubleToQuantum( Y + 2.02790 * U ) );
}

double Magick::ColorYUV::v ( void ) const
{
  return ScaleQuantumToDouble(  0.61500 * redQuantum() - 0.51500 *
				greenQuantum() - 0.10000 * blueQuantum() );
}

void Magick::ColorYUV::y ( double y_ )
{
  double U = u();
  double V = v();

  redQuantum   ( ScaleDoubleToQuantum( y_ + 1.13980 * V ) );
  greenQuantum ( ScaleDoubleToQuantum( y_ - 0.39380 * U - 0.58050 * V ) );
  blueQuantum  ( ScaleDoubleToQuantum( y_ + 2.02790 * U ) );
}

double Magick::ColorYUV::y ( void ) const
{
  return ScaleQuantumToDouble(  0.29900 * redQuantum() + 0.58700 *
				greenQuantum() + 0.11400 * blueQuantum() );
}

// This may look like C code, but it is really -*- C++ -*-
//
// Copyright Bob Friesenhahn, 1999
//
// Color Implementation
//
#if !defined (Color_header)
#define Color_header

#include <string>

#include <Magick++/Include.h>

#define ScaleDoubleToQuantum(quantum) ((unsigned int)(quantum * MaxRGB))
#define ScaleQuantumToDouble(quantum) (((double)quantum)/MaxRGB)

namespace Magick
{

  class Color;

  // Compare two Color objects regardless of LHS/RHS
  int operator == ( const Magick::Color& left_, const Magick::Color& right_ );
  int operator != ( const Magick::Color& left_, const Magick::Color& right_ );
  int operator >  ( const Magick::Color& left_, const Magick::Color& right_ );
  int operator <  ( const Magick::Color& left_, const Magick::Color& right_ );
  int operator >= ( const Magick::Color& left_, const Magick::Color& right_ );
  int operator <= ( const Magick::Color& left_, const Magick::Color& right_ );

  // Base color class stores RGB components scaled to fit Quantum
  //
  // Please note that this base class is subject to change so if you
  // want to be totally safe, don't rely on it.
  //
  class Color
  {
  public:
    Color ( Quantum red_,
	    Quantum green_,
	    Quantum blue_,
	    Quantum alpha_ = MaxRGB );
    Color ( const std::string x11color_ );
    Color ( const char * x11color_ );
    Color ( void );
    virtual        ~Color ( void );
    Color ( const Color & color_ );

    // Red color (0 to MaxRGB)
    void           redQuantum ( Quantum red_ );
    Quantum        redQuantum ( void ) const;

    // Green color (0 to MaxRGB)
    void           greenQuantum ( Quantum green_ );
    Quantum        greenQuantum ( void ) const;

    // Blue color (0 to MaxRGB)
    void           blueQuantum ( Quantum blue_ );
    Quantum        blueQuantum ( void ) const;

    // Alpha level (0 = transparent, 1 = opaque)
    void           alphaQuantum ( Quantum alpha_ );
    Quantum        alphaQuantum ( void ) const;

    // Scaled version of alpha for use in sub-classes
    void           alpha ( double alpha_ );
    double         alpha ( void ) const;
        
    // Does object contain valid color?
    void           isValid ( bool valid_ );
    bool           isValid ( void ) const;
    
    // Set color via X11 color specification string
    const Color& operator= ( std::string x11color_ );
    const Color& operator= ( const char * x11color_ );

    // Assignment operator
    Color operator= ( const Color& color_ );
    
    // Return X11 color specification string
    /* virtual */ operator std::string() const;

    //
    // Public methods beyond this point are for Magick++ use only.
    //

    // Increment operator (advance to next RunlengthPacket)
    Color& operator++ ();

    // Decrement operator (go to preceding RunlengthPacket)
    Color& operator-- ();

  protected:

  private:

    // PacketType specifies the interpretation of RunlengthPacket members
    // RGBPacket:
    //   Red      = red;
    //   Green    = green;
    //   Blue     = blue;
    // RGBAPacket:
    //   Red      = red;
    //   Green    = green;
    //   Blue     = blue;
    //   Alpha    = index;
    // CYMKPacket:
    //   Cyan     = red
    //   Yellow   = green
    //   Magenta  = blue
    //   Black(K) = index
    enum PacketType
    {
      RGBPacket,
      RGBAPacket,
      CYMKPacket
    };

    // Private constructor to construct with RunlengthPacket*
    // Used to point Color at a pixel.
    Color ( MagickLib::RunlengthPacket* rep_ );

    // Common initializer for RunlengthPacket representation
    void initRep();


    MagickLib::RunlengthPacket* _packet;
    bool                        _packetOwn;
    PacketType			_packetType;
  };

  //
  // HSL Colorspace colors
  //
  class ColorHSL : public Color
  {
  public:
    ColorHSL ( double hue_, double saturation_, double luminosity_ );
    ColorHSL ( );
    /* virtual */  ~ColorHSL ( );
    
    void           hue ( double hue_ );
    double         hue ( void ) const;
    
    void           saturation ( double saturation_ );
    double         saturation ( void ) const;
    
    void           luminosity ( double luminosity_ );
    double         luminosity ( void ) const;
  };
  
  //
  // Grayscale RGB color
  //
  // Grayscale is simply RGB with equal parts of red, green, and blue
  // All double arguments have a valid range of 0.0 - 1.0.
  class ColorGray : public Color
  {
  public:
    ColorGray ( double shade_ );
    ColorGray ( void );
    /* virtual */ ~ColorGray ();
    
    void           shade ( double shade_ );
    double         shade ( void ) const;
  };
  
  //
  // Monochrome color
  //
  // Color arguments are constrained to 'false' (black pixel) and 'true'
  // (white pixel)
  class ColorMono : public Color
  {
  public:
    ColorMono ( bool mono_ );
    ColorMono ( void );
    /* virtual */ ~ColorMono ();
    
    void           mono ( bool mono_ );
    bool           mono ( void ) const;
    
  };
  
  //
  // RGB color
  //
  // All color arguments have a valid range of 0.0 - 1.0.
  class ColorRGB : public Color
  {
  public:
    ColorRGB ( double red_, double green_, double blue_ );
    ColorRGB ( const std::string x11color_ );
    ColorRGB ( void );
    /* virtual */  ~ColorRGB ( void );
    
    void           red ( double red_ );
    double         red ( void ) const;
    
    void           green ( double green_ );
    double         green ( void ) const;
    
    void           blue ( double blue_ );
    double         blue ( void ) const;
  };
  
  //
  // YUV Colorspace color
  //
  // Argument ranges:
  //        Y:  0.0 through 1.0
  //        U: -0.5 through 0.5
  //        V: -0.5 through 0.5
  class ColorYUV : public Color
  {
  public:
    ColorYUV ( double y_, double u_, double v_ );
    ColorYUV ( void );
    /* virtual */ ~ColorYUV ( void );
    
    void           u ( double u_ );
    double         u ( void ) const;
    
    void           v ( double v_ );
    double         v ( void ) const;
    
    void           y ( double y_ );
    double         y ( void ) const;
  };
} // namespace Magick

//
// Inlines
//


//
// Color
//

inline Magick::Color::Color ( Quantum red_,
			      Quantum green_,
			      Quantum blue_,
			      Quantum alpha_ )
  : _packet(0),
    _packetOwn(true)
{

  initRep();

  redQuantum   ( red_   );
  greenQuantum ( green_ );
  blueQuantum  ( blue_  );
  alphaQuantum ( alpha_ );
}

inline Magick::Color::Color ( const std::string x11color_ )
  : _packet(0),
    _packetOwn(true)
{
  initRep();

  // Use operator = implementation
  *this = x11color_;
}

inline Magick::Color::Color ( const char * x11color_ )
  : _packet(0),
    _packetOwn(true)
{
    initRep();

  // Use operator = implementation
  *this = x11color_;
}

inline Magick::Color::Color ( void )
  : _packet(0),
    _packetOwn(true)
{
  // No more initialization
}

inline Magick::Color::~Color( void )
{
  if ( _packetOwn )
    delete _packet;
}

inline void Magick::Color::redQuantum ( Quantum red_ )
{
  if ( !_packet )
    initRep();
  
  _packet->red = (Quantum) (red_ > MaxRGB ? MaxRGB : red_);
}

inline Magick::Quantum Magick::Color::redQuantum ( void ) const
{
  return _packet->red;
}

inline void Magick::Color::greenQuantum ( Quantum green_ )
{
  if ( !_packet )
    initRep();

  _packet->green = (Quantum) (green_ > MaxRGB ? MaxRGB : green_);
}

inline Magick::Quantum  Magick::Color::greenQuantum ( void ) const
{
  return _packet->green;
}

inline void  Magick::Color::blueQuantum ( Quantum blue_ )
{
  if ( !_packet )
    initRep();

  _packet->blue = (Quantum) (blue_ > MaxRGB ? MaxRGB : blue_);
}

inline Magick::Quantum Magick::Color::blueQuantum ( void ) const
{
  return _packet->blue;
}

inline void  Magick::Color::alphaQuantum ( Quantum alpha_ )
{
  if ( !_packet )
    initRep();

  _packet->index = (Quantum) (alpha_ > MaxRGB ? MaxRGB : alpha_);
}

inline Magick::Quantum Magick::Color::alphaQuantum ( void ) const
{
  return _packet->index;
}

// Scaled version of alpha for use in sub-classes
inline void  Magick::Color::alpha ( double alpha_ )
{
  alphaQuantum( ScaleDoubleToQuantum(alpha_) );
}
inline double Magick::Color::alpha ( void ) const
{
  return ScaleQuantumToDouble( alphaQuantum() );
}

// Does object contain valid color?
inline void Magick::Color::isValid ( bool valid_ )
{
  if ( ! valid_ )
    {
      delete _packet;
      _packet = 0;
    }
}
inline bool Magick::Color::isValid ( void ) const
{
  return _packet != 0;
}

// Private constructor to construct with RunlengthPacket*
// Used to point Color at a pixel.
inline Magick::Color::Color ( MagickLib::RunlengthPacket* rep_ )
  : _packet(rep_),
    _packetOwn(true)
{
}

// Common initializer for RunlengthPacket representation
inline void Magick::Color::initRep()
{
  _packet = new MagickLib::RunlengthPacket;
  _packet->red    = 0;
  _packet->green  = 0;
  _packet->blue   = 0;
  _packet->length = 1;
  _packet->index  = MaxRGB;
}

// Increment operator
inline Magick::Color& Magick::Color::operator++ ()
{
  ++_packet;
  return *this;
}

// Decrement operator
inline Magick::Color& Magick::Color::operator-- ()
{
  --_packet;
  return *this;
}


//
// ColorRGB
//
inline void Magick::ColorRGB::red ( double red_ )
{
  redQuantum( ScaleDoubleToQuantum(red_) );
}

inline double Magick::ColorRGB::red ( void ) const
{
  return ScaleQuantumToDouble( redQuantum() );
}

inline void Magick::ColorRGB::green ( double green_ )
{
  greenQuantum( ScaleDoubleToQuantum(green_) );
}

inline double Magick::ColorRGB::green ( void ) const
{
  return ScaleQuantumToDouble( greenQuantum() );
}

inline void Magick::ColorRGB::blue ( double blue_ )
{
  blueQuantum( ScaleDoubleToQuantum(blue_) );
}

inline double Magick::ColorRGB::blue ( void ) const
{
  return ScaleQuantumToDouble( blueQuantum() );
}

#endif // Color_header

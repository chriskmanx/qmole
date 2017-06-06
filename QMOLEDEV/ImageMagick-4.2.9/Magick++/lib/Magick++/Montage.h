// This may look like C code, but it is really -*- C++ -*-
//
// Definition of Montage class used to specify montage options.
//

#if !defined(Montage_header)
#define Montage_header

#include <string>
#include <Magick++/Color.h>
#include <Magick++/Geometry.h>
#include <Magick++/Include.h>

//
// Basic (Un-framed) Montage
//
namespace Magick
{
  class Montage
  {
  public:
    Montage( void );
    virtual ~Montage( void );
    
    void              backgroundColor ( const Color &backgroundColor_ );
    Color             backgroundColor ( void ) const;
    
    void              compose ( CompositeOperator compose_ );
    CompositeOperator compose ( void ) const;
    
    void              fileName( std::string fileName_ );
    std::string       fileName( void ) const;
    
    void              font ( std::string font_ );
    std::string       font ( void ) const;
    
    void              geometry ( const Geometry &geometry_ );
    Geometry          geometry ( void ) const;
    
    void              gravity ( unsigned int gravity_ );
    unsigned int      gravity ( void ) const;
    
    // Apply as attribute to all images before montage
    void              label( std::string label_ );
    std::string       label( void ) const;
    
    void              penColor ( const Color &pen_ );
    Color             penColor ( void ) const;
    
    void              pointSize ( unsigned int pointSize_ );
    unsigned int      pointSize ( void ) const;
    
    void              shadow ( bool shadow_ );
    bool              shadow ( void ) const;
    
    void              texture ( std::string texture_ );
    std::string       texture ( void ) const;
    
    void              tile ( const Geometry &tile_ );
    Geometry          tile ( void ) const;
    
    void              title ( std::string title_ );
    std::string       title ( void ) const;
    
    // Apply to montage with TransparentImage()
    void              transparentColor ( const Color &transparentColor_ );
    Color             transparentColor ( void ) const;

    //
    // Implementation methods/members
    //

    // Update elements in existing MontageInfo structure
    virtual void      updateMontageInfo ( MagickLib::MontageInfo &montageInfo_ ) const;
        
  protected:

  private:
    Color             _backgroundColor;   // Color that thumbnails are composed on
    CompositeOperator _compose;           // Composition algorithm to use (e.g. ReplaceCompositeOp)
    std::string       _fileName;          // Filename to save montages to
    std::string       _font;              // Label font
    Geometry          _geometry;          // Thumbnail width & height plus border width & height
    unsigned int      _gravity;           // Thumbnail position (e.g. SouthWestGravity)
    std::string       _label;             // Thumbnail label (applied to image prior to montage)
    Color             _pen;               // Color for text annotations
    unsigned int      _pointSize;         // Font point size
    bool              _shadow;            // Enable drop-shadows on thumbnails
    std::string       _texture;           // Background texture image
    Geometry          _tile;              // Thumbnail rows and colmns
    std::string       _title;             // Montage title
    Color             _transparentColor;  // Transparent color
  };

  //
  // Montage With Frames (Extends Basic Montage)
  //
  class MontageFramed : public Montage
  {
  public:
    MontageFramed ( void );
    /* virtual */ ~MontageFramed ( void );
    
    void           borderColor ( const Color &borderColor_ );
    Color          borderColor ( void ) const;
    
    void           borderWidth ( unsigned int borderWidth_ );
    unsigned int   borderWidth ( void ) const;
    
    void           frameGeometry ( const Geometry &frame_ );
    Geometry       frameGeometry ( void ) const;
    
    void           matteColor ( const Color &matteColor_ );
    Color          matteColor ( void ) const;

    //
    // Implementation methods/members
    //

    // Update elements in existing MontageInfo structure
    /* virtual */ void updateMontageInfo ( MagickLib::MontageInfo &montageInfo_ ) const;
    
  protected:

  private:
    
    Color          _borderColor;	// Frame border color
    unsigned int   _borderWidth;	// Pixels between thumbnail and surrounding frame
    Geometry       _frame;		// Frame geometry (width & height frame thickness)
    Color          _matteColor;		// Frame foreground color
  };
} // namespace Magick

//
// Inlines
//

//
// Implementation of Montage
//

inline Magick::Montage::Montage ( void )
  : _backgroundColor(),
    _compose(UndefinedCompositeOp),
    _fileName(),
    _font(),
    _geometry(),
    _gravity(0),
    _label(),
    _pen(),
    _pointSize(0),
    _shadow(false),
    _texture(),
    _tile(),
    _title(),
    _transparentColor()
{
}

inline Magick::Montage::~Montage( void )
{
  // Nothing to do
}

inline void Magick::Montage::backgroundColor ( const Magick::Color &backgroundColor_ )
{
  _backgroundColor = backgroundColor_;
}
inline Magick::Color Magick::Montage::backgroundColor ( void ) const
{
  return _backgroundColor;
}

inline void Magick::Montage::compose ( Magick::CompositeOperator compose_ )
{
  _compose = compose_;
}
inline Magick::CompositeOperator Magick::Montage::compose ( void ) const
{
  return _compose;
}

inline void Magick::Montage::fileName( std::string fileName_ )
{
  _fileName = fileName_;
}
inline std::string Magick::Montage::fileName( void ) const
{
  return _fileName;
}

inline void Magick::Montage::font ( std::string font_ )
{
  _font = font_;
}
inline std::string Magick::Montage::font ( void ) const
{
  return _font;
}

inline void Magick::Montage::geometry ( const Geometry &geometry_ )
{
  _geometry = geometry_;
}
inline Magick::Geometry Magick::Montage::geometry ( void ) const
{
  return _geometry;
}

inline void Magick::Montage::gravity ( unsigned int gravity_ )
{
  _gravity = gravity_;
}
inline unsigned int Magick::Montage::gravity ( void ) const
{
  return _gravity;
}

// Apply as attribute to all images before doing montage
inline void Magick::Montage::label( std::string label_ )
{
  _label = label_;
}
inline std::string Magick::Montage::label( void ) const
{
  return _label;
}

inline void Magick::Montage::penColor ( const Color &pen_ )
{
  _pen = pen_;
}
inline Magick::Color Magick::Montage::penColor ( void ) const
{
  return _pen;
}

inline void Magick::Montage::pointSize ( unsigned int pointSize_ )
{
  _pointSize = pointSize_;
}
inline unsigned int Magick::Montage::pointSize ( void ) const
{
  return _pointSize;
}

inline void Magick::Montage::shadow ( bool shadow_ )
{
  _shadow = shadow_;
}
inline bool Magick::Montage::shadow ( void ) const
{
  return _shadow;
}

inline void Magick::Montage::texture ( std::string texture_ )
{
  _texture = texture_;
}
inline std::string Magick::Montage::texture ( void ) const
{
  return _texture;
}

inline void Magick::Montage::tile ( const Geometry &tile_ )
{
  _tile = tile_;
}
inline Magick::Geometry Magick::Montage::tile ( void ) const
{
  return _tile;
}

inline void Magick::Montage::title ( std::string title_ )
{
  _title = title_;
}
inline std::string Magick::Montage::title ( void ) const
{
  return _title;
}

// Applied after the fact to montage with TransparentImage()
inline void Magick::Montage::transparentColor ( const Magick::Color &transparentColor_ )
{
  _transparentColor = transparentColor_;
}
inline Magick::Color Magick::Montage::transparentColor ( void ) const
{
  return _transparentColor;
}

//
// Implementation of MontageFramed
//

inline Magick::MontageFramed::MontageFramed ( void )
  : _borderColor(),
    _borderWidth(0),
    _frame(),
    _matteColor()
{
}

inline /* virtual */ Magick::MontageFramed::~MontageFramed ( void )
{
  // Nothing to do
}

inline void Magick::MontageFramed::borderColor ( const Magick::Color &borderColor_ )
{
  _borderColor = borderColor_;
}
inline Magick::Color Magick::MontageFramed::borderColor ( void ) const
{
  return _borderColor;
}

inline void Magick::MontageFramed::borderWidth ( unsigned int borderWidth_ )
{
  _borderWidth = borderWidth_;
}
inline unsigned int Magick::MontageFramed::borderWidth ( void ) const
{
  return _borderWidth;
}

inline void Magick::MontageFramed::frameGeometry ( const Magick::Geometry &frame_ )
{
  _frame = frame_;
}
inline Magick::Geometry Magick::MontageFramed::frameGeometry ( void ) const
{
  return _frame;
}

inline void Magick::MontageFramed::matteColor ( const Magick::Color &matteColor_ )
{
  _matteColor = matteColor_;
}
inline Magick::Color Magick::MontageFramed::matteColor ( void ) const
{
  return _matteColor;
}

#endif // Montage_header

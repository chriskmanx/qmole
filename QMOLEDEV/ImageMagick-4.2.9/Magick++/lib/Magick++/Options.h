// This may look like C code, but it is really -*- C++ -*-
//
// Copyright Bob Friesenhahn, 1999
//
// Definition of Options
//
// Options which may be applied to an image. These options are the
// equivalent of options supplied to ImageMagick utilities.
//
// This is an internal implementation class and is not part of the
// Magick++ API
//

#if !defined(Options_header)
#define Options_header

#include <string>
#include <Magick++/Color.h>
#include <Magick++/Geometry.h>
#include <Magick++/Include.h>

namespace Magick
{
  using MagickLib::ImageInfo;
  using MagickLib::QuantizeInfo;

  class Image;

  class Options
  {
    friend class Image;
  public:
    Options( void );
    Options( const Options& options_ );
    ~Options();

    // Remove pixel aliasing
    void            antiAlias( bool flag_ );
    bool            antiAlias( void );
    
    // Join images into a single multi-image file
    void            adjoin ( bool flag_ );
    bool            adjoin ( void ) const;
    
    // Time in 1/100ths of a second which must expire before displaying
    // the next image in an animated sequence.
    void            animationDelay ( unsigned int delay_ );
    unsigned int    animationDelay ( void ) const;
    
    // Number of iterations to loop an animation (e.g. Netscape loop
    // extension) for.
    void            animationIterations ( unsigned int iterations_ );
    unsigned int    animationIterations ( void ) const;
    
    // Image background color
    void            backgroundColor ( const Color &color_ );
    Color           backgroundColor ( void ) const;
    
    // Name of texture image to tile onto the image background
    void            backgroundTexture ( const std::string &backgroundTexture_ );
    std::string     backgroundTexture ( void ) const;
    
    // Image border color
    void            borderColor ( const Color &color_ );
    Color           borderColor ( void ) const;
    
    // Text bounding-box base color (default none)
    void            boxColor ( const Color &boxColor_ );
    Color           boxColor ( void ) const;
    
    // Colors within this distance are considered equal
    void            colorFuzz ( unsigned int fuzz_ );
    unsigned int    colorFuzz ( void ) const;
    
    // Compression type ( NoCompression, BZipCompression,
    // FaxCompression, JPEGCompression, LZWCompression,
    // RunlengthEncodedCompression, or ZipCompression )
    void            compressType ( CompressionType compressType_ );
    CompressionType compressType ( void ) const;
    
    // Vertical and horizontal resolution in pixels of the image
    void            density ( const Geometry &geomery_ );
    Geometry        density ( void ) const;
    
    void            depth ( unsigned int depth_ );
    unsigned int    depth ( void ) const;
    
    void            fileName ( const std::string &fileName_ );
    std::string     fileName ( void ) const;
    
    void            font ( const std::string &font_ );
    std::string     font ( void ) const;
    
    void            fontPointsize ( unsigned int pointSize_ );
    unsigned int    fontPointsize ( void ) const;
    
    std::string     format ( void ) const;
    
    void            gifDisposeMethod ( unsigned int disposeMethod_ );
    unsigned int    gifDisposeMethod ( void ) const;
    
    void            interlaceType ( InterlaceType interlace_ );
    InterlaceType   interlaceType ( void ) const;
    
    // Linewidth for drawing vector objects (default one)
    void            lineWidth ( unsigned int lineWidth_ );
    unsigned int    lineWidth ( void ) const;
    
    void            magick ( const std::string &magick_ );
    std::string     magick ( void ) const;
    
    // Transparent color
    void            matteColor ( const Color &matteColor_ );
    Color           matteColor ( void ) const;
    
    void            monochrome ( bool monochromeFlag_ );
    bool            monochrome ( void ) const;
    
    void            penColor ( const Color &penColor_ );
    Color           penColor ( void  ) const;

    // Pen texture image.
    void            penTexture ( const MagickLib::Image *penTexture_ );
    const MagickLib::Image* penTexture ( void  ) const;

    void            psPageSize ( const Geometry &pageSize_ );
    Geometry        psPageSize ( void ) const;
    
    void            quality ( unsigned int quality_ );
    unsigned int    quality ( void ) const;
    
    // Maximum number of colors to quantize to
    void            quantizeColors ( unsigned int colors_ );
    unsigned int    quantizeColors ( void ) const;
    
    // Colorspace to quantize in.
    void            quantizeColorSpace ( ColorspaceType colorSpace_ );
    ColorspaceType  quantizeColorSpace ( void ) const;
    
    // Dither image during quantization.
    void            quantizeDither ( bool ditherFlag_ );
    bool            quantizeDither ( void ) const;
    
    // Quantization tree-depth
    void            quantizeTreeDepth ( unsigned int treeDepth_ );
    unsigned int    quantizeTreeDepth ( void ) const;
    
    void            resolutionUnits ( ResolutionType resolutionUnits_ );
    ResolutionType  resolutionUnits ( void ) const;
    
    void            size ( const Geometry &geometry_ );
    Geometry        size ( void ) const;
    
    void            subImage ( unsigned int subImage_ );
    unsigned int    subImage ( void ) const;
    
    void            subRange ( unsigned int subRange_ );
    unsigned int    subRange ( void ) const;
    
    void            tileName ( const std::string &tileName_ );
    std::string     tileName ( void ) const;
    
    void            verbose ( bool verboseFlag_ );
    bool            verbose ( void ) const;
    
    void            view ( const std::string &view_ );
    std::string     view ( void ) const;
    
    void            x11Display ( const std::string &display_ );
    std::string     x11Display ( void ) const;
    
    //
    // Internal implementation methods.  Please do not use.
    //
    ImageInfo *     imageInfo( void );
    QuantizeInfo *  quantizeInfo( void );
    
  private:
    
    // Not supported
    Options operator=(const Options&);
    
    ImageInfo*        _imageInfo;
    QuantizeInfo*     _quantizeInfo;
    MagickLib::Image* _penTexture;
  };
} // namespace Magick

//
// Inlines
//

inline void Magick::Options::antiAlias( bool flag_ )
{
  _imageInfo->antialias = static_cast<unsigned int>( flag_ );
}
inline bool Magick::Options::antiAlias( void )
{
  // Manually cast to bool to avoid warnings
  // and in case bool is only emulated.
  if ( _imageInfo->antialias )
    return true;

  return false;
}

inline void Magick::Options::adjoin ( bool flag_ )
{
  _imageInfo->adjoin = (unsigned int) flag_;
}
inline bool Magick::Options::adjoin ( void ) const
{
  if ( _imageInfo->adjoin )
    return true;
  else
    return false;
}

inline void Magick::Options::colorFuzz ( unsigned int fuzz_ )
{
  _imageInfo->fuzz = fuzz_;
}
inline unsigned int Magick::Options::colorFuzz ( void ) const
{
  return _imageInfo->fuzz;
}

inline void Magick::Options::compressType ( CompressionType compressType_ )
{
  _imageInfo->compression = compressType_;
}
inline Magick::CompressionType Magick::Options::compressType ( void ) const
{
  return (Magick::CompressionType)_imageInfo->compression;
}

inline void Magick::Options::depth ( unsigned int depth_ )
{
  _imageInfo->depth = depth_;
}
inline unsigned int Magick::Options::depth ( void ) const
{
  return _imageInfo->depth;
}

inline void Magick::Options::fontPointsize ( unsigned int pointSize_ )
{
  _imageInfo->pointsize = pointSize_;
}
inline unsigned int Magick::Options::fontPointsize ( void ) const
{
  return _imageInfo->pointsize;
}

inline void Magick::Options::interlaceType ( Magick::InterlaceType interlace_ )
{
  _imageInfo->interlace = interlace_;
}
inline Magick::InterlaceType Magick::Options::interlaceType ( void ) const
{
  return (Magick::InterlaceType)_imageInfo->interlace;
}


// Linewidth for drawing lines, circles, ellipses, etc.
inline void Magick::Options::lineWidth ( unsigned int lineWidth_ )
{
  _imageInfo->linewidth = lineWidth_;
}
inline unsigned int Magick::Options::lineWidth ( void ) const
{
  return _imageInfo->linewidth;
}

inline void Magick::Options::monochrome ( bool monochromeFlag_ )
{
  _imageInfo->monochrome = monochromeFlag_;
}
inline bool Magick::Options::monochrome ( void ) const
{
  if ( _imageInfo->monochrome )
    return true;

    return false;
}

inline void Magick::Options::quantizeColors ( unsigned int colors_ )
{
  _quantizeInfo->number_colors = colors_;
}
inline unsigned int Magick::Options::quantizeColors ( void ) const
{
  return _quantizeInfo->number_colors;
}

inline void Magick::Options::quantizeColorSpace ( Magick::ColorspaceType colorSpace_ )
{
  _quantizeInfo->colorspace = colorSpace_;
}
inline Magick::ColorspaceType Magick::Options::quantizeColorSpace ( void ) const
{
  return (Magick::ColorspaceType)_quantizeInfo->colorspace;
}

inline void Magick::Options::quantizeDither ( bool ditherFlag_ )
{
  _imageInfo->dither = ditherFlag_;
  _quantizeInfo->dither = ditherFlag_;
}
inline bool Magick::Options::quantizeDither ( void ) const
{
  if ( _imageInfo->dither )
    return true;

    return false;
}

inline void Magick::Options::quantizeTreeDepth ( unsigned int treeDepth_ )
{
  _quantizeInfo->tree_depth = treeDepth_;
}
inline unsigned int Magick::Options::quantizeTreeDepth ( void ) const
{
  return _quantizeInfo->tree_depth;
}

inline void Magick::Options::quality ( unsigned int quality_ )
{
  _imageInfo->quality = quality_;
}
inline unsigned int Magick::Options::quality ( void ) const
{
  return _imageInfo->quality;
}

inline void Magick::Options::subImage ( unsigned int subImage_ )
{
  _imageInfo->subimage = subImage_;
}
inline unsigned int Magick::Options::subImage ( void ) const
{
  return _imageInfo->subimage;
}

inline void Magick::Options::subRange ( unsigned int subRange_ )
{
  _imageInfo->subrange = subRange_;
}
inline unsigned int Magick::Options::subRange ( void ) const
{
  return _imageInfo->subrange;
}

inline void Magick::Options::verbose ( bool verboseFlag_ )
{
  _imageInfo->verbose = verboseFlag_;
}
inline bool Magick::Options::verbose ( void ) const
{
  if ( _imageInfo->verbose )
    return true;

  return false;
}

inline MagickLib::ImageInfo * Magick::Options::imageInfo( void )
{
  return _imageInfo;
}

inline MagickLib::QuantizeInfo * Magick::Options::quantizeInfo( void )
{
  return _quantizeInfo;
}

#endif // Options_header

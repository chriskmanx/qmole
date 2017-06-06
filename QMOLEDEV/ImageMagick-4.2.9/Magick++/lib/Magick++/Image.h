// This may look like C code, but it is really -*- C++ -*-
//
// Copyright Bob Friesenhahn, 1999
//
// Definition of Image, the representation of a single image in Magick++
//


#if !defined(Image_header)
#define Image_header

#include <string>
#include <list>

#include <Magick++/Blob.h>
#include <Magick++/Color.h>
#include <Magick++/Drawable.h>
#include <Magick++/Exception.h>
#include <Magick++/Geometry.h>
#include <Magick++/Options.h>
#include <Magick++/LastError.h>
#include <Magick++/Include.h>

namespace Magick
{
  // Forward declarations
  class Image;
  class ImageRef;

  const std::string borderGeometryDefault = "6x6+0+0";
  const std::string frameGeometryDefault  = "25x25+6+6";
  const std::string raiseGeometryDefault  = "6x6+0+0";

  // Compare two Image objects regardless of LHS/RHS
  // Image sizes and signatures are used as basis of comparison
  int operator == ( const Magick::Image& left_, const Magick::Image& right_ );
  int operator != ( const Magick::Image& left_, const Magick::Image& right_ );
  int operator >  ( const Magick::Image& left_, const Magick::Image& right_ );
  int operator <  ( const Magick::Image& left_, const Magick::Image& right_ );
  int operator >= ( const Magick::Image& left_, const Magick::Image& right_ );
  int operator <= ( const Magick::Image& left_, const Magick::Image& right_ );

  //
  // Image is the representation of an image.  In reality, it actually
  // a handle object which contains a pointer to a shared reference
  // object (ImageRef). As such, this object is extremely space efficient.
  //
  class Image
  {
  public:
    // Construct from image file or image specification
    Image( const std::string &imageSpec_ );
    
    // Construct a blank image canvas of specified size and color
    Image( const Geometry &size_, const Color &color_ );

    // Construct Image from in-memory BLOB
    Image ( const Blob &blob_ );

    // Construct Image of specified size from in-memory BLOB
    Image ( const Geometry &size_, const Blob &blob_ );
    
    // Default constructor
    Image( void );
    
    // Destructor
    ~Image();
    
    /// Copy constructor
    Image ( const Image & image_ );
    
    // Assignment operator
    Image operator= ( const Image &image_ );
    
    //
    // Image operations
    //
    
    // Add noise to image with specified noise type
    void            addNoise ( NoiseType noiseType_ );
    
    // Annotate image (draw text on image)
    void            annotate ( const std::string &text_,
			       const Geometry &location_ );
    void            annotate ( const std::string &text_,
			       const Geometry &location_,
			       GravityType gravity_ );
    void            annotate ( const std::string &text_,
			       const Geometry &location_,
			       GravityType gravity_,
			       double degrees_ );
    void            annotate ( const std::string &text_,
			       GravityType gravity_ = NorthWestGravity);
    
    // Blur image with specified blur factor
    void            blur ( double factor_ );
    
    // Border image (add border to image)
    void            border ( const Geometry &geometry_ = borderGeometryDefault );
    
    // Charcoal effect image (looks like charcoal sketch)
    void            charcoal ( double factor_ = 50 );
    
    // Chop image (remove vertical or horizontal subregion of image)
    // FIXME: describe how geometry argument is used to select either horizontal or
    // vertical subregion of image.
    void            chop ( const Geometry &geometry_ );
    
    // Colorize opaque color in image using pen color
    void            colorize ( const Color &opaqueColor_,
			       const Color &penColor_ );
    
    // Comment image (add comment string to image)
    void            comment ( const std::string &comment_ );
    
    // Compose an image onto another at specified offset and using
    // specified algorithm
    void            composite ( const Image &compositeImage_,
				int xOffset_,
				int yOffset_,
				CompositeOperator compose_ = InCompositeOp );
    void            composite ( const Image &compositeImage_,
				const Geometry &offset_,
				CompositeOperator compose_ = InCompositeOp );
    void            composite ( const Image &compositeImage_,
				GravityType gravity_,
				CompositeOperator compose_ = InCompositeOp );
    
    // Condense image (Re-run-length encode image in memory)
    void            condense ( void );
    
    // Contrast image (enhance intensity differences in image)
    void            contrast ( unsigned int sharpen_ );
    
    // Crop image (subregion of original image)
    void            crop ( const Geometry &geometry_ );
    
    // Cycle image colormap
    void            cycleColormap ( int amount_ );
    
    // Despeckle image (reduce speckle noise)
    void            despeckle ( void );
    
    // Display image on screen
    void            display ( void );
    
    // Draw on image using a single drawable
    void            draw ( const Drawable &drawable_ );

    // Draw on image using a drawable list
    void            draw ( const std::list<Magick::Drawable> &drawable_ );
    
    // Edge image (hilight edges in image)
    void            edge ( double factor_ );
    
    // Emboss image (hilight edges with 3D effect)
    void            emboss ( void );
    
    // Enhance image (minimize noise)
    void            enhance ( void );
    
    // Equalize image (histogram equalization)
    void            equalize ( void );
    
    // Flip image (reflect each scanline in the vertical direction)
    void            flip ( void );

    // Flood-fill color across pixels that match the color of the
    // target pixel and are neighbors of the target pixel.
    // Uses current fuzz setting when determining color match.
    void            floodFillColor( int x_, int y_,
				    const Color &fillColor_ );
    void            floodFillColor( const Geometry &point_,
				    const Color &fillColor_ );

    // Flood-fill color across pixels starting at target-pixel and
    // stopping at pixels matching specified border color.
    // Uses current fuzz setting when determining color match.
    void            floodFillColor( int x_, int y_,
				    const Color &fillColor_,
				    const Color &borderColor_ );
    void            floodFillColor( const Geometry &point_,
				    const Color &fillColor_,
				    const Color &borderColor_ );

    // Flood-fill texture across pixels that match the color of the
    // target pixel and are neighbors of the target pixel.
    // Uses current fuzz setting when determining color match.
    void            floodFillTexture( int x_, int y_,
				      const Image &texture_ );
    void            floodFillTexture( const Geometry &point_,
				      const Image &texture_ );

    // Flood-fill texture across pixels starting at target-pixel and
    // stopping at pixels matching specified border color.
    // Uses current fuzz setting when determining color match.
    void            floodFillTexture( int x_, int y_,
				      const Image &texture_,
				      const Color &borderColor_ );
    void            floodFillTexture( const Geometry &point_,
				      const Image &texture_,
				      const Color &borderColor_ );
    
    // Flop image (reflect each scanline in the horizontal direction)
    void            flop ( void );
    
    // Frame image
    void            frame ( const Geometry &geometry_ = frameGeometryDefault );
    void            frame ( unsigned int width_, unsigned int height_,
			    int innerBevel_ = 6, int outerBevel_ = 6 );
    
    // Gamma correct image
    void            gamma ( double gamma_ );
    void            gamma ( double gammaRed_,
			    double gammaGreen_,
			    double gammaBlue_ );
    
    // Implode image (special effect)
    void            implode ( double factor_ );
    
    // Label image
    void            label ( const std::string &label_ );
    
    // Extract layer from image
    void            layer ( LayerType layer_ );
    
    // Magnify image by integral size
    void            magnify ( void );
    
    // Remap image colors with closest color from reference image
    void            map ( const Image &mapImage_ , bool dither_ = false );
    
    // Floodfill designated area with a matte value
    void            matteFloodfill ( const Color &target_ ,
				     unsigned int matte_,
				     int x_, int y_,
				     PaintMethod method_ );
    
    // Reduce image by integral size
    void            minify ( void );
    
    // Modulate percent hue, saturation, and brightness of an image
    void            modulate ( double brightness_,
			       double saturation_,
			       double hue_ );
    
    // Negate colors in image.  Set grayscale to only negate grayscale
    // values in image.
    void            negate ( bool grayscale_ = false );
    
    // Normalize image (increase contrast by normalizing the pixel
    // values to span the full range of color values)
    void            normalize ( void );
    
    // Oilpaint image (image looks like oil painting)
    void            oilPaint ( unsigned int radius_ = 3 );
    
    // Change color of opaque pixel to specified pen color.
    void            opaque ( const Color &opaqueColor_,
			     const Color &penColor_ );

    // Ping is similar to read except only enough of the image is read
    // to determine the image columns, rows, and filesize.  Access the
    // columns(), rows(), and fileSize() attributes after invoking
    // ping.  The image data is not valid after calling ping.
    void            ping ( const std::string &imageSpec_ );
    
    // Quantize image (reduce number of colors)
    void            quantize ( bool measureError_ = false );
    
    // Raise image (lighten or darken the edges of an image to give a
    // 3-D raised or lowered effect)
    void            raise ( const Geometry &geometry_ = raiseGeometryDefault,
			    bool raisedFlag_ = false );
    
    // Read image into current object
    void            read ( const std::string &imageSpec_ );

    // Read image of specified size into current object
    void            read ( const Geometry &size_,
			   const std::string &imageSpec_ );

    // Read image from in-memory BLOB
    void            read ( const Blob &blob_ );

    // Read image of specified size from in-memory BLOB
    void            read ( const Geometry &size_,
			   const Blob &blob_ );
    
    // Reduce noise in image using a noise peak elimination filter
    void            reduceNoise ( void );
    
    // Roll image (rolls image vertically and horizontally) by specified
    // number of columnms and rows)
    void            roll ( const Geometry &roll_ );
    void            roll ( int columns_,
			   int rows_ );
    
    // Rotate image counter-clockwise by specified number of degrees.
    // Optionally crop image to original size and sharpen image.
    void            rotate ( double degrees_,
			     bool crop_ = false,
			     unsigned int sharpen_ = false );
    
    // Resize image by using pixel sampling algorithm
    void            sample ( const Geometry &geometry_ );
    
    // Resize image by using simple ratio algorithm
    void            scale ( const Geometry &geometry_ );
    
    // Segment (coalesce similar image components) by analyzing the
    // histograms of the color components and identifying units that are
    // homogeneous with the fuzzy c-means technique.
    // Also uses QuantizeColorSpace and Verbose image attributes
    void            segment ( double clusterThreshold_ = 1.0, 
			      double smoothingThreshold_ = 1.5 );
    
    // Shade image using distant light source
    void            shade ( double azimuth_ = 30,
			    double elevation_ = 30,
			    bool   colorShading_ = false );
    
    // Sharpen pixels in image
    void            sharpen ( double factor_ );
    
    // Shear image (create parallelogram by sliding image by X or Y axis)
    void            shear ( double xShearAngle_,
			    double yShearAngle_,
			    bool crop_ = false );
    
    // Solarize image (similar to effect seen when exposing a
    // photographic film to light during the development process)
    void            solarize ( double factor_ = 50.0 );
    
    // Spread pixels randomly within image by specified ammount
    void            spread ( unsigned int amount_ = 3 );
    
    // Add a digital watermark to the image (based on second image)
    void            stegano ( const Image &watermark_ );
    
    // Create an image which appears in stereo when viewed with red-blue glasses
    // (Red image on left, blue on right)
    void            stereo ( const Image &rightImage_ );
    
    // Swirl image (image pixels are rotated by degrees)
    void            swirl ( double degrees_ );
    
    // Layer a texture on image background
    void            texture ( const Image &texture_ );
    
    // Threshold image
    void            threshold ( double threshold_ );
    
    // Transform image based on image and crop geometries
    // Crop geometry is optional
    void            transform ( const Geometry &imageGeometry_ );
    void            transform ( const Geometry &imageGeometry_,
				const Geometry &cropGeometry_  );

    // Convert the image colorspace representation
    void            transformColorSpace( ColorspaceType colorSpace_ );

    // Add matte image to image, setting pixels matching color to transparent
    void            transparent ( const Color &color_ );
    
    // Trim edges that are the background color from the image
    void            trim ( void );

    // Un-condense image (Uncompresses runlength-encoded pixels
    // packets to a rectangular array of pixels)
    void            uncondense ( void );
    
    // Map image pixels to a sine wave
    void            wave ( double amplitude_ = 25.0, double wavelength_ = 150.0 );
    
    // Write image to a file
    void            write ( const std::string &imageSpec_ );

    // Write image to in-memory BLOB.  A length estimate may be specified to
    // avoid overhead associated with reallocation of memory if the output
    // size can be estimated in advance.
    void            write ( Blob *blob_,
			    unsigned long lengthEstimate_ = 1664 );
    
    // Zoom image to specified size.
    void            zoom ( const Geometry &geometry_ );
    
    //
    // Image Option Accessors
    //

    // Anti-alias Postscript and TrueType fonts (default true)
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
    
    // Base image width (before transformations)
    unsigned int    baseColumns ( void ) const;
    
    // Base image filename (before transformations)
    std::string     baseFilename ( void ) const;
    
    // Base image height (before transformations)
    unsigned int    baseRows ( void ) const;
    
    // Image border color
    void            borderColor ( const Color &color_ );
    Color           borderColor ( void ) const;
    
    // Text bounding-box base color (default none)
    void            boxColor ( const Color &boxColor_ );
    Color           boxColor ( void ) const;
    
    // Chromaticity blue primary point (e.g. x=0.15, y=0.06)
    void            chromaBluePrimary ( float x_, float y_ );
    void            chromaBluePrimary ( float *x_, float *y_ ) const;
    
    // Chromaticity green primary point (e.g. x=0.3, y=0.6)
    void            chromaGreenPrimary ( float x_, float y_ );
    void            chromaGreenPrimary ( float *x_, float *y_ ) const;
    
    // Chromaticity red primary point (e.g. x=0.64, y=0.33)
    void            chromaRedPrimary ( float x_, float y_ );
    void            chromaRedPrimary ( float *x_, float *y_ ) const;
    
    // Chromaticity white point (e.g. x=0.3127, y=0.329)
    void            chromaWhitePoint ( float x_, float y_ );
    void            chromaWhitePoint ( float *x_, float *y_ ) const;
    
    // Image class (DirectClass or PseudoClass)
    // NOTE: setting a DirectClass image to PseudoClass will result in
    // the loss of color information if the number of colors in the
    // image is greater than the maximum palette size (either 256 or
    // 65536 entries depending on whether QuantumLeap was specified
    // when ImageMagick was built).
    void            classType ( ClassType class_ );
    ClassType       classType ( void ) const;
    
    // Colors within this distance are considered equal
    void            colorFuzz ( unsigned int fuzz_ );
    unsigned int    colorFuzz ( void ) const;
    
    // Color at colormap position index_
    void            colorMap ( unsigned int index_, const Color &color_ );
    Color           colorMap ( unsigned int index_ ) const;
    
    // Image width
    unsigned int    columns ( void ) const;
    
    // Image comment
    std::string     comment ( void ) const;
    
    // Compression type
    void            compressType ( CompressionType compressType_ );
    CompressionType compressType ( void ) const;

    // Image pixels are condensed (Run-Length encoded)
    bool            condensed( void ) const;
    
    // Vertical and horizontal resolution in pixels of the image
    void            density ( const Geometry &geomery_ );
    Geometry        density ( void ) const;

    // Image depth (bits allocated to red/green/blue components)
    void            depth ( unsigned int depth_ );
    unsigned int    depth ( void ) const;

    // Tile names from within an image montage
    std::string     directory ( void ) const;

    // Image file name
    void            fileName ( const std::string &fileName_ );
    std::string     fileName ( void ) const;

    // Number of bytes of the image on disk
    unsigned int    fileSize ( void ) const;

    // Filter to use when resizing image
    void            filterType ( FilterType filterType_ );
    FilterType      filterType ( void ) const;

    // Text rendering font
    void            font ( const std::string &font_ );
    std::string     font ( void ) const;

    // Font point size
    void            fontPointsize ( unsigned int pointSize_ );
    unsigned int    fontPointsize ( void ) const;

    // Long image format description
    std::string     format ( void ) const;

    // Gamma level of the image
    double          gamma ( void ) const;

    // Preferred size of the image when encoding
    Geometry        geometry ( void ) const;

    // GIF disposal method
    void            gifDisposeMethod ( unsigned int disposeMethod_ );
    unsigned int    gifDisposeMethod ( void ) const;

    // ICC color profile (BLOB)
    void            iccColorProfile( const Blob &colorProfile_ );
    Blob            iccColorProfile( void ) const;

    // Type of interlacing to use
    void            interlaceType ( InterlaceType interlace_ );
    InterlaceType   interlaceType ( void ) const;

    // IPTC profile (BLOB)
    void            iptcProfile( const Blob& iptcProfile_ );
    Blob            iptcProfile( void ) const;

    // Does object contain valid image?
    void            isValid ( bool isValid_ );
    bool            isValid ( void ) const;

    // Image label
    std::string     label ( void ) const;
    
    // Linewidth for drawing vector objects (default one)
    void            lineWidth ( unsigned int lineWidth_ );
    unsigned int    lineWidth ( void ) const;
    
    // File type magick identifier (.e.g "GIF")
    void            magick ( const std::string &magick_ );
    std::string     magick ( void ) const;
    
    // Image supports transparency (matte layer)
    void            matte ( bool matteFlag_ );
    bool            matte ( void ) const;
    
    // Transparent color
    void            matteColor ( const Color &matteColor_ );
    Color           matteColor ( void ) const;
    
    // The mean error per pixel computed when an image is color reduced
    double          meanErrorPerPixel ( void ) const;

    // Tile size and offset within an image montage
    Geometry        montageGeometry ( void ) const;

    // Transform image to black and white
    void            monochrome ( bool monochromeFlag_ );
    bool            monochrome ( void ) const;

    // The normalized max error per pixel computed when an image is
    // color reduced.
    double          normalizedMaxError ( void ) const;

    // The normalized mean error per pixel computed when an image is
    // color reduced.
    double          normalizedMeanError ( void ) const;

    // The number of runlength-encoded packets in the image
    unsigned int    packets ( void ) const;

    // The number of bytes in each pixel packet
    unsigned int    packetSize ( void ) const;

    // Pen color
    void            penColor ( const Color &penColor_ );
    Color           penColor ( void  ) const;

    // Pen texture image.
    void            penTexture ( const Image &penTexture_ );
    Image           penTexture ( void  ) const;

    // Get/set pixel color at location x & y.
    void            pixelColor ( unsigned int x_, unsigned int y_,
				 const Color &color_ );
    Color           pixelColor ( unsigned int x_, unsigned int y_ );

    // Postscript page size. 
    void            psPageSize ( const Geometry &pageSize_ );
    Geometry        psPageSize ( void ) const;

    // JPEG/MIFF/PNG compression level (default 75).
    void            quality ( unsigned int quality_ );
    unsigned int    quality ( void ) const;
    
    // Maximum number of colors to quantize to
    void            quantizeColors ( unsigned int colors_ );
    unsigned int    quantizeColors ( void ) const;
    
    // Colorspace to quantize in.
    void            quantizeColorSpace ( ColorspaceType colorSpace_ );
    ColorspaceType  quantizeColorSpace ( void ) const;
    
    // Dither image during quantization (default true).
    void            quantizeDither ( bool ditherFlag_ );
    bool            quantizeDither ( void ) const;

    // Quantization error.  Only valid if verbose is set to true
    // prior to executing quantize and the value is read back
    // immediately.
    unsigned int    quantizeError ( void );
    
    // Quantization tree-depth
    void            quantizeTreeDepth ( unsigned int treeDepth_ );
    unsigned int    quantizeTreeDepth ( void ) const;

    // The type of rendering intent
    void            renderingIntent ( RenderingIntent renderingIntent_ );
    RenderingIntent renderingIntent ( void ) const;

    // Units of image resolution
    void            resolutionUnits ( ResolutionType resolutionUnits_ );
    ResolutionType  resolutionUnits ( void ) const;

    // The number of pixel rows in the image
    unsigned int    rows ( void ) const;

    // Image scene number
    void            scene ( unsigned int scene_ );
    unsigned int    scene ( void ) const;
    
    // Image signature.  Set force_ to true in order to re-calculate
    // the signature regardless of whether the image data has been
    // modified.
    std::string     signature ( bool force_ = false ) const;

    // Width and height of a raw image 
    void            size ( const Geometry &geometry_ );
    Geometry        size ( void ) const;

    // Subimage of an image sequence
    void            subImage ( unsigned int subImage_ );
    unsigned int    subImage ( void ) const;

    // Number of images relative to the base image
    void            subRange ( unsigned int subRange_ );
    unsigned int    subRange ( void ) const;

    // Tile name
    void            tileName ( const std::string &tileName_ );
    std::string     tileName ( void ) const;

    // Number of colors in the image
    unsigned long   totalColors ( void ) const;

    // Image type
    ImageType       type ( void ) const;

    // Print detailed information about the image
    void            verbose ( bool verboseFlag_ );
    bool            verbose ( void ) const;
    
    // FlashPix viewing parameters
    void            view ( const std::string &view_ );
    std::string     view ( void ) const;

    // X11 display to display to, obtain fonts from, or to capture
    // image from
    void            x11Display ( const std::string &display_ );
    std::string     x11Display ( void ) const;

    // x resolution of the image
    double          xResolution ( void ) const;

    // y resolution of the image
    double          yResolution ( void ) const;

    //
    // No user-serviceable parts beyond this point
    //
    
    // Construct with image and options
    Image ( MagickLib::Image* image_, Magick::Options* options_ );

    // Retrieve Image*
    MagickLib::Image*& image( void );
    const MagickLib::Image* constImage( void ) const;

    // Retrieve Options*
    Options* options( void );
    const Options* constOptions( void ) const;

    // Retrieve ImageInfo*
    MagickLib::ImageInfo * imageInfo( void );
    const MagickLib::ImageInfo * constImageInfo( void ) const;

    // Replace current image (reference counted)
    MagickLib::Image* replaceImage ( MagickLib::Image* replacement_ );

    // Prepare to update image (copy if reference > 1)
    void            modifyImage ( void );

    // Test for ImageMagick error and throw exception if error
    void            throwMagickError( void );

  private:

    ImageRef * _imgRef;
  };


  //
  // Reference counted access to Image *
  // (Private implementation class)
  //
  class ImageRef {
    friend class Image; 
  private:
    // Construct with an image pointer and default options
    ImageRef ( MagickLib::Image * image_ );
    // Construct with an image pointer and options
    ImageRef ( MagickLib::Image * image_, const Options * options_ );
    // Construct with null image and default options
    ImageRef ( void );
    // Destroy image and options
    ~ImageRef( void );
    
    // Copy constructor and assignment are not supported
    ImageRef(const ImageRef&);
    ImageRef operator=(const ImageRef&);
    
    void                 image ( MagickLib::Image * image_ );
    MagickLib::Image *&  image ( void );
    void                 imageMissing ( void ) const;
    
    void                 options ( Options * options_ );
    Options *            options ( void );
    void                 optionsMissing( void ) const;
    
    MagickLib::Image *   _image;    // ImageMagick Image
    Options *            _options;  // User-specified options
    int                  _refCount; // Reference count
  };

} // end of namespace Magick

//
// Inlines
//

//
// ImageRef
//

// Set image pointer
inline void Magick::ImageRef::image ( MagickLib::Image * image_ )
{
  _image = image_;
}
// Get image pointer
inline MagickLib::Image *& Magick::ImageRef::image ( void )
{
  if ( !_image )
    imageMissing(); // Throw exception

  return _image;
}

// Set options pointer
inline void  Magick::ImageRef::options ( Options * options_ )
{
  _options = options_;
}
// Get options pointer
inline Magick::Options * Magick::ImageRef::options ( void )
{
  if ( !_options )
    optionsMissing();

  return _options;
}

//
// Image
//

// Get MagickLib::Image*
inline MagickLib::Image*& Magick::Image::image( void )
{
  return _imgRef->image();
}
inline const MagickLib::Image* Magick::Image::constImage( void ) const
{
  return _imgRef->image();
}

// Get Magick::Options*
inline Magick::Options* Magick::Image::options( void )
{
  return _imgRef->options();
}
inline const Magick::Options* Magick::Image::constOptions( void ) const
{
  return _imgRef->options();
}

// Get ImageInfo *
inline MagickLib::ImageInfo* Magick::Image::imageInfo( void )
{
  return _imgRef->options()->imageInfo();
}
inline const MagickLib::ImageInfo * Magick::Image::constImageInfo( void ) const
{
  return _imgRef->options()->imageInfo();
}


#endif // Image_header

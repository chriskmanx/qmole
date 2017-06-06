// This may look like C code, but it is really -*- C++ -*-
//
// Copyright Bob Friesenhahn, 1999
//
// Inclusion of ImageMagick headers (with namespace magic)

#ifndef MagickInclude_header
#define MagickInclude_header

//
// Include ImageMagick headers into namespace "MagickLib". If
// MAGICK_IMPLEMENTATION is defined, include ImageMagick development
// headers.  This scheme minimizes the possibility of conflict with
// user code.
//

#if defined(__BORLANDC__)
//
// The following includes need to be specified to compile with Borland
// C++ Builder 4.0.
//
#include <vcl.h>
#include <stdio.h>

#endif // defined(__BORLANDC__)

// If <clocale> is not included, then SPARC Works 5.0 blows up when
// including locale.h
#include <clocale>

// Needed for stdio FILE
#include <cstdio>

namespace MagickLib
{
#undef PACKAGE
#undef VERSION

#include <magick/api.h>

#ifdef MAGICK_IMPLEMENTATION
# include <magick/defines.h>
#endif // MAGICK_IMPLEMENTATION
#undef class
}

//
// Import ImageMagick symbols and types which are used as part of the
// Magick++ API definition into namespace "Magick".
//
namespace Magick
{
  // The datatype for an RGB component
  using MagickLib::Quantum;

  // Image class types
  using MagickLib::ClassType;
  using MagickLib::UndefinedClass;
  using MagickLib::DirectClass;
  using MagickLib::PseudoClass;
  
  // Color-space types
  using MagickLib::ColorspaceType;
  using MagickLib::UndefinedColorspace;
  using MagickLib::RGBColorspace;
  using MagickLib::GRAYColorspace;
  using MagickLib::TransparentColorspace;
  using MagickLib::OHTAColorspace;
  using MagickLib::XYZColorspace;
  using MagickLib::YCbCrColorspace;
  using MagickLib::YCCColorspace;
  using MagickLib::YIQColorspace;
  using MagickLib::YPbPrColorspace;
  using MagickLib::YUVColorspace;
  using MagickLib::CMYKColorspace;
  using MagickLib::sRGBColorspace;
  
  // Composition operations
  using MagickLib::CompositeOperator;
  using MagickLib::UndefinedCompositeOp;
  using MagickLib::OverCompositeOp;
  using MagickLib::InCompositeOp;
  using MagickLib::OutCompositeOp;
  using MagickLib::AtopCompositeOp;
  using MagickLib::XorCompositeOp;
  using MagickLib::PlusCompositeOp;
  using MagickLib::MinusCompositeOp;
  using MagickLib::AddCompositeOp;
  using MagickLib::SubtractCompositeOp;
  using MagickLib::DifferenceCompositeOp;
  using MagickLib::BumpmapCompositeOp;
  using MagickLib::ReplaceCompositeOp;
  using MagickLib::ReplaceRedCompositeOp;
  using MagickLib::ReplaceGreenCompositeOp;
  using MagickLib::ReplaceBlueCompositeOp;
  using MagickLib::ReplaceMatteCompositeOp;
  
  // Compression algorithms
  using MagickLib::CompressionType;
  using MagickLib::UndefinedCompression;
  using MagickLib::NoCompression;
  using MagickLib::BZipCompression;
  using MagickLib::FaxCompression;
  using MagickLib::Group4Compression;
  using MagickLib::JPEGCompression;
  using MagickLib::LZWCompression;
  using MagickLib::RunlengthEncodedCompression;
  using MagickLib::ZipCompression;
  
  // Filter types
  using MagickLib::FilterType;
  using MagickLib::UndefinedFilter;
  using MagickLib::PointFilter;
  using MagickLib::BoxFilter;
  using MagickLib::TriangleFilter;
  using MagickLib::HermiteFilter;
  using MagickLib::HanningFilter;
  using MagickLib::HammingFilter;
  using MagickLib::BlackmanFilter;
  using MagickLib::GaussianFilter;
  using MagickLib::QuadraticFilter;
  using MagickLib::CubicFilter;
  using MagickLib::CatromFilter;
  using MagickLib::MitchellFilter;
  using MagickLib::LanczosFilter;
  using MagickLib::BesselFilter;
  using MagickLib::SincFilter;
  
  // Image types
  using MagickLib::ImageType;
  using MagickLib::UndefinedType;
  using MagickLib::BilevelType;
  using MagickLib::GrayscaleType;
  using MagickLib::PaletteType;
  using MagickLib::TrueColorType;
  using MagickLib::MatteType;
  using MagickLib::ColorSeparationType;
  
  // Interlace types
  using MagickLib::InterlaceType;
  using MagickLib::UndefinedInterlace;
  using MagickLib::NoInterlace;
  using MagickLib::LineInterlace;
  using MagickLib::PlaneInterlace;
  using MagickLib::PartitionInterlace;
  
  // Layer types
  using MagickLib::LayerType;
  using MagickLib::UndefinedLayer;
  using MagickLib::RedLayer;
  using MagickLib::GreenLayer;
  using MagickLib::BlueLayer;
  using MagickLib::MatteLayer;
  
  // Noise types
  using MagickLib::NoiseType;
  using MagickLib::UniformNoise;
  using MagickLib::GaussianNoise;
  using MagickLib::MultiplicativeGaussianNoise;
  using MagickLib::ImpulseNoise;
  using MagickLib::LaplacianNoise;
  using MagickLib::PoissonNoise;
  
  // Paint methods
  using MagickLib::PaintMethod;
  using MagickLib::PointMethod;
  using MagickLib::ReplaceMethod;
  using MagickLib::FloodfillMethod;
  using MagickLib::FillToBorderMethod;
  using MagickLib::ResetMethod;


  // Preview types
  // Not currently actually used by Magick++
  using MagickLib::UndefinedPreview;
  using MagickLib::RotatePreview;
  using MagickLib::ShearPreview;
  using MagickLib::RollPreview;
  using MagickLib::HuePreview;
  using MagickLib::SaturationPreview;
  using MagickLib::BrightnessPreview;
  using MagickLib::GammaPreview;
  using MagickLib::SpiffPreview;
  using MagickLib::DullPreview;
  using MagickLib::GrayscalePreview;
  using MagickLib::QuantizePreview;
  using MagickLib::DespecklePreview;
  using MagickLib::ReduceNoisePreview;
  using MagickLib::AddNoisePreview;
  using MagickLib::SharpenPreview;
  using MagickLib::BlurPreview;
  using MagickLib::ThresholdPreview;
  using MagickLib::EdgeDetectPreview;
  using MagickLib::SpreadPreview;
  using MagickLib::ShadePreview;
  using MagickLib::RaisePreview;
  using MagickLib::SegmentPreview;
  using MagickLib::SolarizePreview;
  using MagickLib::SwirlPreview;
  using MagickLib::ImplodePreview;
  using MagickLib::WavePreview;
  using MagickLib::OilPaintPreview;
  using MagickLib::CharcoalDrawingPreview;
  using MagickLib::JPEGPreview;
  
  // Rendering intents
  using MagickLib::RenderingIntent;
  using MagickLib::UndefinedIntent;
  using MagickLib::SaturationIntent;
  using MagickLib::PerceptualIntent;
  using MagickLib::AbsoluteIntent;
  using MagickLib::RelativeIntent;
  
  // Resolution units
  using MagickLib::ResolutionType;
  using MagickLib::UndefinedResolution;
  using MagickLib::PixelsPerInchResolution;
  using MagickLib::PixelsPerCentimeterResolution;

  // Bit gravity
  // The enumeration values are safely defined by the X11 protocol.
  enum GravityType
  {
#undef ForgetGravity
    ForgetGravity,	//  0
#undef NorthWestGravity
    NorthWestGravity,	//  1
#undef NorthGravity
    NorthGravity,	//  2
#undef NorthEastGravity
    NorthEastGravity,	//  3
#undef WestGravity
    WestGravity,	//  4
#undef CenterGravity
    CenterGravity,	//  5
#undef EastGravity
    EastGravity,	//  6
#undef SouthWestGravity
    SouthWestGravity,	//  7
#undef SouthGravity
    SouthGravity,	//  8
#undef SouthEastGravity
    SouthEastGravity,	//  9
#undef StaticGravity
    StaticGravity	// 10
  };

}

#endif // MagickInclude_header

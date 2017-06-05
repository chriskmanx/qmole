/********************************************************************************
*                                                                               *
*                        C h a r t   B a s e   W i d g e t                      *
*                                                                               *
*********************************************************************************
* Copyright (C) 2003,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* This library is free software; you can redistribute it and/or                 *
* modify it under the terms of the GNU Lesser General Public                    *
* License as published by the Free Software Foundation; either                  *
* version 2.1 of the License, or (at your option) any later version.            *
*                                                                               *
* This library is distributed in the hope that it will be useful,               *
* but WITHOUT ANY WARRANTY; without even the implied warranty of                *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU             *
* Lesser General Public License for more details.                               *
*                                                                               *
* You should have received a copy of the GNU Lesser General Public              *
* License along with this library; if not, write to the Free Software           *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.    *
*********************************************************************************
* $Id: FXChart.h,v 1.17 2006/01/22 18:01:13 fox Exp $                           *
********************************************************************************/
#ifndef FXCHART_H
#define FXCHART_H


//////////////////////////////  UNDER DEVELOPMENT  //////////////////////////////


// Define FXCHARTAPI for DLL builds
#ifdef FOXDLL
#ifdef CHARTDLL_EXPORTS
#define FXCHARTAPI FXEXPORT
#else
#define FXCHARTAPI FXIMPORT
#endif
#else
#define FXCHARTAPI
#endif


namespace FX {


/// Tickmark placement styles
enum {
  TICKS_OFF     = 0,                            /// No tickmarks
  TICKS_MAJOR   = 0x01,                         /// Display major ticks
  TICKS_MINOR   = 0x02,                         /// Display minor ticks
  TICKS_INSIDE  = 0x04,                         /// Tickmarks inside box
  TICKS_OUTSIDE = 0x08,                         /// Tickmarks outside box
  TICKS_CROSS   = (TICKS_INSIDE|TICKS_OUTSIDE)  /// Tickmarks inside and outside box
  };


/// Tickmark definition
struct Ticks {
  FXuchar  style;               /// Style flags
  FXuchar  majorlength;         /// Major tick length
  FXuchar  minorlength;         /// Minor tick length
  FXuchar  majorweight;         /// Major tick line weight
  FXuchar  minorweight;         /// Minor tick line weight
  FXdouble majorspace;          /// Major tick spacing
  FXdouble minorspace;          /// Minor tick spacing
  FXColor  majorcolor;          /// Major tick color
  FXColor  minorcolor;          /// Minor tick color
  };


/// Line styles
enum {
  LINESTYLE_NONE,               /// No line
  LINESTYLE_SOLID,              /// Solid line
  LINESTYLE_DOTTED,             /// Dotted line
  LINESTYLE_SHORTDASHED,        /// Short dashed line
  LINESTYLE_LONGDASHED,         /// Long dashed line
  LINESTYLE_DOTDASHED           /// Dot-dashed line
  };


/// Line style definition
struct LineStyle {
  FXuchar  style;               /// Line style flags
  FXuchar  width;               /// Line width
  FXuchar  cap;                 /// End cap style
  FXuchar  join;                /// Join style
  FXColor  color;               /// Line color
  FXColor  backcolor;           /// Back color when stippling (may be clear)
  };


/// Fill styles
enum {
  FILLSTYLE_OFF,                /// Not filled
  FILLSTYLE_SOLID,              /// Solid color
  FILLSTYLE_HATCH,              /// Hatch pattern
  FILLSTYLE_TEXTURE,            /// Repeating texture
  FILLSTYLE_IMAGE,              /// Fill with an image
  FILLSTYLE_HORIZONTAL,         /// Horizontal gradient
  FILLSTYLE_VERTICAL,           /// Vertical gradient
  FILLSTYLE_DIAGONAL,           /// Diagonal gradient
  FILLSTYLE_RDIAGONAL           /// Reverse diagonal gradient
  };


/// Fill style definition
struct FillStyle {
  FXuchar   style;              /// Fill style
  FXuchar   hatch;              /// Hatch pattern if hatch style
  FXImage  *image;              /// Image used for texture or image fill
  FXColor   color;              /// Fill color
  FXColor   backcolor;          /// Back color when hatching (may be clear)
  FXColor   lower;              /// Lower gradient color
  FXColor   upper;              /// Upper gradient color
  };


/// Text definition
struct TextStyle {
  FXFont   *font;               /// Text font
  FXColor   color;              /// Text color
  FXColor   shadowcolor;        /// Text shadow color (may be clear)
  FXshort   shadowx;            /// X shadow offset
  FXshort   shadowy;            /// Y shadow offset
  };


/// Tick number definition
struct Numbers {
  FXuchar   style;              /// Number format style
  // ... How to format ... //
  TextStyle textstyle;          /// Text display style
  };


/// Marker styles
enum {
  MARKER_NONE          = 0,     /// Draw nothing
  MARKER_SQUARE        = 1,     /// Draw (solid) square
  MARKER_CIRCLE        = 2,     /// Draw (solid) circle
  MARKER_DIAMOND       = 3,     /// Draw (solid) diamond
  MARKER_TRIANGLE_UP   = 4,     /// Draw (solid) upward triangle
  MARKER_TRIANGLE_DN   = 5,     /// Draw (solid) downward triangle
  MARKER_TRIANGLE_LT   = 6,     /// Draw (solid) leftward triangle
  MARKER_TRIANGLE_RT   = 7,     /// Draw (solid) rightward triangle
  MARKER_SOLID         = 8      /// Fill shape
  };


/// Marker definition
struct Marker {
  FXuchar style;                /// Marker style
  FXColor color;                /// Color of markers
  FXint   size;                 /// How big to draw markers
  };


/// Caption definition
struct Caption {
  FXString  caption;            /// Text string
  TextStyle textstyle;          /// Text display style
  };


/// Grid styles
enum {
  GRID_OFF   = 0,               /// No grid displayed
  GRID_MAJOR = 1,               /// Draw grid lines at major ticks
  GRID_MINOR = 2                /// Draw grid lines at minor ticks
  };


/// Grid defintion
struct Grid {
  FXuchar   style;              /// Grid draw style
  LineStyle major;              /// Major grid line styles
  LineStyle minor;              /// Minor grid line styles
  };


/// Axis styles
enum {
  AXIS_OFF       = 0,           /// Nothing drawn on axis
  AXIS_CAPTION   = 0x0001,      /// Axis label drawn
  AXIS_NUMBERS   = 0x0002,      /// Draw numbers on major ticks
  AXIS_UNITS     = 0x0004,      /// Units display (. . .  N/m^2)
  AXIS_GRID      = 0x0008,      /// Grid lines drawn on major ticks
  AXIS_TICKS     = 0x0010,      /// Grid lines drawn on major ticks
  AXIS_EXPONENT  = 0x0020,      /// Exponent near end of axis ( . . .  x 10^5 N/m^2)
  AXIS_REVERSED  = 0x0040,      /// Numbers increase to left
  AXIS_ROUND     = 0x0080,      /// Round range to nearest nice number
  AXIS_LOG       = 0x0100,      /// Logarithmic scale
  AXIS_GRIDFRONT = 0x0200       /// Grid in front of data
  };


/// Axis definition
struct Axis {
  FXuint      style;            /// Axis style flags
  Ticks       ticks;            /// Tick drawing style
  Grid        grid;             /// Grid settings
  Caption     label;            /// Axis caption
  Caption     units;            /// Axis units
  Numbers     numbers;          /// Number drawing info
  FXColor     linecolor;        /// Line color of axis line
  FXuchar     lineweight;       /// Line weight of axis line
  FXdouble    minimum;          /// Maximum data value
  FXdouble    maximum;          /// Minimum data value
  };


class FXImage;


// Base class for the various chart widgets
class FXCHARTAPI FXChart : public FXComposite {
  FXDECLARE(FXChart)
protected:
  FXImage  *chart;      // Chart image
  FXString  tip;        // Tooltip value
  FXString  help;       // Help value
  FillStyle fill;       // Fill style
protected:
  FXChart();
  void drawMarker(FXDC& dc,FXint x,FXint y,const Marker& m) const;
private:
  FXChart(const FXChart&);
  FXChart &operator=(const FXChart&);
public:
  long onPaint(FXObject*,FXSelector,void*);
  long onQueryHelp(FXObject*,FXSelector,void*);
  long onQueryTip(FXObject*,FXSelector,void*);
  long onClipboardLost(FXObject*,FXSelector,void*);
  long onClipboardRequest(FXObject*,FXSelector,void*);
public:
  static FXDragType bmpType;
  static FXDragType gifType;
  static FXDragType jpgType;
  static FXDragType pngType;
  static FXDragType tifType;
  static FXDragType csvType;
public:

  /// Construct color well with initial color clr
  FXChart(FXComposite* p,FXObject* tgt=NULL,FXSelector sel=0,FXuint opts=FRAME_NORMAL,FXint x=0,FXint y=0,FXint w=0,FXint h=0);

  /// Create server-side resources
  virtual void create();

  /// Detach server-side resources
  virtual void detach();

  /// Perform layout
  virtual void layout();

  /// Set fill style
  void setFillStyle(const FillStyle& fs);

  /// Get fill style
  FillStyle getFillStyle() const { return fill; }

  /// Set status line help text for this chart
  void setHelpText(const FXString& text);

  /// Get status line help text for this chart
  FXString getHelpText() const { return help; }

  /// Set tool tip message for this chart
  void setTipText(const FXString& text);

  /// Get tool tip message for this chart
  FXString getTipText() const { return tip; }

  /// Save chart to a stream
  virtual void save(FXStream& store) const;

  /// Load chart from a stream
  virtual void load(FXStream& store);

  /// Destructor
  virtual ~FXChart();
  };

}

#endif

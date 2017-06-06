// This may look like C code, but it is really -*- C++ -*-
//
// Copyright Bob Friesenhahn, 1999
//
// Definition of Drawable (Graphic objects)
//

#if !defined(Drawable_header)
#define Drawable_header

#include <string>
#include <iostream>
#include <list>

#include <Magick++/Color.h>
#include <Magick++/Geometry.h>
#include <Magick++/Include.h>

namespace Magick
{
  //
  // Representation of an x,y coordinate
  //
  class Coordinate
  {
    friend std::ostream& operator<<( std::ostream& stream_,
				     const Coordinate& coordinate_ );
  public:
    Coordinate ( void );
    Coordinate ( double x_, double y_ );
    ~Coordinate ();

    void   x ( double x_ );
    double x ( void ) const;

    void   y ( double y_ );
    double y ( void ) const;

  private:
    double _x;
    double _y;
  };

  //
  // Representation of a drawable object
  //
  class Drawable
  {
  public:
    // Constructor
    Drawable ( void );
    
    // Destructor
    ~Drawable ( void );
    
    //
    // Drawable Objects
    //
    
    // Point
    void point ( double x_, double y_ );
    void point ( Coordinate coordinate_ );
    
    // Line
    void line ( double startX_, double startY_,
		double endX_, double endY_ );
    void line ( Coordinate startCoordinate_,
		Coordinate endCoordinate_ );
    
    // Rectangle
    void rectangle ( double upperLeftX_, double upperLeftY_,
		     double lowerRightX_, double lowerRightY );
    void rectangle ( Coordinate upperLeftCoordinate_,
		     Coordinate lowerRightCoordinate_ );
    
    // Filled Rectangle
    void fillRectangle ( double upperLeftX_, double upperLeftY_,
			 double lowerRightX_, double lowerRightY );
    void fillRectangle ( Coordinate upperLeftCoordinate_,
			 Coordinate lowerRightCoordinate_ );
    
    // Circle
    void circle ( double originX_, double originY_,
		  double perimX_, double perimY_ );
    void circle ( Coordinate originCoordinate_,
		  Coordinate perimCoordinate_ );
    
    // Filled Circle
    void fillCircle ( double originX_, double originY_,
		      double perimX_, double perimY_ );
    void fillCircle ( Coordinate originCoordinate_,
		      Coordinate perimCoordinate_ );
    
    // Ellipse
    void ellipse ( double originX_, double originY_, 
		   double width_, double height_,
		   double arcStart_, double arcEnd_ );
    void ellipse ( Coordinate originCoordinate_, 
		   double width_, double height_,
		   double arcStart_, double arcEnd_ );
    
    // Filled Ellipse
    void fillEllipse ( double originX_, double originY_, 
		       double width_, double height_,
		       double arcStart_, double arcEnd_ );
    void fillEllipse ( Coordinate originCoordinate_, 
		       double width_, double height_,
		       double arcStart_, double arcEnd_ );
    
    // Polygon (Coordinate list must contain at least three members)
    void polygon ( const std::list<Magick::Coordinate> &coordinates_ );
    
    // Filled Polygon (vectors_ is number coordinates)
    void fillPolygon ( const std::list<Magick::Coordinate> &coordinates_ );
    
    // Colorize at point using PaintMethod
    void color ( double x_, double y_,
		 PaintMethod paintMethod_ );
    void color ( Coordinate coordinate_,
		 PaintMethod paintMethod_ );
    
    // Change pixel matte value to transparent using PaintMethod
    void matte ( double x_, double y_,
		 PaintMethod paintMethod_ );
    void matte ( Coordinate coordinate_,
		 PaintMethod paintMethod_ );
    
    // Draw text at point
    void text ( double x_, double y_,
		std::string text_ );
    void text ( Coordinate coordinate_,
		std::string text_ );
    
    // Draw image at point
    void image ( double x_, double y_,
		 const std::string &image_ );
    void image ( Coordinate coordinate_,
		 const std::string &image_ );
    
    //
    // Following accessors are for internal use only
    //
    void        primitive ( std::string primitive_ );
    std::string primitive ( void ) const;
    
  private:
    
    // This is the string normally passed by -draw, e.g. "circle
    // +100+100 +200+200"
    std::string _primitive;
  };
} // namespace Magick

//
// Inlines
//

//
// Coordinate class
//
inline Magick::Coordinate::Coordinate ( void )
  : _x(0),
    _y(0)
{
}

inline Magick::Coordinate::Coordinate ( double x_, double y_ )
  : _x(x_),
    _y(y_)
{
}

inline Magick::Coordinate::~Coordinate ()
{
  // Nothing to do
}

inline void Magick::Coordinate::x ( double x_ )
{
  _x = x_;
}

inline double Magick::Coordinate::x ( void ) const
{
  return _x;
}

inline void Magick::Coordinate::y ( double y_ )
{
  _y = y_;
}

inline double Magick::Coordinate::y ( void ) const
{
  return _y;
}

//
// Drawable class
//

// Constructor
inline Magick::Drawable::Drawable( void )
  : _primitive()
{
  // All components are self-initializing
}

// Destructor
inline Magick::Drawable::~Drawable( void )
{
  // Nothing to do
}

// Completed draw primitive (for ImageMagick)
inline void Magick::Drawable::primitive ( std::string primitive_ )
{
  _primitive = primitive_;
}
inline std::string Magick::Drawable::primitive ( void ) const
{
  // cout << _primitive << endl;
  return _primitive;
}

// Point
inline void Magick::Drawable::point ( double x_,
				      double y_ )
{
  point( Coordinate( x_, y_ ) );
}

// Line
inline void Magick::Drawable::line ( double startX_,
				     double startY_,
				     double endX_,
				     double endY_ )
{
  line( Coordinate( startX_, startY_ ), Coordinate( endX_, endY_ ) );
}

// Rectangle
inline void Magick::Drawable::rectangle ( double upperLeftX_,
					  double upperLeftY_,
					  double lowerRightX_,
					  double lowerRightY )
{
  rectangle( Coordinate( upperLeftX_, upperLeftY_ ),
	     Coordinate( lowerRightX_, lowerRightY ) );
}

// Filled Rectangle
inline void Magick::Drawable::fillRectangle ( double upperLeftX_,
					      double upperLeftY_,
					      double lowerRightX_,
					      double lowerRightY )
{
  fillRectangle( Coordinate( upperLeftX_, upperLeftY_ ),
		 Coordinate( lowerRightX_, lowerRightY ) );
}

// Circle
inline void Magick::Drawable::circle ( double originX_,
				       double originY_,
				       double perimX_,
				       double perimY_ )
{
  circle( Coordinate( originX_, originY_ ),
	  Coordinate( perimX_, perimY_ ) );
}

// Filled Circle
inline void Magick::Drawable::fillCircle ( double originX_,
					   double originY_,
					   double perimX_,
					   double perimY_ )
{
  fillCircle( Coordinate( originX_, originY_ ),
	      Coordinate( perimX_, perimY_ ) );
}

// Ellipse
inline void Magick::Drawable::ellipse ( double originX_,
					double originY_, 
					double width_,
					double height_,
					double arcStart_,
					double arcEnd_ )
{
  ellipse( Coordinate( originX_, originY_ ),
	   width_, height_,
	   arcStart_, arcEnd_ );
}

// Filled Ellipse
inline void Magick::Drawable::fillEllipse ( double originX_,
					    double originY_, 
					    double width_,
					    double height_,
					    double arcStart_,
					    double arcEnd_ )
{
  fillEllipse( Coordinate( originX_, originY_ ),
	       width_, height_,
	       arcStart_, arcEnd_ );
}

// Colorize at point using PaintMethod
inline void Magick::Drawable::color ( double x_, double y_,
				      Magick::PaintMethod paintMethod_ )
{
  color( Coordinate( x_, y_ ), paintMethod_ );
}

// Change pixel matte value to transparent using PaintMethod
inline void Magick::Drawable::matte ( double x_, double y_,
				      Magick::PaintMethod paintMethod_ )
{
  matte( Coordinate( x_, y_ ), paintMethod_ );
}

// Draw text at point
inline void Magick::Drawable::text ( double x_,
				     double y_,
				     std::string text_ )
{
  text( Coordinate( x_, y_), text_ );
}

// Draw image at point
inline void Magick::Drawable::image ( double x_,
				      double y_,
				      const std::string &image_ )
{
  image( Coordinate( x_, y_), image_ );
}


#endif // Drawable_header

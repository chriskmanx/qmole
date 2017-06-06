// This may look like C code, but it is really -*- C++ -*-
//
// Copyright Bob Friesenhahn, 1999
//
// Implementation of Drawable (Graphic objects)
//

#define MAGICK_IMPLEMENTATION

#include <string>
#include <cstdarg>
#include <iostream>
#include <strstream>

using namespace std;

#include <Magick++/Drawable.h>


std::ostream& Magick::operator<<( std::ostream& stream_, const Magick::Coordinate& coordinate_)
{
  stream_ << coordinate_._x << "," << coordinate_._y;
  return stream_;
}

//
// Drawable implementation
//


//
// Drawable Objects
//

// Point
void Magick::Drawable::point ( Magick::Coordinate coordinate_ )
{
  char buffer[MaxTextExtent + 1];
  ostrstream buffstr( buffer, sizeof(buffer));
  buffstr << "point " << coordinate_ << ends;
  _primitive.assign(buffer);
}

// Line
void Magick::Drawable::line ( Magick::Coordinate startCoordinate_,
			      Magick::Coordinate endCoordinate_ )
{
  char buffer[MaxTextExtent + 1];
  ostrstream buffstr( buffer, sizeof(buffer));
  buffstr << "line " << startCoordinate_ << " " << endCoordinate_ << ends;
  _primitive.assign( buffer );
}

// Rectangle
void Magick::Drawable::rectangle ( Magick::Coordinate upperLeftCoordinate_,
				   Magick::Coordinate lowerRightCoordinate_ )
{
  char buffer[MaxTextExtent + 1];
  ostrstream buffstr( buffer, sizeof(buffer));
  buffstr << "rectangle " << upperLeftCoordinate_ << " " << lowerRightCoordinate_ << ends;
  _primitive.assign( buffer );
}

// Filled Rectangle
void Magick::Drawable::fillRectangle ( Magick::Coordinate upperLeftCoordinate_,
				       Magick::Coordinate lowerRightCoordinate_ )
{
  char buffer[MaxTextExtent + 1];
  ostrstream buffstr( buffer, sizeof(buffer));
  buffstr << "fillRectangle " << upperLeftCoordinate_ << " " << lowerRightCoordinate_ << ends;
  _primitive.assign( buffer );
}

// Circle
void Magick::Drawable::circle ( Magick::Coordinate originCoordinate_,
				Magick::Coordinate perimCoordinate_ )
{
  char buffer[MaxTextExtent + 1];
  ostrstream buffstr( buffer, sizeof(buffer));
  buffstr << "circle " << originCoordinate_ << " " << perimCoordinate_ << ends;
  _primitive.assign( buffer );
}

// Filled Circle
void Magick::Drawable::fillCircle ( Magick::Coordinate originCoordinate_,
				    Magick::Coordinate perimCoordinate_ )
{
  char buffer[MaxTextExtent + 1];
  ostrstream buffstr( buffer, sizeof(buffer));
  buffstr << "fillCircle " << originCoordinate_ << " " << perimCoordinate_ << ends;
  _primitive.assign( buffer );
}

// Ellipse
void Magick::Drawable::ellipse( Magick::Coordinate originCoordinate_, 
				double width_, double height_,
				double arcStart_, double arcEnd_ )
{
  char buffer[MaxTextExtent + 1];
  ostrstream buffstr( buffer, sizeof(buffer));
  buffstr << "ellipse " << originCoordinate_
	  << " " << width_ << "," << height_
	  << " " << arcStart_ << "," << arcEnd_
	  << ends;
  _primitive.assign( buffer );
}

// Filled Ellipse
void Magick::Drawable::fillEllipse( Magick::Coordinate originCoordinate_, 
				    double width_, double height_,
				    double arcStart_, double arcEnd_ )
{
  char buffer[MaxTextExtent + 1];
  ostrstream buffstr( buffer, sizeof(buffer));
  buffstr << "fillEllipse " << originCoordinate_
	  << " " << width_ << "," << height_
	  << " " << arcStart_ << "," << arcEnd_
	  << ends;
  _primitive.assign( buffer );
}

// Polygon (Coordinate list must contain at least three members)
void Magick::Drawable::polygon ( const std::list<Magick::Coordinate> &coordinates_ )
{
  char buffer[MaxTextExtent + 1];
  ostrstream buffstr( buffer, sizeof(buffer));

  buffstr << "polygon";

  list<Magick::Coordinate>::const_iterator p = coordinates_.begin();

  while ( p != coordinates_.end() )
    {
      buffstr << " "
	      << *p;
      p++;
    }

  buffstr << ends;
  _primitive.assign( buffer );
}

// Filled Polygon (Coordinate list must contain at least three members)
void Magick::Drawable::fillPolygon ( const std::list<Magick::Coordinate> &coordinates_ )
{
  char buffer[MaxTextExtent + 1];
  ostrstream buffstr( buffer, sizeof(buffer));

  buffstr << "fillPolygon";

  std::list<Magick::Coordinate>::const_iterator p = coordinates_.begin();

  while ( p != coordinates_.end() )
    {
      buffstr << " "
	      << *p;
      p++;
    }

  buffstr << ends;
  _primitive.assign( buffer );
}

// Colorize at point using PaintMethod
void Magick::Drawable::color( Magick::Coordinate coordinate_,
			      Magick::PaintMethod paintMethod_ )
{
  char buffer[MaxTextExtent + 1];
  ostrstream buffstr( buffer, sizeof(buffer));
  
  buffstr << "color " << coordinate_ << " ";
  
  switch ( paintMethod_ )
    {
    case PointMethod :
      buffstr << "point";
      break;
    case ReplaceMethod :
      buffstr << "replace";
      break;
    case FloodfillMethod :
      buffstr << "floodfill";
      break;
    case FillToBorderMethod :
      buffstr << "filltoborder";
      break;
    case ResetMethod :
      buffstr << "reset";
      break;
    default :
      {
	buffstr << "point";
      }
    }
  buffstr << ends;

  _primitive.assign(buffer);
}

// Change pixel matte value to transparent using PaintMethod
void Magick::Drawable::matte( Magick::Coordinate coordinate_,
			      Magick::PaintMethod paintMethod_ )
{
  char buffer[MaxTextExtent + 1];
  ostrstream buffstr( buffer, sizeof(buffer));
  
  buffstr << "matte " << coordinate_ << " ";
  
  switch ( paintMethod_ )
    {
    case PointMethod :
      buffstr << "point";
      break;
    case ReplaceMethod :
      buffstr << "replace";
      break;
    case FloodfillMethod :
      buffstr << "floodfill";
      break;
    case FillToBorderMethod :
      buffstr << "filltoborder";
      break;
    case ResetMethod :
      buffstr << "reset";
      break;
    default :
      {
	buffstr << "point";
      }
    }
  buffstr << ends;

  _primitive.assign(buffer);
}

// Draw text at point
void Magick::Drawable::text ( Magick::Coordinate coordinate_,
			      std::string text_ )
{
  char buffer[MaxTextExtent + 1];
  ostrstream buffstr( buffer, sizeof(buffer));
  buffstr << "text " << coordinate_ << text_ << ends;
  _primitive.assign( buffer );
}

// Draw image at point
void Magick::Drawable::image ( Magick::Coordinate coordinate_,
			       const std::string &image_ )
{
  char buffer[MaxTextExtent + 1];
  ostrstream buffstr( buffer, sizeof(buffer));
  buffstr << "image " << coordinate_ << image_ << ends;
  _primitive.assign( buffer );
}

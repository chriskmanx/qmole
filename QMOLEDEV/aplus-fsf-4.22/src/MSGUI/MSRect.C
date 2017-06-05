///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSRect.H>

MSRect::MSRect(void)
{ _x=_y=0;_width=_height=1; }
MSRect::MSRect(const MSRect& r_)
{ _x=r_.x();_y=r_.y();_width=r_.width();_height=r_.height(); }
MSRect::MSRect(int x_,int y_,int w_,int h_)
{ _x=x_;_y=y_;_width=w_; _height=h_; }

MSRect::~MSRect(void)
{_x=0;_y=0;_width=0;_height=0;} 

MSRect& MSRect::operator=(const MSRect& r_)
{ 
  if (this!=&r_)
   {
     _x=r_.x();_y=r_.y();_width=r_.width();_height=r_.height(); 
   }
  return *this;
}

int MSRect::x(void)      const { return _x; }
int MSRect::y(void)      const { return _y; }
int MSRect::width(void)  const { return _width; }
int MSRect::height(void) const { return _height; }

void MSRect::x(int x_)      { _x=x_; }
void MSRect::y(int y_)      { _y=y_; }
void MSRect::width(int w_)  { _width=w_; }
void MSRect::height(int h_) { _height=h_; }

void MSRect::configuration(int x_,int y_,int w_,int h_) 
{ _x=x_;_y=y_;_width =w_;_height=h_; }



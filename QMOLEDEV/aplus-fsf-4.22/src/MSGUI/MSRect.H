#ifndef MSRectHEADER
#define MSRectHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>

class MSGUIExport MSRect 
{
protected:
  int _x;
  int _y;
  int _width;
  int _height;

public:
  MSRect(void);
  MSRect(const MSRect&);
  MSRect(int x_,int y_,int w_,int h_);
  virtual ~MSRect(void);

  int x(void)      const;
  int y(void)      const;
  int width(void)  const;
  int height(void) const;

  void x(int x_);
  void y(int y_);
  void width(int w_);
  void height(int h_);

  void configuration(int x_,int y_,int w_,int h_); 

  MSRect& operator=(const MSRect&);
};

#endif






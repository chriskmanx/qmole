#ifndef MSArrowHEADER
#define MSArrowHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSWidgetCommon.H>

class MSGUIExport MSArrow : public MSRect
{
public:
  enum ArrowType {Left,Right,Up,Down};

  MSArrow(MSWidgetCommon *);
  MSArrow(MSWidgetCommon *,ArrowType);
  ~MSArrow(void);

  void owner(MSWidgetCommon *);
  void type(ArrowType);
  void draw(void);
  void resize(void);
  void resize(int w_,int h_); 
  void configure(int x_,int y_,int w_,int h_); 

  void moveTo(int x_,int y_);
  void y_origin(int);
  void x_origin(int);
  void select(MSBoolean);

  MSBoolean selected(void) const;
  ArrowType arrowType(void) const;
  MSWidgetCommon *owner(void) const;
  int topCount(void) const;
  int centerCount(void) const;
  int bottomCount(void) const;

  void color(unsigned long);
  void color(const char *);
  unsigned long color(void) const;
  
protected:
  MSWidgetCommon *_owner;
  ArrowType       _arrowType;
  MSBoolean       _selected;
  int             _topCount;
  int             _centerCount;
  int             _bottomCount;
  XRectangle     *_top;
  XRectangle     *_center;
  XRectangle     *_bottom;
  MSShadow       *_pShadow;

  MSShadow& shadow(void);
  const MSShadow& shadow(void) const;
  void init(void);
  void computeSize(void);
  void computeArrowRects(void);
  void offsetArrow(int,int);

  Display *display(void) const;
  Window window(void) const;
};

inline void MSArrow::select(MSBoolean s)
{ _selected=s; draw(); }
inline MSBoolean MSArrow::selected(void) const
{ return _selected; }
inline MSArrow::ArrowType MSArrow::arrowType(void) const
{ return _arrowType; }
inline int MSArrow::topCount(void) const
{ return _topCount; }
inline int MSArrow::centerCount(void) const
{ return _centerCount; }
inline int MSArrow::bottomCount(void) const
{ return _bottomCount; }

inline void MSArrow::resize(void)
{ computeSize(); }

inline MSWidgetCommon *MSArrow::owner(void) const
{ return _owner; }
inline Display *MSArrow::display(void) const
{ return owner()->display(); }
inline Window MSArrow::window(void) const
{ return owner()->window(); }

#endif





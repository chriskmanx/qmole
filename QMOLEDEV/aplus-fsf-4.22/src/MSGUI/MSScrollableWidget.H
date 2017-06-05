#ifndef MSScrollableWidgetHEADER
#define MSScrollableWidgetHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSWidgetCommon.H>
#include <MSGUI/MSHScrollBar.H>
#include <MSGUI/MSVScrollBar.H>

class MSGUIExport MSScrollableWidget : public MSWidgetCommon
{
public:
  MSScrollableWidget(MSWidget *owner_);
  ~MSScrollableWidget(void);

  virtual void show(void);
  virtual void redraw(void);
  virtual void print(const char *file_=0);
  virtual void naturalSize(void);

protected:
  class MSGUIExport Vsb : public MSVScrollBar
  {
  protected:
    virtual void drag(void);
    virtual void change(void);
    void scroll(void);
    MSScrollableWidget *scrollableWidget(void) const { return (MSScrollableWidget *)owner(); }
  public:
    Vsb(MSWidget *);
    ~Vsb(void);
  };

  class MSGUIExport Hsb : public MSHScrollBar
  {
  protected:
    virtual void drag(void);
    virtual void change(void);
    void scroll(void);
    MSScrollableWidget *scrollableWidget(void) const { return (MSScrollableWidget *)owner(); }
  public:
    Hsb(MSWidget *);
    ~Hsb(void);
  };
  class MSGUIExport ViewPort : public MSWidgetCommon
  {
  protected:
    friend class MSScrollableWidget;
    MSScrollableWidget *scrollableWidget(void) const { return (MSScrollableWidget *)owner(); }
  public:
    ViewPort(MSWidget *);
    void redraw(void);
  };
  class MSGUIExport DrawAreaWidget : public MSWidgetCommon
  {
  protected:
    friend class MSScrollableWidget;
    MSScrollableWidget *scrollableWidget(void) const { return (MSScrollableWidget *)(owner()->owner()); }
  public:
    DrawAreaWidget(MSWidget *);
    virtual void redraw(void);
  };
  
friend class Vsb;
friend class Hsb;
friend class ViewPort;
friend class DrawAreaWidget;

  Vsb             *_vsb;
  Hsb             *_hsb;
  ViewPort        *_viewPort;
  DrawAreaWidget  *_drawAreaWidget;
  int              _naturalWidth;
  int              _naturalHeight;
  
  Hsb *hsb(void) {return _hsb;}
  Vsb *vsb(void) {return _vsb;}
  MSWidgetCommon *viewPort(void) {return _viewPort;}
  DrawAreaWidget *drawAreaWidget(void) {return _drawAreaWidget;}

  int naturalWidth(void) {return _naturalWidth;}
  void naturalWidth(int naturalWidth_) {_naturalWidth=naturalWidth_;}
  
  int naturalHeight(void) {return _naturalHeight;}
  void naturalHeight(int naturalHeight_) {_naturalHeight=naturalHeight_;}

  void init(void);

  virtual void redrawScrollableWidget(void);
  virtual void redrawScrollableWidgetImmediately(void);

  virtual void firstMapNotify(void);
  virtual void computeSize(void);
  virtual void configure(void);

  virtual void expose(const XEvent *);
  virtual void focusIn(void);
  virtual void focusOut(void);
  virtual void updateBackground(unsigned long);

  // Convenient methods to map (x,y) coordinates from the Scrollable Widget
  // to the Draw Area and the View Port.
  void drawAreaXY(int &x_,int &y_);
  void viewPortXY(int &x_,int &y_);

  // Can be overriden by subclasses to get notification when
  // scrollbar is changed by the user, default implementation
  // moves the draw area accordingly
  virtual void vsbChange(void);
  virtual void hsbChange(void);
  
  virtual void drawAreaGeometry(int &x_,int &y_,int &w_,int &h_);
  virtual void moveDrawArea(int,int);
  virtual void resizeDrawArea(int,int);
  virtual void resizeViewPort(int,int);

  // To be provided by subclasses
  virtual void redrawDrawAreaImmediately(void);
  virtual void redrawDrawArea(void);

  // To be provided by subclasses
  virtual void redrawViewPortImmediately(void);
  virtual void redrawViewPort(void);
};

#endif

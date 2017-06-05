#ifndef MSScrolledWindowHEADER
#define MSScrolledWindowHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSManager.H>
#include <MSGUI/MSPrimitive.H>
#include <MSGUI/MSVScrollBar.H>
#include <MSGUI/MSHScrollBar.H>

class MSGUIExport MSScrolledWindow : public MSManager
{
  class MSGUIExport Vsb : public MSVScrollBar
  {
  protected:
    virtual void drag(void);
    virtual void change(void);
    MSScrolledWindow *scrolledWindow(void) const { return (MSScrolledWindow *)owner(); }
  public:
    Vsb(MSWidget *);
    ~Vsb(void);
  };

  class MSGUIExport Hsb : public MSHScrollBar
  {
  protected:
    virtual void drag(void);
    virtual void change(void);
    MSScrolledWindow *scrolledWindow(void) const { return (MSScrolledWindow *)owner(); }
  public:
    Hsb(MSWidget *);
    ~Hsb(void);
  };

friend class Vsb;
friend class Hsb;

public:
  enum ScrollBarDisplayPolicy{Static,AsNeeded};
  
  MSScrolledWindow(MSWidget *);
  ~MSScrolledWindow(void);

  void scrollBarDisplayPolicy(ScrollBarDisplayPolicy);
  ScrollBarDisplayPolicy scrollBarDisplayPolicy(void) const;
  
  MSWidget *scrollWindow(void);
  const MSWidget *scrollWindow(void) const;

  MSScrollBar *vsb(void) const;
  MSScrollBar *hsb(void) const;

  int viewWidth(void) const;
  int viewHeight(void) const;
  int scrollBarSize(void) const;

  void viewHeight(int);
  void viewWidth(int);
  void scrollBarSize(int);
  void vsbBackground(unsigned long pixel_);
  void hsbBackground(unsigned long pixel_);
  void vsbBackground(const char *color_);
  void hsbBackground(const char *color_);

  virtual void show(void);
  virtual void naturalSize(void);
  virtual void print(const char *file_=0);
  
  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);

protected:
  Vsb                     *_vsb;
  Hsb                     *_hsb;
  MSPrimitive             *_clipWindow;
  MSWidget                *_scrollWindow;
  int                      _viewHeight;
  int                      _viewWidth;
  ScrollBarDisplayPolicy   _scrollBarDisplayPolicy;
  
  MSPrimitive *clipWindow(void) const;

  void init(void);
  virtual void computeSize(void);
  virtual void placement(void);

  virtual void pageUp(void);
  virtual void pageDown(void);
  virtual void home(void);
  virtual void end(void);

  virtual void focusIn(void);
  virtual void focusOut(void);
  virtual void childConfigure(MSWidget *);
  virtual void childCreate(MSWidget *);
  virtual void childDestroy(MSWidget *);
  virtual void childInsert(MSWidget *);
  virtual void childRemove(MSWidget *);
  virtual void visibilityObscured(void);
  virtual void visibilityUnobscured(void);

  virtual MSWidgetVector children(void);

  virtual void updateBackground(unsigned long);
};

inline MSPrimitive *MSScrolledWindow::clipWindow(void) const   
{ return _clipWindow; }
inline MSScrollBar *MSScrolledWindow::vsb(void) const
{ return _vsb; }
inline MSScrollBar *MSScrolledWindow::hsb(void) const  
{ return _hsb; }

inline MSWidget *MSScrolledWindow::scrollWindow(void)
{ return _scrollWindow; }
inline const MSWidget *MSScrolledWindow::scrollWindow(void) const
{ return _scrollWindow; }

inline int MSScrolledWindow::viewWidth(void) const 
{ return _viewWidth; }
inline int MSScrolledWindow::viewHeight(void) const      
{ return _viewHeight; }
inline int MSScrolledWindow::scrollBarSize(void) const     
{ return _vsb->width(); }

inline void MSScrolledWindow::vsbBackground(unsigned long pixel_) 
{ _vsb->background(pixel_); }
inline void MSScrolledWindow::hsbBackground(unsigned long pixel_) 
{ _hsb->background(pixel_); }

inline void MSScrolledWindow::vsbBackground(const char *color_) 
{ _vsb->background(color_); }
inline void MSScrolledWindow::hsbBackground(const char *color_) 
{ _hsb->background(color_); }

inline MSScrolledWindow::ScrollBarDisplayPolicy MSScrolledWindow::scrollBarDisplayPolicy(void) const
{ return _scrollBarDisplayPolicy; }

#endif








#ifndef AplusPageHEADER
#define AplusPageHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

// AplusPage class needs to be rewritten to use MStk's MSPage widget.
//

#include <MSGUI/MSPage.H>
#include "Macros.H"
#include "AplusEvent.H"
#include "AplusCommon.H"

class APageFunction : public AFunction
{
public:
  APageFunction(void) { _func=0; _arg=0; }
  APageFunction(AFunc func_,AClientData *arg_=0) : AFunction(func_,arg_) {}
  ~APageFunction(void) {}
  
  A grc(int row_,int col_)
   { A a=gv(Et,2);
     a->p[0]=(row_==-1)?(I)aplus_nl:(I)gi(row_);
     a->p[1]=(col_==-1)?(I)aplus_nl:(I)gi(col_); 
     return a;
   }
  int callFunc(V v_,A a_,int row_,int col_)
   { int ret=0;
     A i;
     if (row_==-1 && col_==-1) i=0;
     else i=grc(row_,col_);
     if (func()!=0) 
      {
        A r=(A)(*func())(arg(),a_,i,aplus_nl,v_);
        if (r!=0 && QA(r) && r->t==It) ret=(int)r->p[0];        
        if (QA(r)) dc((A)r); 
      }
     if (i!=0) dc((A)i);
     if (a_!=0 && isNull(a_)==MSFalse) dc((A)a_);
     return ret; 
   }
  int invoke(V v_,char value_,int row_,int col_) 
   { A str=(A)gs(Ct); *str->p=(I)value_;
     return callFunc(v_,str,row_,col_);
   } 
  int invoke(V v_,int row_,int col_) 
   { return callFunc(v_,aplus_nl,row_,col_); } 
  A invoke(V v_,A index_) 
   { A r=aplus_nl;
     if (func()!=0) r=(A)(*func())(arg(),aplus_nl,index_,aplus_nl,v_);
     return (r!=0 && QA(r) && r->t==It)?r:aplus_nl;
   }
};


class AplusPage;
class AplusBlinkTimer : public MSIntervalTimer
{
private:
  AplusPage *_page;
  AplusPage *page(void) { return _page; }
public:
  AplusBlinkTimer(AplusPage *page_,unsigned long interval_);
  ~AplusBlinkTimer();
  virtual void process(void);
};


class AplusPage : public MSCompositeText
{
friend class AplusBlinkTimer;
public:
  enum BlinkPhase{Normal,Reverse};

protected:
  int                _x_cursor;
  int                _y_cursor;
  int                _margin;
  MSNodeItem         _colorListHead;
  AplusBlinkTimer   *_blinkTimer;
  BlinkPhase         _blinkPhase;
  A                  _blink;
  unsigned long      _blinkRate;
  Font               _boldFontID;
  APageFunction      _indexFunc;
  APageFunction      _boldFunc;
  APageFunction      _underlineFunc;
  APageFunction      _blinkFunc;
  RubberBand         _rubberBand;
  int                _lineWidth;
  A                  _boxMatrix;
  A                  _lineMatrix;
  A                  _boxColors;
  MSNodeItem         _boxListHead;
  A                  _keyBuf;

  MSNodeItem *colorListHead(void)  { return _colorListHead.address(); }
  MSNodeItem *boxListHead(void)    { return _boxListHead.address(); }
  AplusBlinkTimer *blinkTimer(void)     { return _blinkTimer; }
  A blink(void)                    { return _blink; }
  BlinkPhase blinkPhase(void)      { return _blinkPhase; }
  Font boldFontID(void)            { return _boldFontID; }
  RubberBand *rubberBand(void)     { return &_rubberBand; }
  A boxMatrix(void)                { return _boxMatrix; }
  A lineMatrix(void)               { return _lineMatrix; }
  A boxColors(void)                { return _boxColors; }
  A keyBuffer(void)                { return _keyBuf; }
  int x_cursor(void)               { return _x_cursor; }
  int y_cursor(void)               { return _y_cursor; }
  unsigned long boxColor(int row_)
   { return (isNull(boxColors())==MSFalse && row_<boxColors()->n)?
            (unsigned long)boxColors()->p[row_]:foreground();
   }

  virtual void refresh(void);
  virtual void computeSize(void);
  virtual void screenRedraw(void);
  virtual void drawRow(int row_,int col_,const char *str_,int len_,
                       int index_,MSBoolean bold_,BlinkPhase phase_,MSBoolean underline_);
  virtual void drawAllRowsCols(A index_);
  virtual void drawAllRows(A index_);
  virtual void drawAllCols(A index_);
  virtual void drawIndexed(A index_);
  virtual void drawBoxes(MSBoolean clear_);
  virtual void drawLines(MSBoolean clear_);
  void drawRect(GC gc_,int x_,int y_,int w_,int h_)
   { XDrawRectangle(display(),window(),gc_,x_,y_,w_,h_); }

  int computeYCoord(int row_);
  int computeXCoord(int row_,int col_);
  int xToCol(int x_);
  int yToRow(int y_);
  int drawWidth(void);
  int drawHeight(void);

  ColorCell *colorCell(int index_);
  void addColor(int index_,unsigned long fg_,unsigned long bg_);

  void x_cursor(int x_)          { _x_cursor=x_; }
  void y_cursor(int x_)          { _y_cursor=x_; }
  void boldFontID(Font id_)      { _boldFontID=id_; }
  void blink(A blink_)           { dc(blink()); _blink=(A)ic(blink_); }
  void blinkPhase(BlinkPhase b_) { _blinkPhase=b_; }
  void toggleBlinkPhase(void)    { blinkPhase((blinkPhase()==Normal)?Reverse:Normal); }
  MSBoolean drawBlink(A blink_,BlinkPhase phase_);
  MSBoolean verifyBlink(A blink_);
  MSBoolean verifyA(A a_);
  MSBoolean verifyA(A a_,A r_,A c_);
  void blinkTimeOut(void);
  void blinkUpdate(void);

  void startBlinkTimer(void);
  void stopBlinkTimer(void);
  virtual void processBlinkTimer(void);

public:
  AplusPage(MSWidget *);
  ~AplusPage();

  virtual void receiveEvent(MSEvent &);
  virtual void addSenderNotify(MSEventSender *);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

  int margin(void) { return _margin; }
  void margin(int margin_) 
   { if (margin()!=margin_) 
      { _margin=margin_; refresh(); }
   }

  virtual void redraw(void);
  virtual int numRows(void);
  virtual int numCols(void);
  char *string(int row_);

  APageFunction *indexFunc(void) { return &_indexFunc; }
  void indexFunc(AFunc func_,AClientData *arg_)
   { indexFunc()->func(func_); indexFunc()->arg(arg_); redraw(); }

  APageFunction *boldFunc(void) { return &_boldFunc; }
  void boldFunc(AFunc func_,AClientData *arg_)
   { boldFunc()->func(func_); boldFunc()->arg(arg_); redraw(); }

  APageFunction *underlineFunc(void) { return &_underlineFunc; }
  void underlineFunc(AFunc func_,AClientData *arg_)
   { underlineFunc()->func(func_); underlineFunc()->arg(arg_); redraw(); }

  APageFunction *blinkFunc(void) { return &_blinkFunc; }
  void blinkFunc(AFunc func_,AClientData *arg_)
   { blinkFunc()->func(func_); blinkFunc()->arg(arg_); redraw(); }

  unsigned long blinkRate(void)  { return _blinkRate; }
  void blinkRate(unsigned long rate_);
  int lineWidth(void) { return _lineWidth; }
  void lineWidth(int lw_) 
   { if (lw_!=lineWidth()) { _lineWidth=lw_; redraw(); }}
  void colorTable(A ct_);
  A colorTable(void);
  A rBand(void);
  A boxes(void);
  A lines(void);
  void boxes(A boxes_);
  void lines(A lines_);
  A boxColorVector(void);
  void boxColorVector(A colors_);
  A keyBuf(void);
  A cursorPosition(void) 
  {A a=gv(It,2); a->p[0]=(I)y_cursor(); a->p[1]=(I)x_cursor(); return a;}
  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char *);
  virtual void buttonPress(const XEvent *);
  virtual void buttonRelease(const XEvent *);
  virtual void configure(void);
  virtual void firstMapNotify(void);
  virtual void naturalSize(void) { computeSize(); }

  void commonUpdate(V v_,A index_,A pick_,I ravel_);
  virtual void update(V v_,int row_,int col_,UpdateType type_);
  virtual void update(V v_,A index_,A pick_,I ravel_);
  virtual void update(const MSIndexVector &v_) { MSCompositeText::update(v_); }
  virtual void screenUpdate(A index_);
  virtual MSBoolean verifyData(V v_,A a_);
  virtual void updateData(void);
  virtual void updateTitle(void) {}
  virtual void updateFont(Font);
  virtual void updateForeground(unsigned long);

  virtual void print(const char *file_=0);  

  STANDARD_WIDGET_METHODS_H
};

#endif


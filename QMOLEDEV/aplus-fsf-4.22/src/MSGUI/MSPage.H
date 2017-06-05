#ifndef MSPageHEADER
#define MSPageHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSCompositeText.H>
#include <MSTypes/MSIntVector.H>
#include <MSTypes/MSIntMatrix.H>
#include <MSTypes/MSBinaryMatrix.H>
#include <MSTypes/MSCharMatrix.H>
#include <MSTypes/MSNodeList.H>
#include <MSIPC/MSIntervalTimer.H>

class MSGUIExport ColorCell
{
protected:
  int           _id;
  unsigned long _fg;
  unsigned long _bg;

public:
  ColorCell(int id_,unsigned long fg_,unsigned long bg_) 
   { _id=id_,_fg=fg_,_bg=bg_; }  
  ~ColorCell(void) {}

  int id(void) const           { return _id; }  
  unsigned long fg(void) const { return _fg; }  
  unsigned long bg(void) const { return _bg; }  
  
  void fg(unsigned long fg_) { _fg=fg_; }
  void bg(unsigned long bg_) { _bg=bg_; }
};

class MSPage;
class MSGUIExport MSPageBlinkTimer : public MSIntervalTimer
{
private:
  MSPage *_page;
  MSPage *page(void) { return _page; }
public:
  MSPageBlinkTimer(MSPage *page_,unsigned long interval_);
  virtual void process(void);
};

class MSGUIExport RubberBand
{
public:
  int _x;
  int _y;
  int _xs;
  int _ys;

  RubberBand(void) { configure(0,0,0,0); }
  RubberBand(int x_,int y_,int xs_,int ys_) { configure(x_,y_,xs_,ys_); }

  void configure(int x_,int y_,int xs_,int ys_) { _x=x_,_y=y_,_xs=xs_,_ys=ys_; }

  int x(void)  const { return _x; }
  int y(void)  const { return _y; }
  int xs(void) const { return _xs; }
  int ys(void) const { return _ys; }
};

class MSGUIExport MSPage : public MSCompositeText
{
friend class MSPageBlinkTimer;
public:
  enum BlinkPhase{Normal,Reverse};

protected:
  int               _x_cursor;
  int               _y_cursor;
  int               _margin;
  int               _lineWidth;
  MSNodeItem        _colorListHead;
  MSNodeItem        _boxListHead;
  MSPageBlinkTimer *_blinkTimer;
  BlinkPhase        _blinkPhase;
  unsigned long     _blinkRate;
  Font              _boldFontID;
  MSIntMatrix       _colorMatrix;
  MSBinaryMatrix    _boldMatrix;
  MSBinaryMatrix    _underlineMatrix;
  MSBinaryMatrix    _blinkMatrix;
  MSBinaryMatrix    _currentBlinkMatrix;  
  MSIntMatrix       _boxMatrix;
  MSIntMatrix       _lineMatrix;
  MSIntVector       _boxColors;
  RubberBand        _rubberBandRect;

  MSNodeItem *colorListHead(void)               { return &_colorListHead; }
  MSNodeItem *boxListHead(void)                 { return &_boxListHead; }
  const MSNodeItem *colorListHead(void)   const { return &_colorListHead; }
  const MSNodeItem *boxListHead(void)     const { return &_boxListHead; }
  MSPageBlinkTimer *blinkTimer(void)      const { return _blinkTimer; }
  BlinkPhase blinkPhase(void)             const { return _blinkPhase; }
  Font boldFontID(void)                   const { return _boldFontID; }
  RubberBand& rubberBandRect(void)              { return _rubberBandRect; }
  const RubberBand& rubberBandRect(void)  const { return _rubberBandRect; }
  const MSIntVector& boxColors(void)      const { return _boxColors; }
  const MSIntMatrix& boxMatrix(void)      const { return _boxMatrix; }
  const MSIntMatrix& lineMatrix(void)     const { return _lineMatrix; }

  MSIntMatrix& colorMatrix(void)                { return _colorMatrix; }
  MSBinaryMatrix& boldMatrix(void)              { return _boldMatrix; }
  MSBinaryMatrix& underlineMatrix(void)         { return _underlineMatrix; }
  MSBinaryMatrix& blinkMatrix(void)             { return _blinkMatrix; }
  MSBinaryMatrix& currentBlinkMatrix(void)      { return _currentBlinkMatrix; }

  int x_cursor(void) const                      { return _x_cursor; }
  int y_cursor(void) const                      { return _y_cursor; }

  unsigned long boxColor(int row_) const
   { return (row_<boxColors().length())?(unsigned long)boxColors()(row_):foreground(); }

  void init(void);
  virtual void refresh(void);
  virtual void computeSize(void);
  virtual void screenRedraw(void);
  virtual void drawRow(int row_,int col_,const char *str_,int len_,
                       int index_,MSBoolean bold_,BlinkPhase phase_,MSBoolean underline_);
  virtual void drawAllRowsCols(void);
  virtual void drawAllRows(const MSIntVector&);
  virtual void drawAllCols(const MSIntVector&);
  virtual void drawIndexed(const MSIntVector&,const MSIntVector&);
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

  void x_cursor(int x_)                    { _x_cursor=x_; }
  void y_cursor(int x_)                    { _y_cursor=x_; }
  void boldFontID(Font id_)                { _boldFontID=id_; }
  void blinkPhase(BlinkPhase b_)           { _blinkPhase=b_; }
  void toggleBlinkPhase(void)              { blinkPhase((blinkPhase()==Normal)?Reverse:Normal); }

  MSBoolean drawBlink(const MSBinaryMatrix& blink_,BlinkPhase phase_);

  MSBoolean verify(const MSIntMatrix&) const;
  MSBoolean verify(const MSBinaryMatrix&) const;

  void blinkTimeOut(void);
  void blinkUpdate(void);

  void startBlinkTimer(void);
  void stopBlinkTimer(void);
  virtual void processBlinkTimer(void);

  virtual void buttonPress(const XEvent *);
  virtual void buttonRelease(const XEvent *);
  virtual void button1Press(const XEvent *);
  virtual void button1Release(const XEvent *);
  virtual void configure(void);
  virtual void firstMapNotify(void);

  virtual void rubberBand(void);

  virtual void update(const MSIndexVector&);
  virtual void updateData(void);
  virtual void screenUpdate(void);
  virtual void screenUpdate(const MSIntVector&,const MSIntVector&);
  virtual void updateFont(Font);
  virtual void updateForeground(unsigned long);
  virtual void updateBackground(unsigned long);


public:
  MSPage(MSWidget *owner_);
  MSPage(MSWidget *owner_,MSCharMatrix& model_);
  ~MSPage(void);

  void model(MSCharMatrix&);

  MSCharMatrix& matrix(void)             { return *((MSCharMatrix *)_model); }
  const MSCharMatrix& matrix(void) const { return *((MSCharMatrix *)_model); }
  int margin(void)                 const { return _margin; }
  int lineWidth(void)              const { return _lineWidth; }
  unsigned long blinkRate(void)    const { return _blinkRate; }
  const MSIntMatrix& boxes(void)   const { return _boxMatrix; }
  const MSIntMatrix& lines(void)   const { return _lineMatrix; }

  void margin(int); 
  void blinkRate(unsigned long);
  void lineWidth(int);
  void boxes(const MSIntMatrix&); 
  void lines(const MSIntMatrix&);

  void addColor(int index_,unsigned long fg_,unsigned long bg_);
  void addColor(int index_,const char *fg_,const char *bg_);

  const MSIntMatrix& colorMatrix(void)           const { return _colorMatrix; }
  const MSBinaryMatrix& boldMatrix(void)         const { return _boldMatrix; }
  const MSBinaryMatrix& underlineMatrix(void)    const { return _underlineMatrix; }
  const MSBinaryMatrix& blinkMatrix(void)        const { return _blinkMatrix; }
  const MSBinaryMatrix& currentBlinkMatrix(void) const { return _currentBlinkMatrix; }

  void colorMatrix(const MSIntMatrix& m_)           { _colorMatrix=m_; } 
  void boldMatrix(const MSBinaryMatrix& m_)         { _boldMatrix=m_; } 
  void underlineMatrix(const MSBinaryMatrix& m_)    { _underlineMatrix=m_; } 
  void blinkMatrix(const MSBinaryMatrix& m_)        { _blinkMatrix=m_; } 
  void currentBlinkMatrix(const MSBinaryMatrix& m_) { _currentBlinkMatrix=m_; } 

  virtual void naturalSize(void);
  virtual void print(const char *file_=0);  
  virtual void redraw(void);
  virtual void unfreeze(void);
};

#endif













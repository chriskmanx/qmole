#ifndef MSTextHEADER
#define MSTextHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSCompositeText.H>
#include <MSGUI/MSLabel.H>
#include <MSGUI/MSVScrollBar.H>
#include <MSGUI/MSPrimitive.H>
#include <MSIPC/MSIntervalTimer.H>

class MSText;
class MSGC;
class MSPixmap;

class MSGUIExport MSText : public MSCompositeText
{
public:  
  class MSGUIExport Panner : public MSPrimitive
  {
  friend class MSText;
  protected:
    virtual void expose(const XEvent *);
  public:
    Panner(MSWidget *);
    ~Panner(void);
  };

  class MSGUIExport Vsb : public MSVScrollBar
  {
  protected:
    virtual void drag(void);
    virtual void change(void);
  public:
    Vsb(MSWidget *);
    ~Vsb(void);
  };

  class MSGUIExport CursorTimer : public MSIntervalTimer
  {
  protected:
    MSText *_text;
  public:
    CursorTimer(MSText *,unsigned long);
    ~CursorTimer(void);
    virtual void process(void);
  };

  class MSGUIExport ScrollTimer : public MSIntervalTimer
  {
  public:
    enum Direction{Up,Down};
    ScrollTimer(MSText *,Direction direction_=Down);
    void direction(Direction direction_) {_direction=direction_;}
    Direction direction(void) {return _direction;}
    virtual void process(void);

  private:
    Direction  _direction;
    MSText    *_text;

    MSText *text(void) {return _text;}
  };

friend class Panner;
friend class Vsb;
friend class CursorTimer;
friend class ScrollTimer;

public:
  MSText(MSWidget *,const char *title_=0);
  MSText(MSWidget *,const MSStringVector&);
  ~MSText(void);

  void model(MSString&);
  void model(const MSString&);

  MSString& viewModel(void)             { return *((MSString*)_model); }
  const MSString& viewModel(void) const { return *((MSString*)_model); }
  
  virtual void redraw(void);
  virtual void refresh(void);
  virtual void print(const char *file_=0);
  virtual void naturalSize(void);
  virtual void save(void);

  const char *string(void) const;
  unsigned length(void) const;
  unsigned maxEditLength(void) const;
  unsigned rows(void) const;
  unsigned columns(void) const;
  unsigned cursorPosition(void) const;
  unsigned firstLine(void) const;
  MSString selectedString(void) const;

  void string(const char *string_);
  void cursorPosition(unsigned position_);
  void firstLine(int firstLine_);
  void maxEditLength(unsigned);
  void rows(unsigned rows_);
  void columns(unsigned columns_);

  // key actions methods
  virtual void kill(void);
  virtual void yank(void);
  virtual void returnKey(void);
  virtual void escape(void);
  virtual void forwardChar(void);
  virtual void backwardChar(void);
  virtual void home(void);
  virtual void end(void);
  virtual void pageUp(void);
  virtual void pageDown(void); 
  virtual void startOfLine(void);
  virtual void endOfLine(void);
  virtual void deleteChar(void);
  virtual void backspace(void);
  virtual void tab(void);
  virtual void shiftTab(void);
  virtual void up(void); 
  virtual void down(void);
  virtual void insertMode(void);

  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);

protected:
  class MSGUIExport Line
  {
  protected:
    unsigned  _start;
    unsigned  _end;
    MSBoolean _dirty;

  public:
    Line(void) { _start=0,_end=0,_dirty=MSTrue; }
    ~Line(void) {}

    void start(unsigned start_) { if (start_!=_start) {_start=start_;}}
    void end(unsigned end_)     { if (end_!=_end) {_end=end_;}}

    void dirty(MSBoolean dirty_) { _dirty=dirty_; }
    void clean(void) { _dirty=MSFalse; }

    unsigned start(void) const  { return _start; }
    unsigned end(void) const    { return _end; }
    MSBoolean dirty(void) const { return _dirty; }
  };

protected:
  MSString        _text;
  MSString        _yankBuffer;
  Vsb            *_vsb;
  Panner         *_panner;
  MSLabel        *_label;
  MSPixmap       *_cursorPixmap;
  MSGC           *_imageMSGC;
  CursorTimer    *_blinkTimer;
  Line          **_lines;
  unsigned        _firstLine;
  unsigned        _cursorPosition;
  unsigned        _cursorX;
  unsigned        _cursorY;
  unsigned        _rows;
  unsigned        _columns;
  unsigned        _naturalRows;
  unsigned        _naturalCols;
  unsigned        _numLines;
  unsigned        _maxNumLines;
  unsigned        _maxEditLength;
  unsigned        _selectionStart;
  unsigned        _selectionLength;
  MSBoolean       _blinkOn;
  MSBoolean       _haveFocus;
  MSBoolean       _cursorOn;
  int             _startDrag;
  ScrollTimer    *_scrollTimer;

  MSPixmap *cursorPixmap(void) const   { return _cursorPixmap; }
  MSGC *imageMSGC(void)const           { return _imageMSGC; }
  GC imageGC(void) const;
  Vsb *vsb(void) const                 { return _vsb; }
  Panner *panner(void) const           { return _panner; }
  MSLabel *label(void) const           { return _label; }
  CursorTimer *blinkTimer(void) const  { return _blinkTimer; }
  Line *line(unsigned line_) const     { return _lines[line_]; }
  Line **lines(void) const             { return _lines; }
  MSBoolean blinkOn(void) const        { return _blinkOn; }
  MSBoolean haveFocus(void) const      { return _haveFocus; }
  MSBoolean cursorOn(void) const       { return _cursorOn; }
  unsigned selectionStart(void) const  { return _selectionStart; }
  unsigned selectionLength(void) const { return _selectionLength; }
  unsigned naturalRows(void) const     { return _naturalRows; }
  unsigned naturalCols(void) const     { return _naturalCols; }
  ScrollTimer *scrollTimer(void)       { return _scrollTimer; }
  int startDrag(void) const            { return _startDrag; }
  int selectionEnd(void) const         { return _selectionStart+_selectionLength-1; }

  void init(void);
  
  void selectionStart(unsigned start_) { _selectionStart=start_; }
  void selectionLength(unsigned len_)  { _selectionLength=len_; }

  MSBoolean primaryIsOurs(void) const;
  void startBlinking(void);
  void stopBlinking(void);

  void initLines(unsigned n_);
  void resetLines(void) { resetLinesFrom(0); }
  void resetLinesFrom(unsigned line_);
  void resetLinesAfterInsert(unsigned line_,unsigned cp_,unsigned slen_,MSBoolean insert_);
  void resetLinePosition(unsigned line_,unsigned pos_);
  void makeIBeamCursor(void);
  void createGCs(void);
  void moveCursorY(int);
  void moveCursorX(int);
  void moveCursor(int,int);
  virtual void scrollUp(unsigned n_);
  virtual void scrollDown(unsigned n_);

  virtual void positionLabel(void);
  virtual void placement(void);
  virtual void computeSize(void);
  virtual void drawTextFrom(unsigned pos_);
  virtual void drawLines(unsigned sl_,unsigned el_);
  virtual void drawCursor(void);
  virtual void clearCursor(void);
  virtual unsigned computeCursorPosition(void);
  virtual unsigned computeNumLines(unsigned start_,unsigned end_);
  virtual unsigned computeMaxNumLines(void);
  unsigned computeEndPosition(unsigned pos_);
  unsigned lineToPosition(unsigned line_);
  unsigned positionToRow(unsigned pos_);
  unsigned positionToCol(unsigned pos_);
  void positionToRowCol(unsigned pos_,unsigned& r_,unsigned& c_);
  void positionToXY(unsigned pos_,unsigned& x_,unsigned& y_);
  unsigned computeCursorX(unsigned,unsigned);
  unsigned lineLength(unsigned row_);
  unsigned yToRow(unsigned);
  unsigned computeX(unsigned,unsigned);
  unsigned lastLine(void);
  MSBoolean inRange(unsigned pos_);
  void selectRange(unsigned selectionStart_,unsigned selectionLength_);
  void clearSelection(void);

  void vsbValueUpdate(void);
  void updateVsb(void);
  void resetVsb(void);

  void insertString(const char *pString_); 
  void deleteString(int start_,unsigned nunChars_);

  MSString& text(void);
  const MSString& text(void) const;
  MSString& yankBuffer(void);
  unsigned numLines(void);
  unsigned maxNumLines(void);
  unsigned cursorX(void);
  unsigned cursorY(void);
  unsigned drawHeight(void);
  unsigned drawWidth(void);

  void numLines(unsigned); 
  void cursorX(unsigned xpos_);
  void cursorY(unsigned ypos_);

  virtual void updateData(void);
  void processCursorTimer(void);
  void lineStatus(void);
  void debugMode(MSBoolean);

  virtual void focusIn(void);
  virtual void focusOut(void);
  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char *);
  virtual void buttonPress(const XEvent *);
  virtual void buttonRelease(const XEvent *);
  virtual void motionNotify(const XEvent *);

  virtual const char* getPrimarySelection(MSString&, int&);
  virtual void insertPrimarySelection(void);

  virtual void selectionClear(const XEvent *);

  virtual void configure(void);
  virtual void firstMapNotify(void);

  virtual void updateBackground(unsigned long);
  virtual void updateForeground(unsigned long);
  virtual void updateFont(Font);
  virtual void updateTitle(void);
  virtual void update(const MSIndexVector&);
  void updateCursor(void);

  virtual void updateSensitivity(void);
  virtual void updateReadOnly(void);
};

inline MSString& MSText::text(void)
{ return _text; }
inline const MSString& MSText::text(void) const
{ return _text; }
inline const char *MSText::string(void) const     
{ return (const char *)text(); }
inline unsigned MSText::length(void) const 
{ return _text.length(); }

inline void MSText::maxEditLength(unsigned maxEditLength_)
{ _maxEditLength=maxEditLength_; }
inline unsigned MSText::maxEditLength(void) const
{ return _maxEditLength; }

inline unsigned MSText::rows(void) const    
{ return _rows; }
inline unsigned MSText::columns(void) const      
{ return _columns; }
inline unsigned MSText::cursorPosition(void) const 
{ return _cursorPosition; }
inline unsigned MSText::firstLine(void) const
{ return _firstLine; }

inline MSString& MSText::yankBuffer(void)  
{ return _yankBuffer; } 
inline unsigned MSText::numLines(void)
{ return _numLines; }
inline unsigned MSText::maxNumLines(void)
{ return _maxNumLines; }
inline unsigned MSText::cursorX(void)
{ return _cursorX; }
inline unsigned MSText::cursorY(void)
{ return _cursorY; }
inline unsigned MSText::drawHeight(void)
{ return _rows*textHeight(); }

#endif













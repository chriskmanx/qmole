#ifndef MSTextFieldHEADER
#define MSTextFieldHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSPrimitiveText.H>
#include <MSGUI/MSPixmap.H>
#include <MSGUI/MSDoubleClick.H>
#include <MSIPC/MSIntervalTimer.H>

class MSTextField;
class MSGC;
class MSTimer;
class MSIntervalTimer;

class MSGUIExport MSTextField : public MSPrimitiveText,public MSDoubleClick
{
  class MSGUIExport CursorTimer : public MSIntervalTimer
  {
  private:
    MSTextField *_textField;
  public:
    CursorTimer(MSTextField *,unsigned long);
    ~CursorTimer(void);
    virtual void process(void);
  };

friend class CursorTimer;

public:
  typedef const char *(*KeyTranslationFunction)(const XEvent *);
  enum EditingMode{InsertMode,OverstrikeMode};

  MSTextField(MSWidget *);
  ~MSTextField(void);

  const MSString& text(void) const;
  unsigned margin(void) const;
  unsigned xMargin(void) const;
  unsigned yMargin(void) const;
  EditingMode editMode(void) const;
  unsigned maxLength(void) const;
  unsigned cursorPosition(void) const;
  unsigned length(void) const;
  virtual const char *string(void);
  unsigned scrollIndex(void) const;

  void margin(unsigned);  // set both xMargin and yMargin to the same value
  void xMargin(unsigned);
  void yMargin(unsigned);
  void editMode(EditingMode);
  void maxLength(unsigned);
  void string(const char *);
  void cursorPosition(unsigned);
  void scrollIndex(unsigned);
  void selectRange(unsigned,unsigned);
  void selectAll(void);
  void clearSelection(void);

  static KeyTranslationFunction keyTranslationFunction(void);
  static void keyTranslationFunction(KeyTranslationFunction);
  
  void color(unsigned long,unsigned long); 

  virtual void redraw(void);
  virtual void activate(void);
  virtual void escape(void);

  // key action methods
  virtual void insertKey(void);
  virtual void returnKey(void);
  virtual void kill(void);
  virtual void yank(void);
  virtual void forwardChar(void);
  virtual void backwardChar(void);
  virtual void home(void);
  virtual void end(void);
  virtual void deleteChar(void);
  virtual void backspace(void);
  virtual void tab(void);
  virtual void shiftTab(void);
  virtual void up(void);
  virtual void down(void);

  unsigned firstCursorPosition(void) const;

  const MSString& inputMask(void) const;
  void inputMask(const MSString&);

  char inputMaskCharacter(void) const;
  void inputMaskCharacter(char);

  MSBoolean inputMaskMode(void) const; // equivalent to inputMask.length()>0

  void startEditing(EditingMode,unsigned);

private:
  static KeyTranslationFunction _keyTranslationFunction;

private:
  MSString     _string;
  unsigned     _cursorPosition;
  unsigned     _xMargin;
  unsigned     _yMargin;
  unsigned     _maxLength;
  unsigned     _selectionStart;
  unsigned     _selectionEnd;
  EditingMode  _editMode;
  MSPixmap    *_cursorPixmap;
  MSGC        *_imageMSGC;
  MSTimer     *_blinkTimer;
  MSString     _yankBuffer;

protected:
  char        *_selfInsertData;
  unsigned     _scrollIndex;
  MSBoolean    _blinkOn;
  MSBoolean    _haveFocus;
  MSBoolean    _cursorOn;
  MSString     _inputMask;
  char         _inputMaskCharacter;
  MSBoolean    _masking;
  

  void init(void);
  void initKeyTranslations(void);
  
  void updateCursor(void);
  void makeIBeamCursor(void);
  void createGCs(void);
  void moveCursor(unsigned);

  void drawInsertCursor(void);
  void drawOverstrikeCursor(void);
  virtual void drawText(MSBoolean clear_=MSTrue);
  virtual void drawText(unsigned startPos_);
  virtual void drawCursor(void);
  virtual void clearCursor(void);
  virtual void clearTextArea(void);

  virtual unsigned computeCursorPosition(unsigned);
  unsigned computeVisibleLength(void);
  virtual void computeSize(void);
  unsigned positionToX(unsigned);

  MSBoolean haveFocus(void) const;
  MSBoolean blinkOn(void) const;
  MSBoolean cursorOn(void) const;
  MSPixmap *cursorPixmap(void) const;
  MSString& yankBuffer(void);
  MSGC *imageMSGC(void) const;
  GC imageGC(void) const;
  MSTimer *blinkTimer(void) const;
  const char *selfInsertData(void) const;
  unsigned selectionStart(void) const;
  unsigned selectionEnd(void) const;

  virtual unsigned textX(void);
  virtual unsigned textY(void);

  void startBlinking(void);
  void stopBlinking(void);

  void selfInsert(void);  
  void selfInsertData(char *data_);
  void insertString(const char *string_);
  void deleteString(unsigned startPos_,unsigned numChars_);

  virtual void focusIn(void);
  virtual void focusOut(void);
  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char*);
  virtual void buttonPress(const XEvent *);
  virtual void button1Press(const XEvent *);
  virtual void button2Press(const XEvent *);
  virtual void button3Press(const XEvent *);
  virtual void defaultDoubleClickBehavior(const XEvent *);    
  virtual void configure(void);
  virtual void firstMapNotify(void);


  void processCursorTimer(void);

  virtual void updateBackground(unsigned long);
  virtual void updateForeground(unsigned long);
  virtual void updateFont(Font);

  void cursorPositionForw(unsigned);
  void cursorPositionBack(unsigned);
  MSBoolean masking(void) const;
  MSBoolean matchStringToMask(const MSString&) const;

protected:
  
  MSBoolean isSelected(void) const;
  virtual void trackSelection(const XEvent *);
};

inline MSBoolean MSTextField::haveFocus(void) const
{ return _haveFocus; }
inline MSBoolean MSTextField::blinkOn(void) const
{ return _blinkOn; }
inline MSBoolean MSTextField::cursorOn(void) const
{ return _cursorOn; }
inline const MSString& MSTextField::text(void) const
{ return _string; }
inline MSPixmap *MSTextField::cursorPixmap(void) const
{ return _cursorPixmap; }
inline MSString& MSTextField::yankBuffer(void)
{ return _yankBuffer; } 
inline MSGC *MSTextField::imageMSGC(void) const
{ return _imageMSGC; }
inline MSTimer *MSTextField::blinkTimer(void) const
{ return _blinkTimer; }
inline const char *MSTextField::selfInsertData(void) const
{ return _selfInsertData; }
inline unsigned MSTextField::scrollIndex(void) const
{ return _scrollIndex; }

inline unsigned MSTextField::selectionStart(void) const
{ return _selectionStart; }
inline unsigned MSTextField::selectionEnd(void) const
{ return _selectionEnd; }

inline void MSTextField::selfInsertData(char *data_) 
{ _selfInsertData=data_; }

inline unsigned MSTextField::xMargin(void) const
{ return _xMargin; }
inline unsigned MSTextField::yMargin(void) const   
{ return _yMargin; }
inline unsigned MSTextField::margin(void) const     // need to bag - remove ref in MSGraphUI
{ return _yMargin; }

inline MSTextField::EditingMode MSTextField::editMode(void) const
{ return _editMode; }
inline unsigned MSTextField::maxLength(void) const
{ return _maxLength; }
inline unsigned MSTextField::cursorPosition(void) const
{ return _cursorPosition; }
inline unsigned MSTextField::length(void) const
{ return text().length(); }

inline MSTextField::KeyTranslationFunction MSTextField::keyTranslationFunction(void)
{ return _keyTranslationFunction; }

inline void MSTextField::keyTranslationFunction(KeyTranslationFunction function_)
{ _keyTranslationFunction=function_; }

inline MSBoolean MSTextField::inputMaskMode(void) const
{ return _inputMask.length() == 0 ? MSFalse:MSTrue; }

inline MSBoolean MSTextField::masking(void) const
{ return _masking; }
 
inline char MSTextField::inputMaskCharacter(void) const
{ return _inputMaskCharacter; }

inline const MSString& MSTextField::inputMask(void) const
{ return _inputMask; }

inline void MSTextField::inputMaskCharacter(char wild_)
{ _inputMaskCharacter = wild_; }
  

#endif

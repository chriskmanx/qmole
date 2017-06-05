#ifndef MSTextRectHEADER
#define MSTextRectHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSWidget.H>
#include <MSTypes/MSString.H>
#include <MSGUI/MSGC.H>

class MSPixmap;

class MSGUIExport MSTextRect : public MSRect 
{
public:
  MSTextRect(MSWidget *);
  MSTextRect(MSWidget *,const char *);
  MSTextRect(MSWidget *,const MSString&);
  ~MSTextRect(void);

  MSWidget *owner(void) const;
  Display *display(void) const;
  MSDisplayServer *server(void) const;
  Window window(void) const;
  GC textGC(void) const;
  MSPixmap *stipple(void) const;
  const XFontStruct *textFontStruct(void) const;

  const MSString& label(void) const;
  int length(void) const;
  const char *string(void) const;
  unsigned long foreground(void) const;
  unsigned long background(void) const;
  Font font(void) const;
  MSBoolean sensitive(void) const;
  MSBoolean doubleByte(void) const;

  int textWidth(void) const;
  int textWidth(const char *pString_) const;
  int textWidth(const char *pString_,int len_) const;
  int charWidth(void) const;
  int charLbearing(void) const;
  int charRbearing(void) const;
  int textAscent(void) const;
  int textDescent(void) const;
  int textHeight(void) const;

  int charWidth(char aChar_) const; 
  int charLbearing(char aChar_) const; 
  int charRbearing(char aChar_) const; 

  MSBoolean label(const MSString&);
  MSBoolean label(const char *);

  void resize(int w_,int h_);
  void moveTo(int x_,int y_);

  void font(Font);
  void background(unsigned long);  
  void foreground(unsigned long);  
  void sensitive(MSBoolean);

protected:
  MSWidget       *_owner;
  MSGC            _textMSGC;
  XFontStruct    *_textFontStruct;
  Font            _fid;
  unsigned long   _fg;
  unsigned long   _bg;
  MSBoolean       _sensitive;
  MSPixmap       *_stipple;
  MSString        _string;

  MSGC& textMSGC(void);

  void init(void);
  virtual void computeSize(void);
  virtual void updateForeground(unsigned long);
  virtual void updateBackground(unsigned long);
  virtual void updateFont(Font);
  virtual void updateSensitivity(void);

private:
  const XCharStruct *charStruct(char aChar_) const; 
};

#ifndef MS_NO_INLINES
#ifndef MSTextRectINLINES
#include <MSGUI/MSTextRectInlines.C>
#endif
#endif

#endif










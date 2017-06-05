#ifndef MSPrimitiveTextHEADER
#define MSPrimitiveTextHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSPrimitive.H>

class MSPixmap;
class MSGUIExport MSPrimitiveText : public MSPrimitive
{
public:
  MSPrimitiveText(MSWidget *);
  ~MSPrimitiveText(void);

  unsigned alignment(void) const;
  void alignment(unsigned);

protected:
  GC             _textGC;
  XFontStruct   *_textFontStruct;
  unsigned       _alignment;
  MSPixmap      *_stipple;

  void init(void);

  GC textGC(void) const;
  const XFontStruct *textFontStruct(void) const;
  MSPixmap *stipple(void) const;
  MSBoolean fixedWidth(void) const;
  MSBoolean doubleByte(void) const;
  int textWidth(const char *pString_) const;
  int textWidth(const char *pString_,int len_) const;
  int charWidth(void) const;
  int charLbearing(void) const;
  int charRbearing(void) const;
  int textAscent(void) const;
  int textDescent(void) const;
  int textHeight(void) const;
  int charWidth(char) const;  
  int charLbearing(char) const; 
  int charRbearing(char) const; 

  virtual void updateBackground(unsigned long);  
  virtual void updateForeground(unsigned long);  
  virtual void updateFont(Font);
  virtual void updateSensitivity(void);

private:
  const XCharStruct *charStruct(char) const; 
};

#ifndef MS_NO_INLINES
#ifndef MSPrimitiveTextINLINES
#include <MSGUI/MSPrimitiveTextInlines.C>
#endif
#endif

#endif














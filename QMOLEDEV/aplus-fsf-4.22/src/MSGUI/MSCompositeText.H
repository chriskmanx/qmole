#ifndef MSCompositeTextHEADER
#define MSCompositeTextHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSComposite.H>

class MSGUIExport MSCompositeText : public MSComposite
{
public:
  MSCompositeText(MSWidget *);
  ~MSCompositeText(void);

  MSBoolean fixedWidth(void) const;
  MSBoolean doubleByte(void) const;

  int textAscent(void) const;
  int textDescent(void) const;
  int charWidth(void) const;
  int charLbearing(void) const;
  int charRbearing(void) const;
  int textHeight(void) const;
  int textWidth(const char *pString_) const;
  int textWidth(const char *pString_,int len_) const;
  int charWidth(char aChar_) const; 
  int charLbearing(char aChar_) const;
  int charRbearing(char aChar_) const; 

protected:
  GC            _textGC;
  GC            _backgroundGC;
  XFontStruct  *_textFontStruct;

  GC textGC(void) const;
  GC backgroundGC(void) const;
  const XFontStruct *textFontStruct(void) const;

  virtual void updateFont(Font);
  virtual void updateBackground(unsigned long);
  virtual void updateForeground(unsigned long);

private:
  const XCharStruct *charStruct(char aChar_) const; 
};

#ifndef MS_NO_INLINES
#ifndef MSCompositeTextINLINES
#include <MSGUI/MSCompositeTextInlines.C>
#endif
#endif

#endif

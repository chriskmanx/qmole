#ifndef MSFontObjectHEADER
#define MSFontObjectHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <string.h>
#include <stdlib.h>
#include <MSGUI/MSGUIEnum.H>
#include <X11/Xlib.h>

class MSGUIExport MSFontObject
{
private:
  const XFontStruct *_fontStruct;
  const XCharStruct *charStruct(char) const; 

public:
  MSFontObject(const XFontStruct *pFontStruct_=0);
  ~MSFontObject(void);

  void fontStruct(const XFontStruct *);
  const XFontStruct *fontStruct(void) const;

  MSBoolean fixedWidth(void) const; 
  MSBoolean doubleByte(void) const;

  Font font(void) const;
  int textAscent(void) const;
  int textDescent(void) const;
  int charWidth(void) const;
  int charLbearing(void) const;
  int charRbearing(void) const;
  int textHeight(void) const;
  int textWidth(const char *pString_) const;
  int textWidth(const char *pString_,int len_) const;
  int charWidth(char) const; 
  int charLbearing(char) const; 
  int charRbearing(char) const; 
};

inline const XFontStruct *MSFontObject::fontStruct(void) const
{ return _fontStruct; }
inline void MSFontObject::fontStruct(const XFontStruct *fontStruct_) 
{ _fontStruct=fontStruct_; }

inline MSBoolean MSFontObject::fixedWidth(void) const
{ return MSBoolean(fontStruct()->per_char==0); }

inline MSBoolean MSFontObject::doubleByte(void) const
{
   if (fontStruct()->min_byte1 != 0 || fontStruct()->max_byte1 != 0)
      return MSTrue;
   else if (fontStruct()->max_char_or_byte2 > 255)
      return MSTrue;
   else
      return MSFalse;
}

inline int MSFontObject::textAscent(void) const
{ return fontStruct()->max_bounds.ascent; }
inline int MSFontObject::textDescent(void) const 
{ return fontStruct()->max_bounds.descent; }
inline int MSFontObject::charWidth(void) const
{ return fontStruct()->max_bounds.width; }
inline int MSFontObject::charLbearing(void) const  
{ return fontStruct()->max_bounds.lbearing; }
inline int MSFontObject::charRbearing(void) const
{ return fontStruct()->max_bounds.rbearing; }
inline int MSFontObject::textHeight(void) const
{ return (textAscent()+textDescent()); }

inline int MSFontObject::textWidth(const char *pString_) const
{ 
   if (doubleByte())  
     return (pString_!=0)?(int)XTextWidth16((XFontStruct *)fontStruct(),(XChar2b*)pString_,strlen(pString_)/2):0;
  else 
     return (pString_!=0)?(int)XTextWidth((XFontStruct *)fontStruct(),pString_,strlen(pString_)):0; 
}
inline int MSFontObject::textWidth(const char *pString_,int len_) const
{ 
   if (doubleByte()) 
      return (pString_!=0)?(int)XTextWidth16((XFontStruct *)fontStruct(),(XChar2b*)pString_,len_/2):0; 
   else  
      return (pString_!=0)?(int)XTextWidth((XFontStruct *)fontStruct(),pString_,len_):0; 
}

inline const XCharStruct *MSFontObject::charStruct(char c_) const
{ return &(fontStruct()->per_char[c_-fontStruct()->min_char_or_byte2]); }

inline int MSFontObject::charWidth(char aChar_) const
{ 
   return (fontStruct()->per_char!=0&&aChar_>=fontStruct()->min_char_or_byte2&&
          aChar_<=fontStruct()->max_char_or_byte2)?charStruct(aChar_)->width:charWidth();
}
inline int MSFontObject::charLbearing(char aChar_) const
{ return (fontStruct()->per_char!=0&&aChar_>=fontStruct()->min_char_or_byte2 &&
          aChar_<=fontStruct()->max_char_or_byte2)?charStruct(aChar_)->lbearing:charLbearing();
}
inline int MSFontObject::charRbearing(char aChar_) const
{ return (fontStruct()->per_char!=0&&aChar_>=fontStruct()->min_char_or_byte2 &&
          aChar_<=fontStruct()->max_char_or_byte2)?charStruct(aChar_)->rbearing:charRbearing();
}
inline Font MSFontObject::font(void) const
{ return fontStruct()->fid; }

  
#endif







#ifndef MSTextRectINLINES
#define MSTextRectINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MS_NO_INLINES
#define INLINELINKAGE inline
#else
#define INLINELINKAGE
#endif

INLINELINKAGE MSWidget *MSTextRect::owner(void) const
{ return _owner; }
INLINELINKAGE Display *MSTextRect::display(void) const
{ return _owner->display(); }
INLINELINKAGE MSDisplayServer *MSTextRect::server(void) const
{ return _owner->server(); }
INLINELINKAGE Window MSTextRect::window(void) const
{ return _owner->window(); }
INLINELINKAGE MSGC& MSTextRect::textMSGC(void)
{ return _textMSGC; }
INLINELINKAGE MSPixmap *MSTextRect::stipple(void) const
{ return _stipple; }
INLINELINKAGE const XFontStruct *MSTextRect::textFontStruct(void) const
{ return _textFontStruct; }

INLINELINKAGE const MSString& MSTextRect::label(void) const
{ return _string; }
INLINELINKAGE int MSTextRect::length(void) const
{ return _string.length(); }
INLINELINKAGE const char *MSTextRect::string(void) const
{ return _string.string(); }
INLINELINKAGE unsigned long MSTextRect::foreground(void) const
{ return _fg; }  
INLINELINKAGE unsigned long MSTextRect::background(void) const
{ return _bg; }  
INLINELINKAGE Font MSTextRect::font(void) const
{ return _fid; }  
INLINELINKAGE MSBoolean MSTextRect::sensitive(void) const
{ return _sensitive; }

INLINELINKAGE MSBoolean MSTextRect::doubleByte(void) const
{
  if (_textFontStruct->min_byte1!=0||_textFontStruct->max_byte1!=0) return MSTrue;
  else if (_textFontStruct->max_char_or_byte2>255) return MSTrue;
  else return MSFalse;
}

INLINELINKAGE int MSTextRect::textWidth(const char *pString_) const
{ 
  if (doubleByte()==MSTrue)
  return (pString_!=0)?
  (int)XTextWidth16(_textFontStruct,(XChar2b*)pString_,strlen(pString_)/2):0;
  else return (pString_!=0)?(int)XTextWidth(_textFontStruct,pString_,strlen(pString_)):0; 
}

INLINELINKAGE int MSTextRect::textWidth(const char *pString_,int len_) const
{ 
  if (doubleByte()==MSTrue)
  return (pString_!=0)?(int)XTextWidth16(_textFontStruct,(XChar2b*)pString_,len_/2):0; 
  else return (pString_!=0)?(int)XTextWidth(_textFontStruct,pString_,len_):0; 
}

INLINELINKAGE int MSTextRect::textWidth(void) const
{ return textWidth(string(),length()); }

INLINELINKAGE int MSTextRect::charWidth(void) const
{ return _textFontStruct->max_bounds.width; }
INLINELINKAGE int MSTextRect::charLbearing(void) const
{ return _textFontStruct->max_bounds.lbearing; }
INLINELINKAGE int MSTextRect::charRbearing(void) const
{ return _textFontStruct->max_bounds.rbearing; }
INLINELINKAGE int MSTextRect::textAscent(void) const
{ return _textFontStruct->max_bounds.ascent; }
INLINELINKAGE int MSTextRect::textDescent(void) const
{ return _textFontStruct->max_bounds.descent; }
INLINELINKAGE int MSTextRect::textHeight(void) const
{ return (textAscent()+textDescent()); }

INLINELINKAGE const XCharStruct *MSTextRect::charStruct(char aChar_) const
{ return &(_textFontStruct->per_char[aChar_-_textFontStruct->min_char_or_byte2]); }

INLINELINKAGE int MSTextRect::charWidth(char aChar_) const
{ 
  return (_textFontStruct->per_char!=0&&aChar_>=_textFontStruct->min_char_or_byte2&&
	  aChar_<=_textFontStruct->max_char_or_byte2)?
	  charStruct(aChar_)->width:charWidth();
}

INLINELINKAGE int MSTextRect::charLbearing(char aChar_) const
{
  return (_textFontStruct->per_char!=0&&aChar_>=_textFontStruct->min_char_or_byte2&&
	  aChar_<=_textFontStruct->max_char_or_byte2)?
	  charStruct(aChar_)->lbearing:charLbearing();
}

INLINELINKAGE int MSTextRect::charRbearing(char aChar_) const
{
  return (_textFontStruct->per_char!=0&&aChar_>=_textFontStruct->min_char_or_byte2&&
	  aChar_<=_textFontStruct->max_char_or_byte2)?
	  charStruct(aChar_)->rbearing:charRbearing();
}

INLINELINKAGE void MSTextRect::resize(int w_,int h_) 
{ width(w_),height(h_); }
INLINELINKAGE void MSTextRect::moveTo(int x_,int y_) 
{ x(x_),y(y_); }

#endif










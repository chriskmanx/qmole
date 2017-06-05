#ifndef MSCompositeTextINLINES
#define MSCompositeTextINLINES

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

INLINELINKAGE GC MSCompositeText::textGC(void) const                          
{ return _textGC; }
INLINELINKAGE GC MSCompositeText::backgroundGC(void) const                           
{ return _backgroundGC; }

INLINELINKAGE const XFontStruct *MSCompositeText::textFontStruct(void) const
{ return _textFontStruct; }

INLINELINKAGE MSBoolean MSCompositeText::fixedWidth(void) const
{ return MSBoolean(_textFontStruct->per_char==0); }

INLINELINKAGE MSBoolean MSCompositeText::doubleByte(void) const
{
  if (_textFontStruct->min_byte1!=0||_textFontStruct->max_byte1!=0) return MSTrue;
  else if (_textFontStruct->max_char_or_byte2>255) return MSTrue;
  else return MSFalse;
}

INLINELINKAGE int MSCompositeText::textWidth(const char *pString_) const
{ 
  if (pString_!=0)
   {
     unsigned len=strlen(pString_);
     if (doubleByte()==MSTrue)
      { return (int)XTextWidth16(_textFontStruct,(XChar2b*)pString_,len/2); }
     else return XTextWidth(_textFontStruct,pString_,len);
   }
  return 0;
}

INLINELINKAGE int MSCompositeText::textWidth(const char *pString_,int len_) const
{ 
  if (pString_!=0)
   {
     if (doubleByte()==MSTrue)
      { return (int)XTextWidth16(_textFontStruct,(XChar2b*)pString_,len_/2); }
     else return (int)XTextWidth(_textFontStruct,pString_,len_);
   }
  return 0;
}

INLINELINKAGE int MSCompositeText::charWidth(void) const
{ return _textFontStruct->max_bounds.width; }
INLINELINKAGE int MSCompositeText::charLbearing(void) const
{ return _textFontStruct->max_bounds.lbearing; }
INLINELINKAGE int MSCompositeText::charRbearing(void) const
{ return _textFontStruct->max_bounds.rbearing; }
INLINELINKAGE int MSCompositeText::textAscent(void) const  
{ return _textFontStruct->max_bounds.ascent; }
INLINELINKAGE int MSCompositeText::textDescent(void) const  
{ return _textFontStruct->max_bounds.descent; }
INLINELINKAGE int MSCompositeText::textHeight(void) const   
{ return (textAscent()+textDescent()); }

INLINELINKAGE const XCharStruct *MSCompositeText::charStruct(char aChar_) const
{ return &(_textFontStruct->per_char[aChar_-_textFontStruct->min_char_or_byte2]); }

INLINELINKAGE int MSCompositeText::charWidth(char aChar_) const
{ 
  return (_textFontStruct->per_char!=0&&aChar_>=_textFontStruct->min_char_or_byte2&&
	  aChar_<=_textFontStruct->max_char_or_byte2)?charStruct(aChar_)->width:charWidth();
}

INLINELINKAGE int MSCompositeText::charLbearing(char aChar_) const
{
  return (_textFontStruct->per_char!=0&&aChar_>=_textFontStruct->min_char_or_byte2&&
	  aChar_<=_textFontStruct->max_char_or_byte2)?
	  charStruct(aChar_)->lbearing:charLbearing();
}

INLINELINKAGE int MSCompositeText::charRbearing(char aChar_) const
{
  return (_textFontStruct->per_char!=0&&aChar_>=_textFontStruct->min_char_or_byte2&&
	  aChar_<=_textFontStruct->max_char_or_byte2)?
	  charStruct(aChar_)->rbearing:charRbearing();
}

#endif


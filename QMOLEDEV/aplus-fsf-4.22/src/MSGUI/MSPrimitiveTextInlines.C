#ifndef MSPrimitiveTextINLINES
#define MSPrimitiveTextINLINES

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

INLINELINKAGE GC MSPrimitiveText::textGC(void) const                     
{ return _textGC; }
INLINELINKAGE const XFontStruct *MSPrimitiveText::textFontStruct(void) const
{ return _textFontStruct; }
INLINELINKAGE MSPixmap *MSPrimitiveText::stipple(void) const
{ return _stipple; }

INLINELINKAGE MSBoolean MSPrimitiveText::fixedWidth(void) const
{ return MSBoolean(_textFontStruct->per_char==0); }
INLINELINKAGE MSBoolean MSPrimitiveText::doubleByte(void) const
{
  if (_textFontStruct->min_byte1!=0||_textFontStruct->max_byte1!=0) return MSTrue;
  else if (_textFontStruct->max_char_or_byte2>255) return MSTrue;
  else return MSFalse;
}

INLINELINKAGE int MSPrimitiveText::charWidth(void) const
{ return _textFontStruct->max_bounds.width; }
INLINELINKAGE int MSPrimitiveText::charLbearing(void) const
{ return _textFontStruct->max_bounds.lbearing; }
INLINELINKAGE int MSPrimitiveText::charRbearing(void) const
{ return _textFontStruct->max_bounds.rbearing; }
INLINELINKAGE int MSPrimitiveText::textAscent(void) const
{ return _textFontStruct->max_bounds.ascent; }
INLINELINKAGE int MSPrimitiveText::textDescent(void) const
{ return _textFontStruct->max_bounds.descent; }
INLINELINKAGE int MSPrimitiveText::textHeight(void) const  
{ return (textAscent()+textDescent()); }

INLINELINKAGE int MSPrimitiveText::textWidth(const char *pString_) const
{ 
  if (doubleByte())
   {
     return (pString_!=0)?
     (int)XTextWidth16((XFontStruct*)_textFontStruct,(XChar2b*)pString_,strlen(pString_)/2):0;
   }
  else
   {
     return (pString_!=0)?
     (int)XTextWidth((XFontStruct*)_textFontStruct,pString_,strlen(pString_)):0;
   }
}

INLINELINKAGE int MSPrimitiveText::textWidth(const char *pString_,int len_) const
{ 
  if (doubleByte())
   {
     return (pString_!=0)?
     (int)XTextWidth16((XFontStruct*)_textFontStruct,(XChar2b*)pString_,len_/2):0;
   }
  else
   {
     return (pString_!=0)?(int)XTextWidth((XFontStruct*)_textFontStruct,pString_,len_):0;
   }
}

INLINELINKAGE const XCharStruct *MSPrimitiveText::charStruct(char aChar_) const
{ return &(_textFontStruct->per_char[aChar_-_textFontStruct->min_char_or_byte2]); }

INLINELINKAGE int MSPrimitiveText::charWidth(char aChar_) const
{ return (_textFontStruct->per_char!=0&&aChar_>=_textFontStruct->min_char_or_byte2&&
           aChar_<=_textFontStruct->max_char_or_byte2)?
	   charStruct(aChar_)->width:charWidth();
}

INLINELINKAGE int MSPrimitiveText::charLbearing(char aChar_) const
{ return (_textFontStruct->per_char!=0&&aChar_>=_textFontStruct->min_char_or_byte2&&
          aChar_<=_textFontStruct->max_char_or_byte2)?
	  charStruct(aChar_)->lbearing:charLbearing();
}

INLINELINKAGE int MSPrimitiveText::charRbearing(char aChar_) const
{ return (_textFontStruct->per_char!=0&&aChar_>=_textFontStruct->min_char_or_byte2&&
          aChar_<=_textFontStruct->max_char_or_byte2)?
	  charStruct(aChar_)->rbearing:charRbearing();
}

INLINELINKAGE unsigned MSPrimitiveText::alignment(void) const
{ return _alignment; }

#endif














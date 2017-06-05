#ifndef MSWidgetOutputINLINES
#define MSWidgetOutputINLINES

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

INLINELINKAGE unsigned long MSWidgetOutput::highlightColor(void) const
{ return _highlightColor; }
INLINELINKAGE MSBoolean MSWidgetOutput::highlighted(void) const
{ return _highlighted; }
INLINELINKAGE int MSWidgetOutput::highlightThickness(void) const
{ return _highlightThickness; }
INLINELINKAGE int MSWidgetOutput::shadowThickness(void) const
{ return _shadowThickness; }
INLINELINKAGE MSShadowStyle MSWidgetOutput::shadowStyle(void) const
{ return _shadowStyle; }
INLINELINKAGE int MSWidgetOutput::topShadowOffset(void) const
{ return _topShadowOffset; }

INLINELINKAGE MSWidgetOutput::OutputMode MSWidgetOutput::outputMode(void)        
{return _outputMode;}

INLINELINKAGE MSBoolean MSWidgetOutput::doubleByte(const XFontStruct *fs_) const
{
  if (fs_->min_byte1!=0||fs_->max_byte1!=0) return MSTrue;
  else if (fs_->max_char_or_byte2>255) return MSTrue;
  else return MSFalse;
}

INLINELINKAGE void MSWidgetOutput::XClearArea(Display *dpy_,Window id_,
					      int x_,int y_,int w_,int h_,int exposures_)
{
  if (_outputMode<Print) ::XClearArea(dpy_,id_,x_,y_,w_,h_,exposures_);
}

INLINELINKAGE int MSWidgetOutput::XTextWidth(const XFontStruct *fs_,const char *str_,int len_) const
{
  return (doubleByte(fs_)==MSTrue)?
  ::XTextWidth16((XFontStruct *)fs_,(XChar2b *)str_,len_/2):
  ::XTextWidth((XFontStruct *)fs_,(char *)str_,len_);
}

INLINELINKAGE int MSWidgetOutput::XTextExtents(const XFontStruct *fs_,const char *str_,int len_,
					       int *dir_,int *ascent_,int *descent_,
					       XCharStruct*overall_)
{
  return (doubleByte(fs_)==MSTrue)?
  ::XTextExtents16((XFontStruct *)fs_,(XChar2b *)str_,len_/2,dir_,ascent_,descent_,overall_):
  ::XTextExtents((XFontStruct *)fs_,(char *)str_,len_,dir_,ascent_,descent_,overall_);
}
  
INLINELINKAGE int MSWidgetOutput::XTextLength(const XFontStruct *fs_,const char *str_) const
{
  if (fs_!=0&&str_!=0) return (doubleByte(fs_)==MSTrue)?(strlen(str_)/2):strlen(str_);
  return 0;
}

#endif















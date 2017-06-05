///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSCompositeText.H>

#ifdef MS_NO_INLINES
#include <MSGUI/MSCompositeTextInlines.C>
#endif

MSCompositeText::MSCompositeText(MSWidget *owner_) : MSComposite(owner_)
{
  _textGC=0;
  _backgroundGC=0;
  _textFontStruct=0;
  XGCValues values;    
  values.foreground=foreground();
  values.background=background();
  values.font=font();
  
  _textGC=XCreateGC(display(),window(),(GCForeground|GCBackground|GCFont),&values);
  _textFontStruct=(XFontStruct *)server()->fontStruct(font());

  values.foreground=background();
  _backgroundGC=XCreateGC(display(),window(),(GCForeground|GCBackground),&values);
}

MSCompositeText::~MSCompositeText(void)
{
  if (textGC()!=0) XFreeGC(display(),_textGC);
  if (backgroundGC()!=0) XFreeGC(display(),_backgroundGC);
  _textFontStruct=0;
}

void MSCompositeText::updateFont(Font oldfid_)
{
  MSComposite::updateFont(oldfid_);
  XSetFont(display(),textGC(),font());
  _textFontStruct=(XFontStruct *)server()->fontStruct(font());
}

void MSCompositeText::updateBackground(unsigned long oldbg_)
{ 
  MSComposite::updateBackground(oldbg_);
  XSetBackground(display(),textGC(),background()); 
}

void MSCompositeText::updateForeground(unsigned long oldfg_)
{ 
  MSComposite::updateForeground(oldfg_);
  XSetForeground(display(),textGC(),foreground()); 
}


///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSPrimitiveText.H>
#include <MSGUI/MSPixmap.H>

#ifdef MS_NO_INLINES
#include <MSGUI/MSPrimitiveTextInlines.C>
#endif

MSPrimitiveText::MSPrimitiveText(MSWidget *owner_) : MSPrimitive(owner_) 
{ init(); }

MSPrimitiveText::~MSPrimitiveText(void)
{
  if (textGC()!=0) XFreeGC(display(),_textGC); 
  _textFontStruct=0;
  if (stipple()!=0) delete _stipple;
}

void MSPrimitiveText::init(void)
{
  _textGC=0;
  _textFontStruct=0;
  _alignment=MSCenter;
  _stipple=new MSPixmap(server(),MSPixmap::ForegroundFiftyPixmap,1,0,1);

  XGCValues values;    
  values.foreground=foreground();
  values.background=background();
  values.font=font();
  values.stipple=stipple()->pixmap();  
  values.fill_style = FillSolid;
  _textGC=XCreateGC(display(),window(),
		    (GCForeground|GCBackground|GCFont|GCStipple|GCFillStyle),
		    &values);
  _textFontStruct=(XFontStruct *)server()->fontStruct(font());
}

void MSPrimitiveText::alignment(unsigned alignment_) 
{ 
  if (alignment()!=alignment_) 
   { 
     _alignment=alignment_; 
     redraw(); 
   } 
}

void MSPrimitiveText::updateFont(Font oldfid_)
{
  MSPrimitive::updateFont(oldfid_);
  XSetFont(display(),textGC(),font());
  _textFontStruct=(XFontStruct *)server()->fontStruct(font());
}

void MSPrimitiveText::updateForeground(unsigned long oldfg_)
{
  MSPrimitive::updateForeground(oldfg_);
  XSetForeground(display(),textGC(),foreground());     
}

void MSPrimitiveText::updateBackground(unsigned long oldbg_)
{
  MSPrimitive::updateBackground(oldbg_);
  XSetBackground(display(),textGC(),background());     
}

void MSPrimitiveText::updateSensitivity (void)
{
   if (sensitive()) XSetFillStyle(display(),textGC(),FillSolid);
   else XSetFillStyle(display(),textGC(),FillStippled);
}









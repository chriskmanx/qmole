///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSTextRect.H>
#include <MSGUI/MSPixmap.H>

#ifdef MS_NO_INLINES
#include <MSGUI/MSTextRectInlines.C>
#endif

MSTextRect::MSTextRect(MSWidget *owner_) :
    _owner(owner_)
{
  _owner=owner_;
  init();
}

MSTextRect::MSTextRect(MSWidget *owner_,const char *pString_) :
    _owner(owner_),_string(pString_)
{
  init();
}

MSTextRect::MSTextRect(MSWidget *owner_,const MSString& aString_) :
    _owner(owner_),_string(aString_)
{
  init();
}

MSTextRect::~MSTextRect(void)
{ 
  if (stipple()!=0)  delete _stipple;
}

void MSTextRect::init()
{
  configuration(0,0,10,10);
  _textFontStruct=0;
  _fg=owner()->foreground();
  _bg=owner()->background();
  _sensitive=MSTrue;
  _stipple=new MSPixmap(owner()->server(),MSPixmap::ForegroundFiftyPixmap,1,0,1);
  _fid=owner()->font();
  
  XGCValues values;    
  values.foreground=foreground();
  values.background=background();
  values.font=font();
  values.stipple=stipple()->pixmap();
  values.fill_style=FillSolid;
  _textMSGC.setGCValues(server(),MSTrue,&values,
                        (GCForeground|GCBackground|GCFont|GCStipple|GCFillStyle));
  _textFontStruct=(XFontStruct *)server()->fontStruct(font());
  computeSize();
}

GC MSTextRect::textGC(void) const
{ return _textMSGC.gc(); }

// this will use ref counting - const char * will have to make a copy
MSBoolean MSTextRect::label(const MSString& aString_) 
{ 
  if (label()!=aString_) { _string=aString_; return MSTrue; }
  else return MSFalse;
}
MSBoolean MSTextRect::label(const char *pString_)
{ 
  if (label()!=pString_) { _string=pString_; return MSTrue; }
  else return MSFalse;
}

void MSTextRect::computeSize(void) {}

void MSTextRect::font(Font fid_)
{
  if (fid_!=0&&fid_!=font())
   {
     Font oldfid=_fid;
     _fid=fid_;
     textMSGC().font(fid_);
     _textFontStruct=(XFontStruct *)server()->fontStruct(font());
     computeSize();
     updateFont(oldfid);          
   }
}

void MSTextRect::foreground(unsigned long pixel_)
{
  if (foreground()!=pixel_)
   {
     unsigned long oldfg=_fg;     
     _fg=pixel_;
     textMSGC().foreground(foreground());
     updateForeground(oldfg);     
   }
}

void MSTextRect::background(unsigned long pixel_)
{
  if (background()!=pixel_)
   {
     unsigned long oldbg=_bg;
     _bg=pixel_;
     textMSGC().background(background());
     updateBackground(oldbg);
   }
}

void MSTextRect::sensitive(MSBoolean sensitive_)
{
   if (sensitive()!=sensitive_)
   {
      _sensitive=sensitive_;
      if (sensitive()) textMSGC().fillStyle(FillSolid);
      else textMSGC().fillStyle(FillStippled);
      updateSensitivity();
   }
}


// subclass should override for notification of changes
void MSTextRect::updateForeground(unsigned long)
{}
void MSTextRect::updateBackground(unsigned long)
{}
void MSTextRect::updateFont(Font)
{}
void MSTextRect::updateSensitivity(void)
{}






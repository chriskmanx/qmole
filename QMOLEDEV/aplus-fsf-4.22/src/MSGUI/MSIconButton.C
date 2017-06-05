///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSIconButton.H>
#include <MSTypes/MSMessageLog.H>

static const unsigned long MSIconButtonEventMask=(ExposureMask|ButtonPressMask|
						  ButtonReleaseMask|Button1MotionMask);
static const int MSIconButtonDefaultShadowThickness=2;
static const int MSIconButtonDefaultHighlightThickness=1;
static const char *MSIconButtonDefaultPixmap="LabelDefaultPixmap"; //Yes, LabelDefaultPixmap is correct

MSIconButton::MSIconButton(MSWidget *owner_,const char *label_,const MSPixmap &pixmap_,
			   const MSPixmap &insensitivePixmap_,const MSPixmap &armedPixmap_)
: MSIcon(owner_,label_,pixmap_,insensitivePixmap_)
{ init(armedPixmap_); }

MSIconButton::MSIconButton(MSWidget *owner_,const MSStringVector &label_,const MSPixmap &pixmap_,
			   const MSPixmap &insensitivePixmap_,const MSPixmap &armedPixmap_)
: MSIcon(owner_,label_,pixmap_,insensitivePixmap_)
{ init(armedPixmap_); }

MSIconButton::MSIconButton(MSWidget *owner_,MSStringVector &label_,const MSPixmap &pixmap_,
			   const MSPixmap &insensitivePixmap_,const MSPixmap &armedPixmap_)
: MSIcon(owner_,label_,pixmap_,insensitivePixmap_)
{ init(armedPixmap_); }

MSIconButton::MSIconButton(MSWidget *owner_,const char *label_) : 
MSIcon(owner_,label_)
{init();}

MSIconButton::MSIconButton(MSWidget *owner_,const MSStringVector& label_) : 
MSIcon(owner_,label_)
{ init(); }

MSIconButton::MSIconButton(MSWidget *owner_,const MSPixmap &pixmap_,
                           const MSPixmap &insensitivePixmap_,const MSPixmap &armedPixmap_) :
MSIcon(owner_,pixmap_,insensitivePixmap_)
{
  init(armedPixmap_);
}

MSIconButton::~MSIconButton(void)
{ if (armedPixmap()!=0) delete _armedPixmap; }

void MSIconButton::init(void)
{
  _armedPixmap=0;
  _highlightThickness=MSIconButtonDefaultHighlightThickness;
  _shadowThickness=MSIconButtonDefaultShadowThickness;
  _armed=MSFalse;
  shadowStyle(MSRaised);
  acceptFocus(MSTrue);
  selectInput(MSIconButtonEventMask);
  addToFocusList();
}

void MSIconButton::init(const MSPixmap &armedPixmap_)
{ 
  init();
  if (armedPixmap_.server()==server()) _armedPixmap=new MSPixmap(armedPixmap_);
  else
  {
    MSMessageLog::warningMessage("Warning - Armed Pixmap supplied for MSIconButton is invalid, using default");
     createDefaultArmedPixmap(armedPixmap_.width(),armedPixmap_.height(),
			      armedPixmap_.foreground(),armedPixmap_.background());
  }
}

void MSIconButton::createDefaultArmedPixmap(int w_,int h_,unsigned long fg_,unsigned long bg_)
{ 
   _armedPixmap=new MSPixmap(server(),MSIconButtonDefaultPixmap,w_,h_,fg_,bg_); 
   GC gc=XCreateGC(display(),window(),0,0);
   XSetForeground(display(),gc,bg_);
   XFillRectangle(display(),armedPixmap()->pixmap(),gc,0,0,w_,h_);
   XFreeGC(display(),gc);
}

void MSIconButton::armedPixmap(const MSPixmap &armedPixmap_)
{
   MSPixmap *tmp=_armedPixmap;
   if (armedPixmap_.server()==server())
      _armedPixmap=new MSPixmap(armedPixmap_);
   else
   {
     MSMessageLog::warningMessage("Warning - Armed Pixmap supplied for MSIconButton is invalid, using default");
     createDefaultArmedPixmap(armedPixmap_.width(),armedPixmap_.height(),
			      armedPixmap_.foreground(),armedPixmap_.background());
   }
   if (tmp!=0) delete tmp;
   updatePixmap();
}

void MSIconButton::redraw(void)
{
  if (mapped()==MSTrue&&frozen()==MSFalse)
   {
     drawBackground();
     if (showPixmap()==MSTrue) drawPixmap();
     if (showLabel()==MSTrue) drawLabel();
     (armed()==MSTrue)?drawSunken():drawRaised();
   }
}

void MSIconButton::drawLabel(void)
{
  if (mapped()==MSTrue)
   {
     if (armed()==MSTrue)
      {
        if (depth()==1)
         { 
           XSetForeground(display(),textGC(),background());
           XSetBackground(display(),textGC(),foreground());
           MSIcon::drawLabel();
         }
        else
         {
           XSetBackground(display(),textGC(),selectShadowColor());     
           MSIcon::drawLabel();
         }
      }
     else
      {
        XSetBackground(display(),textGC(),background());     
        MSIcon::drawLabel();
      }
   }
}

void MSIconButton::drawPixmap(void)
{
   if (mapped()==MSTrue)
   {
      const MSPixmap *pmap;
      if (armed()==MSTrue) pmap=armedPixmap();
      else if (sensitive()==MSTrue) pmap=pixmap();
      else pmap=insensitivePixmap();
      if (pmap!=0)
       {
	 int startx=computePixmapXCoord(pmap);
	 int starty=computePixmapYCoord(pmap);
         GC gc=pixmapGC();
	 XSetForeground(display(),gc,foreground());
	 if (armed()==MSTrue) XSetBackground(display(),gc,selectShadowColor());
	 else XSetBackground(display(),gc,background());
	 copyPixmap(display(),*pmap,window(),gc,startx,starty);
       }
    }
}

void MSIconButton::drawBackground(void)
{
  if (mapped()==MSTrue)
   {
     int ht=highlightThickness();
     XFillRectangle(display(),window(),
                    (armed()==MSTrue)?selectShadowGC():backgroundShadowGC(),
                    ht,ht,width()-(ht<<1),height()-(ht<<1));
     if (highlighted()==MSTrue) drawHighlight();
     else undrawHighlight();
   }
}

void MSIconButton::key(KeySym keysym_,unsigned int,const char *)
{
  if (keysym_==XK_Return) 
   {
     arm();
     activate();
     disarm();
   }
}

void MSIconButton::keyPress(const XEvent *pEvent_,KeySym k_,unsigned int s_,const char *b_)
{
  MSKeyPress keyPress(k_,s_);
  if (isProtected()==MSFalse&&keyTranslate(keyPress)==MSFalse) key(k_,s_,b_); }

void MSIconButton::buttonPress(const XEvent *event_)
{
  if (event_->xbutton.button==Button1)
   { 
     if (isProtected()==MSFalse)
      {
        MSBoolean cont=MSTrue;
        if (acceptFocus()==MSTrue) cont=traverseFocus(this);
        if (cont==MSTrue) buttonPressNotify(this,event_); 
      }
   }
}

void MSIconButton::buttonRelease(const XEvent *event_)
{ if (event_->xbutton.button==Button1) buttonReleaseNotify(this,event_); }

void MSIconButton::button1Press(const XEvent *)
{ arm(); }

void MSIconButton::button1Release(const XEvent *) 
{ 
  if (armed()==MSTrue) 
   {
     activate(); 
     disarm();
     //Discard all button events before returning
     XEvent event;
     while (XCheckWindowEvent(display(),window(),
                              ButtonPressMask|ButtonReleaseMask|Button1MotionMask,
                              &event));
   }
}

void MSIconButton::motionNotify(const XEvent *event_)
{
   if (isProtected()==MSFalse && (acceptFocus()==MSFalse||inputFocus()==this))
   {
      if (event_->xmotion.is_hint==NotifyNormal&&(event_->xmotion.state&Button1Mask)==Button1Mask)
      {
	 if (event_->xmotion.x<0||event_->xmotion.x>width()||
	     event_->xmotion.y<0||event_->xmotion.y>height())
	 {	disarm(); }
	 else if (armed()==MSFalse)
	 {
	    if ((event_->xmotion.x>=0&&event_->xmotion.x<=width())&&
		(event_->xmotion.y>=0&&event_->xmotion.y<=height()))
	    { arm(); }
	 }
      }
   }
}

// default activate method
void MSIconButton::activate(void) 
{ activateCallback(MSWidgetCallback::activate); }

void MSIconButton::arm(void)
{
  if (armed()==MSFalse)
   {
     _armed=MSTrue;
     if (mapped()==MSTrue)
      {
        int offset=highlightThickness()+shadowThickness();
        XFillRectangle(display(),window(),selectShadowGC(),
	    	       offset,offset,width()-2*offset,height()-2*offset);
	if (showPixmap()==MSTrue) drawPixmap();
        if (showLabel()==MSTrue) drawLabel();
        drawSunken();
        XFlush(display());
      }
   }
}

void MSIconButton::disarm(void)
{
  if (armed()==MSTrue)
   {
     _armed=MSFalse;
     if (mapped()==MSTrue)
      {
        int offset=highlightThickness()+shadowThickness();
        XFillRectangle(display(),window(),backgroundShadowGC(),
	   	       offset,offset,width()-2*offset,height()-2*offset);
	if (showPixmap()==MSTrue) drawPixmap();
        if (showLabel()==MSTrue) drawLabel();
        drawRaised();
        XFlush(display());
      }
   }
}

int MSIconButton::computeXCoord(int row_,int column_,const char *string_,int len_)
{
  int pixmapW=0;
  // Checking if _pixmap is equal to zero is good enough because the existence of
  // _pixmap implies the existence of _insensitivePixmap and _armedPixmap
  if (showPixmap()==MSTrue&&pixmap()!=0)
   {
     if (armed()==MSTrue) pixmapW=armedPixmap()->width();
     else if (sensitive()==MSTrue) pixmapW=pixmap()->width();
     else pixmapW=insensitivePixmap()->width();
   }
  return computeXCoordinate(row_,column_,pixmapW,string_,len_);
}

int MSIconButton::computeYCoord(int row_)
{
  int pixmapH=0;
  // Checking if _pixmap is equal to zero is good enough because the existence of
  // _pixmap implies the existence of _insensitivePixmap and _armedPixmap
  if (showPixmap()==MSTrue&&pixmap()!=0)
   {
     if (armed()==MSTrue) pixmapH=armedPixmap()->width();
     else if (sensitive()==MSTrue) pixmapH=pixmap()->height();
     else pixmapH=insensitivePixmap()->height();
   }
  return computeYCoordinate(row_,pixmapH);
}

void MSIconButton::updatePixmap(void)
{
  if (pixmap()==0)
   {
     if (insensitivePixmap()!=0) _pixmap=new MSPixmap(*insensitivePixmap());
     else if (armedPixmap()!=0) _pixmap=new MSPixmap(*armedPixmap());
   }
  if (insensitivePixmap()==0)
   {
     if (pixmap()!=0) _insensitivePixmap=new MSPixmap(*pixmap());
     else if (armedPixmap()!=0) _insensitivePixmap=new MSPixmap(*armedPixmap());
   }
  if (armedPixmap()==0)
   {
     if (pixmap()!=0) _armedPixmap=new MSPixmap(*pixmap());
     else if (insensitivePixmap()!=0) _armedPixmap=new MSPixmap(*insensitivePixmap());
   }
  
   if (firstMap()==MSTrue)
   {
     if (dynamic()==MSTrue) computeSize();
     else redraw();
   }
}

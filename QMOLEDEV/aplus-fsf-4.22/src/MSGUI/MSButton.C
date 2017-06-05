///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSButton.H>
#include <MSTypes/MSMessageLog.H>

static const unsigned long MSButtonEventMask=(ExposureMask|ButtonPressMask|
                                              ButtonReleaseMask|Button1MotionMask);
static const int MSButtonDefaultShadowThickness=2;
static const int MSButtonDefaultHighlightThickness=1;
static const char *MSButtonDefaultPixmap="LabelDefaultPixmap"; //Yes, LabelDefaultPixmap is correct

MSButton::MSButton(MSWidget *owner_) : 
MSLabel(owner_)
{init();}

MSButton::MSButton(MSWidget *owner_,const char *label_) : 
MSLabel(owner_,label_)
{init();}

MSButton::MSButton(MSWidget *owner_,const MSStringVector& label_) : 
MSLabel(owner_,label_)
{ init(); }

MSButton::MSButton(MSWidget *owner_,const MSPixmap &pixmap_,
		   const MSPixmap &insensitivePixmap_,const MSPixmap &armedPixmap_) :
MSLabel(owner_,pixmap_,insensitivePixmap_)
{
  init();
  if (armedPixmap_.server()==server()) 
     _armedPixmap=new MSPixmap(armedPixmap_);
  else
  {
    MSMessageLog::warningMessage("Warning : Armed Pixmap supplied for MSButton is invalid, using default");
    
     createDefaultArmedPixmap(armedPixmap_.width(),armedPixmap_.height(),
			      armedPixmap_.foreground(),armedPixmap_.background());
  }

}

MSButton::~MSButton(void) 
{ if (armedPixmap()!=0) delete _armedPixmap; }

void MSButton::init(void)
{
  _highlightThickness=MSButtonDefaultHighlightThickness;
  _shadowThickness=MSButtonDefaultShadowThickness;
  _armed=MSFalse;
  _armedPixmap=0;
  shadowStyle(MSRaised);
  sensitive(MSTrue);
  acceptFocus(MSTrue);
  selectInput(MSButtonEventMask);
  addToFocusList();
}

void MSButton::createDefaultArmedPixmap(int w_,int h_,unsigned long fg_,unsigned long bg_)
{ 
   _armedPixmap=new MSPixmap(server(),MSButtonDefaultPixmap,w_,h_,fg_,bg_); 
   GC gc=XCreateGC(display(),window(),0,0);
   XSetForeground(display(),gc,bg_);
   XFillRectangle(display(),armedPixmap()->pixmap(),gc,0,0,w_,h_);
   XFreeGC(display(),gc);
}

void MSButton::armedPixmap(const MSPixmap &armedPixmap_)
{
   MSPixmap *tmp=_armedPixmap;
   if (armedPixmap_.server()==server())
      _armedPixmap=new MSPixmap(armedPixmap_);
   else
   {
     MSMessageLog::warningMessage("Warning : Armed Pixmap supplied for MSButton is invalid, using default");
     createDefaultArmedPixmap(armedPixmap_.width(),armedPixmap_.height(),
			      armedPixmap_.foreground(),armedPixmap_.background());
   }
   if (tmp!=0) delete tmp;
   updatePixmap();
 }

void MSButton::updatePixmap(void)
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

void MSButton::redraw(void)
{
  if (mapped()==MSTrue&&frozen()==MSFalse)
   {
     drawBackground();
     if (pixmap()==0) drawLabel();
     else drawPixmap();
     (armed()==MSTrue)?drawSunken():drawRaised();
   }
}

void MSButton::updateSensitivity(void)
{
   MSLabel::updateSensitivity();
   if (sensitive()==MSTrue)
      acceptFocus(MSTrue);
   else
      acceptFocus(MSFalse);
}

void MSButton::drawLabel(void)
{
  if (mapped()==MSTrue)
   {
     if (armed()==MSTrue)
      {
        if (depth()==1)
         { 
           XSetForeground(display(),textGC(),background());
           XSetBackground(display(),textGC(),foreground());
           MSLabel::drawLabel();
         }
        else
         {
           XSetBackground(display(),textGC(),selectShadowColor());     
           MSLabel::drawLabel();
         }
      }
     else
      {
        XSetBackground(display(),textGC(),background());     
        MSLabel::drawLabel();
      }
   }
}

void MSButton::drawPixmap(void)
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

void MSButton::drawBackground(void)
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

void MSButton::key(KeySym keysym_,unsigned int,const char *)
{
  if (keysym_==XK_Return) 
   {
     arm();
     activate();
     disarm();
   }
  else if (keysym_==XK_Up) up();
  else if (keysym_==XK_Down) down();
  else if (keysym_==XK_Left) left();
  else if (keysym_==XK_Right) right();
}

void MSButton::keyPress(const XEvent *pEvent_,KeySym k_,unsigned int s_,const char *b_)
{
  MSKeyPress keyPress(k_,s_);
  if (isProtected()==MSFalse&& keyTranslate(keyPress)==MSFalse) key(k_,s_,b_);
}

void MSButton::buttonPress(const XEvent *event_)
{
  if (visible()==MSTrue)
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
}

void MSButton::buttonRelease(const XEvent *event_)
{ if (event_->xbutton.button==Button1) buttonReleaseNotify(this,event_); }

void MSButton::button1Press(const XEvent *)
{ arm();}

void MSButton::button1Release(const XEvent *) 
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

void MSButton::motionNotify(const XEvent *event_)
{
  if (isProtected()==MSFalse && (acceptFocus()==MSFalse||inputFocus()==this))
   {
     if (event_->xmotion.is_hint==NotifyNormal&&(event_->xmotion.state&Button1Mask)==Button1Mask)
      {
	if (event_->xmotion.x<0||event_->xmotion.x>width()||
	    event_->xmotion.y<0||event_->xmotion.y>height())
	 { disarm(); }
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
void MSButton::activate(void) 
{ activateCallback(MSWidgetCallback::activate); }

void MSButton::arm(void)
{
  if (armed()==MSFalse)
   {
     _armed=MSTrue;
     if (mapped()==MSTrue)
      {
        int offset=highlightThickness()+shadowThickness();
        XFillRectangle(display(),window(),selectShadowGC(),
	    	       offset,offset,width()-2*offset,height()-2*offset);
        if (pixmap()==0) drawLabel();
	else drawPixmap();
        drawSunken();
        XFlush(display());
      }
   }
}

void MSButton::disarm(void)
{
  if (armed()==MSTrue)
   {
     _armed=MSFalse;
     if (mapped()==MSTrue)
      {
        int offset=highlightThickness()+shadowThickness();
        XFillRectangle(display(),window(),backgroundShadowGC(),
	   	       offset,offset,width()-2*offset,height()-2*offset);
        if (pixmap()==0) drawLabel();
	else drawPixmap();
        drawRaised();
        XFlush(display());
      }
   }
}

MSAttrValueList& MSButton::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("activate","",MSAttrValue::Callback);
  return MSLabel::get(avList_);
}

// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################

void MSButton::up(void) {}
void MSButton::down(void) {}
void MSButton::left(void) {}
void MSButton::right(void) {}



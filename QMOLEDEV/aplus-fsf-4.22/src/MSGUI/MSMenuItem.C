///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSMenuItem.H>
#include <MSGUI/MSMenu.H>
#include <MSGUI/MSPixmap.H>
#include <MSGUI/MSArrow.H>
#include <MSGUI/MSFontObject.H>
#include <MSTypes/MSMessageLog.H>

static const int   MSMenuItemDefaultShadowThickness=2;
static const int   MSMenuItemDefaultHighlightThickness=0;
static const int   MSMenuItemDefaultMarginWidth=4;
static const int   MSMenuItemDefaultMarginHeight=2;
static const int   MSMenuItemDefaultIndentation=0;
static const int   MSMenuItemDefaultSpacing=5;
static const char *MSMenuItemDefaultPixmap="MenuItemDefaultPixmap"; 

MSBoolean MSMenuItem::_defaultMnemonic=MSTrue;

MSBoolean MSMenuItem::defaultMnemonic(void)
{ return _defaultMnemonic; }

void MSMenuItem::defaultMnemonic(MSBoolean mnemonic_)
{ _defaultMnemonic = mnemonic_; }


MSMenuItem::MSMenuItem(MSMenu *owner_) :
MSWidgetOutput(owner_)
{
  _mnemonic=0;
  _tag=-1; 
  _pixmap=0;
  _insensitivePixmap=0;
  init(); 
  _showState = ShowLabel;
}

MSMenuItem::MSMenuItem(MSMenu *owner_,const char *label_,char mnemonic_,int tag_) :
MSWidgetOutput(owner_)
{ 
  _pixmap=0;
  _insensitivePixmap=0;
  _label=label_; 
  _mnemonic=mnemonic_;
  _tag=tag_;
  if (mnemonic()==0&&label().length()>1&&defaultMnemonic()==MSTrue) _mnemonic=label().first();
  init();
  _showState = ShowLabel;
}

MSMenuItem::MSMenuItem(MSMenu *owner_,const MSString& label_,char mnemonic_,int tag_) :
MSWidgetOutput(owner_)
{ 
  _pixmap=0;
  _insensitivePixmap=0;
  _label=label_; 
  _mnemonic=mnemonic_;
  _tag=tag_;
  if (mnemonic()==0&&label().length()>1&&defaultMnemonic()==MSTrue) _mnemonic=label().first();
  init();
  _showState = ShowLabel;
}

MSMenuItem::MSMenuItem(MSMenu *owner_,const MSPixmap &pixmap_,
		       const MSPixmap &insensitivePixmap_,int tag_) :
MSWidgetOutput(owner_)
{
  _mnemonic=0;
  _tag=tag_;
  initPixmaps(pixmap_,insensitivePixmap_);
  init();
  _showState = ShowPixmap;
}

MSMenuItem::MSMenuItem(MSMenu *owner_,const MSString& label_, const MSPixmap &pixmap_,
		       const MSPixmap &insensitivePixmap_,char mnemonic_,int tag_) :
MSWidgetOutput(owner_)
{
  _mnemonic=mnemonic_;
  _label = label_;
  if (mnemonic()==0&&label().length()>1&&defaultMnemonic()==MSTrue) _mnemonic=label().first();
  _tag=tag_;
  initPixmaps(pixmap_,insensitivePixmap_);
  init();
  _showState = ShowBoth;
}

MSMenuItem::MSMenuItem(MSMenu *owner_,const char* label_, const MSPixmap &pixmap_,
		       const MSPixmap &insensitivePixmap_,char mnemonic_,int tag_) :
MSWidgetOutput(owner_)
{
  _mnemonic=mnemonic_;
  _label = label_;
  if (mnemonic()==0&&label().length()>1&&defaultMnemonic()==MSTrue) _mnemonic=label().first();
  _tag=tag_;
  initPixmaps(pixmap_,insensitivePixmap_);
  init();
  _showState = ShowBoth;
}

MSMenuItem::~MSMenuItem(void)
{
  if (_fontObject!=0)        delete _fontObject;
  if (_stipple!=0)           delete _stipple;
  if (_pixmap!=0)            delete _pixmap;
  if (_insensitivePixmap!=0) delete _insensitivePixmap;
  if (_pixmapGC!=0) XFreeGC(display(),_pixmapGC);
}

void MSMenuItem::init(void)
{
  childCreateNotify();   
  _acceptFocus=MSTrue;
  _highlightThickness=MSMenuItemDefaultHighlightThickness;
  _shadowThickness=MSMenuItemDefaultShadowThickness;  
  _marginWidth=MSMenuItemDefaultMarginWidth;
  _marginHeight=MSMenuItemDefaultMarginHeight;
  _indent=MSMenuItemDefaultIndentation;
  _item=0;
  _armed=MSFalse;
  _selected=MSFalse;
  _cascade=MSFalse;
  _fontObject=new MSFontObject(server()->fontStruct(MSWidget::font()));
  _stipple=new MSPixmap(server(),MSPixmap::ForegroundFiftyPixmap,1,0,1);
  _pixmapGC=0;
  _dynamic=MSTrue;
  _mapped=MSTrue;
  shadowStyle(MSRaised);
  createGCs();
  childMapNotify(); 
  changeState(MSFalse);
  spacing(MSMenuItemDefaultSpacing);
}

void MSMenuItem::initPixmaps(const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_)
{
  if (pixmap_.server()==server())
    _pixmap=new MSPixmap(pixmap_);
  else
    {
      MSMessageLog::warningMessage("Warning : Pixmap supplied for MSMenuItem is invalid, using default");
      _pixmap=createDefaultPixmap(pixmap_.width(),pixmap_.height(),
				  pixmap_.foreground(),pixmap_.background());
    }
  if (insensitivePixmap_.server()==server())
    {
      _insensitivePixmap=new MSPixmap(insensitivePixmap_);
    }
  else
    {
      MSMessageLog::warningMessage("Warning : Insensitive Pixmap supplied for MSMenuItem is invalid, using default");
      _insensitivePixmap=createDefaultPixmap(insensitivePixmap_.width(),
					     insensitivePixmap_.height(),
					     insensitivePixmap_.foreground(),
					     insensitivePixmap_.background());
    }
}

MSGC& MSMenuItem::textMSGC(void)
{ return _textMSGC; }
GC MSMenuItem::textGC(void) const     
{ return _textMSGC.gc(); }

void MSMenuItem::pixmap(const MSPixmap &pixmap_)
{
  if(showState()==ShowLabel)
    showState(ShowBoth);
  else 
    showState(ShowPixmap);

  MSPixmap *tmp=_pixmap;
  if (pixmap_.server()==server())
  _pixmap=new MSPixmap(pixmap_);
  else
   {
     MSMessageLog::warningMessage("Warning : Pixmap supplied for MSMenuItem is invalid, using default");
     _pixmap=createDefaultPixmap(pixmap_.width(),pixmap_.height(),
				 pixmap_.foreground(),pixmap_.background());
   }
  if (tmp!=0) delete tmp;
  // if we don't have insensitive pixmap, duplicate it
  if (insensitivePixmap()==0) _insensitivePixmap=new MSPixmap(*pixmap());
  if (dynamic()==MSTrue) computeSize();
  else if (owner()->mapped()==MSTrue)
   {
     drawBackground();
     drawSymbol();
     if(showState()!=ShowPixmap)drawLabel();
     drawPixmap();
     (armed()==MSTrue)?drawSunken():drawRaised();
   }
}

void MSMenuItem::insensitivePixmap(const MSPixmap &insensitivePixmap_)
{
  MSPixmap *tmp=_insensitivePixmap;
  if (insensitivePixmap_.server()==server())
  _insensitivePixmap=new MSPixmap(insensitivePixmap_);
  else
   {
     MSMessageLog::warningMessage("Warning : Insensitive Pixmap supplied for MSMenuItem is invalid, using default");
     _insensitivePixmap=createDefaultPixmap(insensitivePixmap_.width(),
					    insensitivePixmap_.height(),
					    insensitivePixmap_.foreground(),
					    insensitivePixmap_.background());
   }
  if (tmp!=0) delete tmp;
  // if we don't have sensitive pixmap, duplicate it
  if (pixmap()==0) _pixmap=new MSPixmap(*insensitivePixmap());
  if (dynamic()==MSTrue) computeSize();
  else if (owner()->mapped()==MSTrue)
   {
     drawBackground();
     drawSymbol(); 
     if(showState()!=ShowPixmap) drawLabel();
     drawPixmap();
     (armed()==MSTrue)?drawSunken():drawRaised();
   }
}

void MSMenuItem::createGCs(void)
{
  XGCValues values;
  unsigned long valueMask=(GCForeground|GCFont|GCStipple);
  values.foreground=MSWidget::foreground();
  values.font=MSWidget::font();
  values.stipple=stipple()->pixmap();
  _textMSGC.setGCValues(server(),MSTrue,&values,valueMask);
}

MSPixmap *MSMenuItem::createDefaultPixmap(int w_,int h_,unsigned long fg_,unsigned long bg_)
{
   MSPixmap *pixmap=new MSPixmap(server(),MSMenuItemDefaultPixmap,w_,h_,fg_,bg_); 
   GC gc=XCreateGC(display(),owner()->window(),0,0);
   XSetForeground(display(),gc,bg_);
   XFillRectangle(display(),pixmap->pixmap(),gc,0,0,w_,h_);
   XFreeGC(display(),gc);
   return pixmap;
}

void MSMenuItem::marginWidth(int margin_)
{
  if (marginWidth()!=margin_)
   {
     _marginWidth=margin_;
     if (dynamic()==MSTrue) computeSize();
   }
}

void MSMenuItem::marginHeight(int margin_)
{
  if (marginHeight()!=margin_)
   {
     _marginHeight=margin_;
     if (dynamic()==MSTrue) computeSize();
   }
}

void MSMenuItem::indent(int indent_)
{
  if (indent()!=indent_)
   {
     _indent=indent_;
     if (dynamic()==MSTrue&&firstMap()==MSTrue) computeSize();
   }
}

void MSMenuItem::updateFont(Font oldfid_)
{
  MSWidgetOutput::updateFont(oldfid_);
  fontObject()->fontStruct(server()->fontStruct(font()));
  textMSGC().font(font());
  if (dynamic()==MSTrue) computeSize();
  else redraw();
}

void MSMenuItem::updateForeground(unsigned long oldfg_)
{
  MSWidgetOutput::updateForeground(oldfg_);
  textMSGC().foreground(foreground());
  if (showState() != ShowPixmap) drawLabel();
  if (showState() != ShowLabel) drawPixmap();
}

void MSMenuItem::updateBackground(unsigned long oldbg_)
{ 
  MSWidgetOutput::updateBackground(oldbg_); 
  if (dynamic()==MSTrue) computeSize();
  else redraw();
}

void MSMenuItem::updateSensitivity(void)
{
  if (sensitive()==MSTrue) textMSGC().fillStyle(FillSolid);
  else textMSGC().fillStyle (FillStippled);
  if (mapped()==MSTrue) redraw();
}

void MSMenuItem::spacing(int spacing_)
{ 
  if ( spacing()!=spacing_ )
   {
     _spacing = spacing_; 
     if (dynamic()==MSTrue) computeSize();
     else redraw();
   }
  
} 


void MSMenuItem::redraw(void)
{
  if (owner()->mapped()==MSTrue&&owner()->frozen()==MSFalse)
   { 
     drawBackground(); 
     if (showState()!=ShowBoth)   drawSymbol(); 
     if (showState()!=ShowLabel)  drawPixmap();
     if (showState()!=ShowPixmap) drawLabel();
     if (selected()==MSTrue)      drawShadow();
   }
}

void MSMenuItem::drawPixmap(void)
{
  if (owner()->mapped()==MSTrue&&owner()->frozen()==MSFalse)
   {
     const MSPixmap *pmap;
     pmap = currentPixmap();
     if(pmap!=0)
       {
	 int srcX,srcY,width,height,destX,destY;
	 computePixmapDrawingCoord(pmap,srcX,srcY,width,height,destX,destY);
	 GC gc=pixmapGC();
	 XSetForeground(display(),gc,foreground());
	 XSetBackground(display(),gc,background());
	 copyPixmap(display(),*pmap,owner()->window(),gc,destX,destY);
       }
   }
}
void MSMenuItem::computePixmapDrawingCoord(const MSPixmap *pixmap_,int &srcX_,int &srcY_,
					   int &width_,int &height_,int &destX_,int &destY_)
{
  int heightOffset=highlightThickness()+shadowThickness()+marginHeight();
  int widthOffset=highlightThickness()+shadowThickness()+marginWidth();
  int myHeight=height()-heightOffset*2; 
  int myWidth;
/*
  if ( showState()!=ShowLabel) 
  myWidth = width()-widthOffset*2-labelWidth()-indent();
  else
  myWidth = width()-widthOffset*2-labelWidth();
*/
  if ( showState()==ShowPixmap)  myWidth = width()-widthOffset*2-indent();
  else if ( showState()==ShowBoth)  myWidth = indent()-spacing();

  if (pixmap_->height()>myHeight)
   {
     height_=myHeight;
     srcY_=(pixmap_->height()-myHeight)/2;
     destY_=heightOffset+y_origin();
   }
  else
   {
     height_=pixmap_->height();
     srcY_=0;
     destY_=heightOffset+y_origin()+(myHeight-pixmap_->height())/2;
   }
  if (pixmap_->width()>myWidth)
   {
     width_=myWidth;
     srcX_=(pixmap_->width()-myWidth)/2;
     destX_=widthOffset+x_origin();
   }
  else
   {
     width_=pixmap_->width();
     srcX_=0;
     destX_ =widthOffset+x_origin();
   }
  if(showState() == ShowPixmap) destX_ += indent();  
}

void MSMenuItem::drawLabel(void)
{
  if (owner()->mapped()==MSTrue&&owner()->frozen()==MSFalse)
   { 
     if (label().length()>0)
      {
	int y=computeYCoord();
	int x=computeXCoord();
	int len=label().length();
	const char *pString=label();
	int dw=drawWidth();
	if (dw>0) while (fontObject()->textWidth(pString,len)>dw) len--;
	if (len>0) 
	 {
	   XDrawString(display(),owner()->window(),textGC(),fontObject()->fontStruct(),
		       x,y+fontObject()->textAscent(),pString,len);
	   if (mnemonic()!=0)
	    {
	      unsigned u=label().indexOf(toupper(mnemonic()));
	      unsigned l=label().indexOf(tolower(mnemonic()));
	      unsigned i=(u<l)?u:l;
	      if (i<label().length())
	       {
		 int cw=fontObject()->charWidth(mnemonic());
		 y+=fontObject()->textHeight();
		 x+=fontObject()->textWidth(pString,i);
		 XDrawLine(display(),owner()->window(),textGC(),x,y,x+cw,y);
	       }
	    }
	 }
      }
   }
}

int MSMenuItem::computeYCoord(void)
{ return highlightThickness()+shadowThickness()+y_origin()+marginHeight(); }
int MSMenuItem::computeXCoord(void)
{ return highlightThickness()+shadowThickness()+x_origin()+marginWidth()+indent();} 
int MSMenuItem::drawWidth(void) const
{ return width()-2*(highlightThickness()+shadowThickness()+marginWidth());}
int MSMenuItem::drawHeight(void) const
{ return height()-2*(highlightThickness()+shadowThickness()+marginHeight()); }  

void MSMenuItem::select(void)   
{  drawShadow(); }
void MSMenuItem::unselect(void) 
{ undrawShadow(); }
void MSMenuItem::activate(void) 
{ 
  if (menu()!=0)
   {
     menu()->releaseGrab();
     if ( changeState() == MSTrue )  state( ( state() == MSTrue )? MSFalse:MSTrue ); 
     if (activateCallback(MSWidgetCallback::activate)==MSFalse) menu()->activate();
     else menu()->done();
   }
}

void MSMenuItem::label(const MSString& aString_)
{
  if(showState() == ShowPixmap) 
    showState(ShowBoth);
  else showState(ShowLabel);
  if (label()!=aString_)
   {
     _label=aString_;

     if (dynamic()==MSTrue) computeSize();
     else if (owner()->mapped()==MSTrue)
       {
	 drawBackground();
	 drawSymbol(); 
	 drawLabel();
	 if (showState()!=ShowLabel)drawPixmap();
	 (armed()==MSTrue)?drawSunken():drawRaised();
       }
   }
}

void MSMenuItem::naturalSize(void) 
{ computeSize(); }

void MSMenuItem::computeSize(void)
{
  int oldW=width();
  int oldH=height();
  int widthOffset=(highlightThickness()+shadowThickness()+marginWidth())<<1;
  int heightOffset=(highlightThickness()+shadowThickness()+marginHeight())<<1;
  
  int pixmapW=pixmapWidth();
  int pixmapH=pixmapHeight();
  int labelW=labelWidth();
  int labelH=labelHeight();
  
  int w,h;
  
  h=(pixmapH>labelH?pixmapH:labelH)+heightOffset;
  w=indent()+widthOffset;
/*
  if( showState() == ShowBoth)  if both are shown the indentation has to include the pixmapWidth
    w += labelW;
  else  
    w += ((showState() == ShowPixmap)? pixmapW:labelW);
*/
  if( showState() == ShowPixmap)  w += pixmapW;
  else w += labelW;
  
  if(w==oldW&&h==oldH) redraw();
  else resize(w,h);
}

int MSMenuItem::computeIndentation(void) 
{ return ( showState() == ShowBoth )? pixmapWidth()+spacing():0; } 

MSWidgetVector MSMenuItem::children(void)
{
  MSWidgetVector vector;
  if (cascadedMenu()!=0) vector.append(cascadedMenu());
  return vector;
}

GC MSMenuItem::pixmapGC(void)
{
  if (_pixmapGC==0)
   {
     XGCValues values;
     _pixmapGC=XCreateGC(display(),owner()->window(),0,&values);
   }
  return _pixmapGC;
}


void MSMenuItem::showState(MSMenuItem::ShowState showState_)
{
  if (_showState!=showState_)
    {
      _showState=showState_;
      if (dynamic()==MSTrue) computeSize();
      else redraw();
    }
}

int MSMenuItem::labelWidth(void)
//{ return showState() != ShowPixmap?fontObject()->textWidth(label(),label().length()):0; }
{ return fontObject()->textWidth(label(),label().length()); }

int MSMenuItem::labelHeight(void)
//{ return showState() != ShowPixmap?fontObject()->textHeight():0; }
{ return fontObject()->textHeight(); }

int MSMenuItem::pixmapWidth(void)
//{ return showState()!=ShowLabel&&currentPixmap()!=0?currentPixmap()->width():0; }
{ return currentPixmap()!=0? currentPixmap()->width():0; }

int MSMenuItem::pixmapHeight(void) 
//{ return showState()!=ShowLabel&&currentPixmap()!=0?currentPixmap()->height():0; }
{ return currentPixmap()!=0? currentPixmap()->height():0; }

const MSPixmap* MSMenuItem::currentPixmap(void) const
{ 
  if (sensitive()==MSTrue) return pixmap();
  else return insensitivePixmap();
}

void MSMenuItem::set(MSAttrValueList& avList_)
{
  MSWidgetOutput::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="label") label(avList_[i].value()),index<<i;  
     else if (avList_[i].attribute()=="mnemonic")
         mnemonic(avList_[i].value()[(unsigned int)0]),index<<i;
     else if (avList_[i].attribute()=="marginWidth")
         marginWidth(avList_[i].value().asInt()),index<<i;
     else if (avList_[i].attribute()=="marginHeight")
         marginHeight(avList_[i].value().asInt()),index<<i;
     else if (avList_[i].attribute()=="changeState")
         changeState(avList_[i].value().asBoolean()),index<<i;
   }
  avList_.remove(index);
}

MSAttrValueList& MSMenuItem::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("label",label(), MSAttrValue::String);
  avList_<<MSAttrValue("mnemonic",MSString(mnemonic()),MSAttrValue::Char);
  avList_<<MSAttrValue("marginWidth",MSString(marginWidth()));
  avList_<<MSAttrValue("marginHeight",MSString(marginHeight()));
  avList_<<MSAttrValue("activate","",MSAttrValue::Callback);

  MSStringVector aStringVector("MSTrue\nMSFalse");
  avList_<<MSAttrValue("changeState",
		       (changeState()==MSTrue?"MSTrue":"MSFalse"),
		       aStringVector);

  return MSWidgetOutput::get(avList_);
}



// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################

MSMenu* MSMenuItem::cascadedMenu(void)             { return 0; }
const MSMenu* MSMenuItem::cascadedMenu(void) const { return 0; }

void MSMenuItem::radioDisarm(void) {}
void MSMenuItem::configure(void) {}
void MSMenuItem::focusIn(void)  {}
void MSMenuItem::focusOut(void) {}
void MSMenuItem::drawSymbol(void) {}
void MSMenuItem::grab(void) {}
void MSMenuItem::arm(void) {}
void MSMenuItem::disarm(void) {}

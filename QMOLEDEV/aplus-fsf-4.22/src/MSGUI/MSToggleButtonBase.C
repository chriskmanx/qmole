///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSToggleButtonBase.H>
#include <MSGUI/MSGC.H>

static const int MSToggleButtonBaseDefaultSpacing=4;
static const int MSToggleButtonBaseDefaultShadowThickness=2;

MSToggleButtonBase::MSToggleButtonBase(MSWidget *owner_,const char *label_,const MSSymbol& tag_)
: MSActionButton(owner_,label_,tag_)
{ init(); }

MSToggleButtonBase::MSToggleButtonBase(MSWidget *owner_,const MSStringVector& label_,
			       const MSSymbol& tag_) : 
MSActionButton(owner_,label_,tag_)
{ init(); }

MSToggleButtonBase::MSToggleButtonBase(MSWidget *owner_,const MSPixmap &pixmap_,
			       const MSPixmap &insensitivePixmap_,
			       const MSPixmap &armedPixmap_,const MSSymbol& tag_) :
MSActionButton(owner_,pixmap_,insensitivePixmap_,armedPixmap_,tag_)
{ init(); }

MSToggleButtonBase::MSToggleButtonBase(MSWidget *owner_,const char *label_,int integerTag_) :
MSActionButton(owner_,label_,integerTag_)
{ init(); }

MSToggleButtonBase::MSToggleButtonBase(MSWidget *owner_,const MSStringVector& label_,int integerTag_) :
MSActionButton(owner_,label_,integerTag_)
{ init(); }

MSToggleButtonBase::MSToggleButtonBase(MSWidget *owner_,const MSPixmap &pixmap_,
			       const MSPixmap &insensitivePixmap_,
			       const MSPixmap &armedPixmap_,int integerTag_) :
MSActionButton(owner_,pixmap_,insensitivePixmap_,armedPixmap_,integerTag_)
{ init(); }

MSToggleButtonBase::~MSToggleButtonBase() 
{}

void MSToggleButtonBase::init(void)
{
  _spacing=MSToggleButtonBaseDefaultSpacing;
  _toggleShadowThickness=MSToggleButtonBaseDefaultShadowThickness;
  _selectColor=selectShadowColor();
  _alignment=(MSAlignment)MSLeft;
  createGCs();
}

void MSToggleButtonBase::createGCs(void)
{
  XGCValues gcvalues;
  unsigned long valueMask=GCForeground|GCBackground;
  gcvalues.foreground=selectColor();   
  gcvalues.background=background();   
  _selectMSGC.setGCValues(server(), MSTrue,&gcvalues,valueMask);
}

void MSToggleButtonBase::toggleShadowThickness(int thickness_) 
{
  if (toggleShadowThickness()!=thickness_)
   {
     _toggleShadowThickness=thickness_;
     computeSize(); 
   }
}

void MSToggleButtonBase::selectColor(const char *color_) 
{ selectColor(server()->pixel(color_)); }
void MSToggleButtonBase::selectColor(unsigned long pixel_)
{ 
  if (selectColor()!=pixel_)
   { 
     _selectColor=pixel_; 
     selectMSGC().foreground(pixel_);
     redraw(); 
   }
}
void MSToggleButtonBase::redraw(void)
{
  if (owner()->mapped()==MSTrue)
   {
     MSWidgetCommon::drawBackground();
     if(highlighted()==MSTrue) drawHighlight();
     else undrawHighlight();
     drawSymbol();
     if (pixmap()==0) drawLabel();
     else drawPixmap();
   }
}

int MSToggleButtonBase::computePixmapXCoord(const MSPixmap *pmap_)
{
  int offset=highlightThickness()+shadowThickness()+margin();
  int r;
  if (alignment()&MSLeft) r=offset+textHeight()+spacing();
  else if (alignment()&MSRight) r=width()-offset-pmap_->width();
  else
   { 
     int leftOffset=highlightThickness()+shadowThickness()+margin()+textHeight()+spacing();
     r=(width()-leftOffset-pmap_->width())/2+leftOffset;
   }
  return r;
}

int MSToggleButtonBase::computePixmapYCoord(const MSPixmap *pmap_)
{
  int offset=highlightThickness()+shadowThickness()+margin();
  int r;
  if (alignment()&MSTop) r=offset;
  else if (alignment()&MSBottom) r=height()-offset-pmap_->height();
  else r=(height()-pmap_->height())/2;
  return r;
}

void MSToggleButtonBase::configure(void)
{
  int Xoffset=highlightThickness()+shadowThickness()+margin()+textHeight()+spacing();
  int Yoffset=highlightThickness()+shadowThickness()+margin();
  XRectangle clipRect[1];
  clipRect[0].x=0;
  clipRect[0].y=0;     
  clipRect[0].width=drawWidth();
  clipRect[0].height=drawHeight();
  XSetClipRectangles(display(),textGC(),Xoffset,Yoffset,&clipRect[0],1,Unsorted);
  redraw();
}

void MSToggleButtonBase::computeSize(void)
{
  int oldW=width();
  int oldH=height();
  int newW,newH;
  int offset =(highlightThickness()+shadowThickness()+margin())<<1;
  if (pixmap()!=0)
   {
     newW=pixmap()->width()+textHeight()+spacing()+offset;
     newH=pixmap()->height()+offset;
   }
  else
   {
     newW=maxPixelWidth()+textHeight()+spacing()+offset;
     newH=rows()*textHeight()+offset;
   }
  if (oldW==newW&&oldH==newH) redraw();
  else resize(newW,newH);
}

int MSToggleButtonBase::computeXCoord(int /*row_*/,int col_,const char *pString_,int len_)
{
  int offset=highlightThickness()+shadowThickness()+margin();
  int leftOffset=offset+textHeight()+spacing();
  int r=0;
  if (pString_!=0)
   {
     if (alignment()&MSLeft) r=leftOffset+textWidth(pString_,col_);
     else if (alignment()&MSRight) 
      {
        int xs=width()-offset-textWidth(pString_,len_);	
	r=xs+textWidth(pString_,col_);
      }
     else 
      {
	int delta=drawWidth()-textWidth(pString_,len_)-leftOffset;
	delta=(delta>0)?delta>>1:0;
	r=leftOffset+delta+textWidth(pString_,col_);
      }
   }
  return r;
}

void MSToggleButtonBase::button1Press(const XEvent *)
{ (armed()==MSFalse)?arm():disarm(); }

void MSToggleButtonBase::key(KeySym keysym_,unsigned int,const char *)
{
  if (keysym_==XK_Return) { (armed()==MSFalse)?arm():disarm(); }
  else if (keysym_==XK_Up) up();
  else if (keysym_==XK_Down) down();
  else if (keysym_==XK_Left) left();
  else if (keysym_==XK_Right) right();
}


void MSToggleButtonBase::arm(void)    
{ setArmState(); }
void MSToggleButtonBase::disarm(void) 
{ setDisarmState(); }

void MSToggleButtonBase::set(MSAttrValueList& avList_)
{
  MSActionButton::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if(avList_[i].attribute()=="selectColor")
      {
        if(avList_[i].value().length()==0) selectColor(selectShadowColor());
        else selectColor(avList_[i].value());
        index<<i;
      }
     else if(avList_[i].attribute()=="toggleShadowThickness")
        toggleShadowThickness(avList_[i].value().asInt()),index<<i;
   }
  avList_.remove(index);
}

 
MSAttrValueList& MSToggleButtonBase::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("selectColor", server()->colorName(selectColor()),MSAttrValue::Color);
  avList_<<MSAttrValue("toggleShadowThickness", MSString(toggleShadowThickness()));
  MSActionButton::get(avList_);
  //We need to remove "activate" callback since it is not supported by us or our subclasses
  MSIndexVector index;
  for(int i=0; i<avList_.length();i++)
   {
     if(avList_[i].valueType()==MSAttrValue::Callback && avList_[i].attribute()=="activate")
      {
        index<<i;
        break;
      }
   }
  avList_.remove(index);
  return avList_;
}

  

// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################

void MSToggleButtonBase::activate(void) {}
void MSToggleButtonBase::button1Release(const XEvent *) {}
void MSToggleButtonBase::motionNotify(const XEvent *) {}


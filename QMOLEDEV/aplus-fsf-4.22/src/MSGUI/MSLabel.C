///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSLabel.H>
#include <MSGUI/MSToolTip.H>
#include <MSTypes/MSMessageLog.H>

static const unsigned long MSLabelEventMask=(ExposureMask);
static const int MSLabelDefaultHighlightThickness=0;
static const int MSLabelDefaultShadowThickness=0;
static const int MSLabelDefaultMargin=2;
static const char *MSLabelDefaultPixmap="LabelDefaultPixmap";

MSLabel::MSLabel(MSWidget *owner_,const char *label_) 
: MSPrimitiveText(owner_)
{ 
  init();
  internalCouple(new MSStringVector(label_));
  _numberOfRows=numRows();
}
MSLabel::MSLabel(MSWidget *owner_,const MSStringVector& label_) 
: MSPrimitiveText(owner_)
{ 
  init();
  internalCouple(new MSStringVector(label_));
  _numberOfRows=numRows();
}
MSLabel::MSLabel(MSWidget *owner_,MSStringVector& label_) 
: MSPrimitiveText(owner_)
{ 
  init();
  model(label_);
  _numberOfRows=numRows();
}

MSLabel::MSLabel(MSWidget *owner_,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_)
: MSPrimitiveText(owner_)
{ 
  init();
  _numberOfRows=numRows();
  if (pixmap_.server()==server()) _pixmap=new MSPixmap(pixmap_);
  else
   {
     MSMessageLog::warningMessage("Warning - Pixmap supplied for MSLabel is invalid, using default");
     createDefaultPixmap(pixmap_.width(),pixmap_.height(),
			 pixmap_.foreground(),pixmap_.background());
   }
  if (insensitivePixmap_.server()==server()) 
   {
     _insensitivePixmap=new MSPixmap(insensitivePixmap_);
   }
  else
   {
     MSMessageLog::warningMessage("Warning - Insensitive Pixmap supplied for MSLabel is invalid, using default");
     createDefaultInsensitivePixmap(insensitivePixmap_.width(),
				    insensitivePixmap_.height(),
				    insensitivePixmap_.foreground(),
				    insensitivePixmap_.background());
   }
}


MSLabel::~MSLabel(void) 
{ 
  freeze();
  if (pixmap()!=0) delete _pixmap;
  if (_pixmapGC!=0) XFreeGC(display(),_pixmapGC);
  if (insensitivePixmap()!=0) delete _insensitivePixmap;
  MSHashTable *hashTbl=server()->toolTipHashTable();
  MSStringVector *tip=(MSStringVector *)hashTbl->lookup((unsigned long)this);
  if ((unsigned long)tip!=hashTbl->notFound()) delete tip;
  hashTbl->remove((unsigned long)this);
  MSToolTip *toolTip=server()->toolTip();
  if (toolTip->displayFor()==this) toolTip->unmap();
}

void MSLabel::init(void)
{
  freeze();
  _resetClipRegion=MSFalse;
  _displayToolTip=MSFalse;
  _pixmapGC=0;
  _pixmap=0;
  _insensitivePixmap=0;
  _highlightThickness=MSLabelDefaultHighlightThickness;
  _shadowThickness=MSLabelDefaultShadowThickness;
  _alignment=MSCenter;
  _margin=MSLabelDefaultMargin;
  dynamic(MSFalse);
  shadowStyle(MSEtchedOut);
  acceptFocus(MSFalse);
}

void MSLabel::createDefaultPixmap(int w_,int h_,unsigned long fg_,unsigned long bg_)
{ 
  _pixmap=new MSPixmap(server(),MSLabelDefaultPixmap,w_,h_,fg_,bg_); 
  GC gc=XCreateGC(display(),window(),0,0);
  XSetForeground(display(),gc,bg_);
  XFillRectangle(display(),pixmap()->pixmap(),gc,0,0,w_,h_);
  XFreeGC(display(),gc);
}

void MSLabel::createDefaultInsensitivePixmap(int w_,int h_,unsigned long fg_,unsigned long bg_)
{ 
  _insensitivePixmap=new MSPixmap(server(),MSLabelDefaultPixmap,w_,h_,fg_,bg_); 
  GC gc=XCreateGC(display(),window(),0,0);
  XSetForeground(display(),gc,bg_);
  XFillRectangle(display(),insensitivePixmap()->pixmap(),gc,0,0,w_,h_);
  XFreeGC(display(),gc);
}

// virtual methods that subclassers can override for different model types
int MSLabel::numRows(void) const
{ return (_model!=0)?label().length():0; }
int MSLabel::numColumns(void) const
{ return (_model!=0)?label().maxLength():0; }
int MSLabel::numColumns(int row_) const
{ return (row_<rows())?label()(row_).length():0; }
const char *MSLabel::string(int row_) const
{ return (row_<rows())?label()(row_).string():0; }

const char *MSLabel::formatOutput(MSString &buffer_,int row_)
{
  if (row_<rows()) buffer_=label()(row_);
  return buffer_.string();
}

void MSLabel::updateData(void)
{
  if (MSView::model()==0) internalCouple(new MSStringVector());
  
  if (dynamic()==MSTrue&&frozen()==MSFalse)
   {
     freeze();
     naturalSize();
     unfreeze();
   }
  redraw();
}

void MSLabel::update(const MSIndexVector& index_)
{
  if (MSView::model()!=0)
   {
     if (index_.length()==0) 
      {
	 if (dynamic()==MSTrue) computeSize();
	 else redraw();
      }
     else if (frozen()==MSFalse&&mapped()==MSTrue)
      {
	if (numberOfRows()!=numRows())
	 {
	   if (dynamic()==MSTrue) computeSize();
	   else redraw();
	 }
        else
	 {
	   if (dynamic()==MSTrue) computeSize();
	   else
	    {
	      for (unsigned i=0;i<index_.length();i++) 
	       { 
		 clearRow(index_(i));
		 drawRow(index_(i)); 
	       }
	    }
	   XFlush(display());
	 }
      }
     _numberOfRows=numRows();
   }
}

void MSLabel::model(MSStringVector& model_)
{ couple(&model_); }

void MSLabel::label(const MSStringVector& label_)
{ 
  if (MSView::model()!=0) label()=label_; 
  else internalCouple(new MSStringVector(label_));
  _numberOfRows=numRows();
}

void MSLabel::margin(int margin_) 
{  
  if (margin()!=margin_) 
   {
     _margin=margin_;
     if (dynamic()==MSTrue) computeSize();
     else redraw();
   }
}

void MSLabel::firstMapNotify(void)
{
  freeze();
  computeSize();
  unfreeze();
}

int MSLabel::drawWidth(void) const
{ return width()-2*(highlightThickness()+shadowThickness()+margin());}  
int MSLabel::drawHeight(void) const
{ return height()-2*(highlightThickness()+shadowThickness()+margin());}  

void MSLabel::naturalSize(void)
{ computeSize(); }


void MSLabel::pixmap(const MSPixmap &pixmap_)
{
  MSPixmap *tmp=_pixmap;
  if (pixmap_.server()==server()) _pixmap=new MSPixmap(pixmap_);
  else
   {
     MSMessageLog::warningMessage("Warning - Pixmap supplied for MSLabel is invalid, using default");
     createDefaultPixmap(pixmap_.width(),pixmap_.height(),
			 pixmap_.foreground(),pixmap_.background());
   }
  if (tmp!=0) delete tmp;
  updatePixmap();
}

void MSLabel::insensitivePixmap(const MSPixmap &insensitivePixmap_)
{
  MSPixmap *tmp=_insensitivePixmap;
  if (insensitivePixmap_.server()==server())
   {
     _insensitivePixmap=new MSPixmap(insensitivePixmap_);
   }
  else
   {
     MSMessageLog::warningMessage("Warning - Insensitive Pixmap supplied for MSLabel is invalid, using default");
     createDefaultInsensitivePixmap(insensitivePixmap_.width(),insensitivePixmap_.height(),
				    insensitivePixmap_.foreground(),insensitivePixmap_.background());
   }
  if (tmp!=0) delete tmp;
  updatePixmap();
}


void MSLabel::computeSize(void)
{
  int oldW=width();
  int oldH=height();
  int newW,newH;
  int offset =(highlightThickness()+shadowThickness()+margin())<<1;
  // If pixmap is used, always size it against the sensitive Pixmap,
  // even though sensitive and non-sensitive pixmap might have different geometry
  if (pixmap()!=0)
   {
     newW=pixmap()->width()+offset;
     newH=pixmap()->height()+offset;
   }
  else
   {
     newW=maxPixelWidth()+offset;
     newH=numRows()*textHeight()+offset;
   }
  if (oldW==newW&&oldH==newH) redraw();
  else resize(newW,newH);
}

void MSLabel::setClipRegion(void)
{
  int offset=highlightThickness()+shadowThickness()+margin();
  XRectangle clipRect[1];
  clipRect[0].x=0;
  clipRect[0].y=0;     
  clipRect[0].width=drawWidth();
  clipRect[0].height=drawHeight();
  XSetClipRectangles(display(),textGC(),offset,offset,&clipRect[0],1,Unsorted);
}

GC MSLabel::pixmapGC(void)
{
  if (_pixmapGC==0)
   {
     XGCValues values;
     _pixmapGC=XCreateGC(display(),window(),0,&values);
   }
  return _pixmapGC;
}

void MSLabel::configure(void)
{
  setClipRegion();
}

void MSLabel::redraw(void)
{
  if (frozen()==MSFalse&&mapped()==MSTrue)
   {
     drawBackground();
     if (pixmap()==0) drawLabel();
     else drawPixmap();
     drawShadow();
   }
}

void MSLabel::drawPixmap(void)
{
  if (mapped()==MSTrue)
   {
     const MSPixmap *pmap;
     if (sensitive()==MSTrue) pmap=pixmap();
     else pmap=insensitivePixmap();
     if (pmap!=0)
      {
	int startx=computePixmapXCoord(pmap);
	int starty=computePixmapYCoord(pmap);
        GC gc=pixmapGC();
	XSetForeground(display(),gc,foreground());
	XSetBackground(display(),gc,background());
	copyPixmap(display(),*pmap,window(),gc,startx,starty);
      }
   }
}

int MSLabel::computePixmapXCoord(const MSPixmap *pmap_)
{
  int offset=highlightThickness()+shadowThickness()+margin();
  int r=0;
  int pwidth=pmap_->width();
  if (alignment()&MSLeft) r=offset;
  else if (alignment()&MSRight) r=width()-offset-pwidth;
  else r=(width()-pwidth)/2;
  return r;
}

int MSLabel::computePixmapYCoord(const MSPixmap *pmap_)
{
  int offset=highlightThickness()+shadowThickness()+margin();
  int r=0;
  int pheight=pmap_->height();
  if (alignment()&MSTop) r=offset;
  else if (alignment()&MSBottom) r=height()-offset-pheight;
  else r=(height()-pheight)/2;
  return r;
}

void MSLabel::drawLabel(void)
{
  if (outputMode()==DrawPPM) resetClipRegion(MSTrue);
  if (resetClipRegion()==MSTrue)
   {
     setClipRegion();
     if (outputMode()==Draw) resetClipRegion(MSFalse);
   }
  drawRows(0,numRows()-1);
}
void MSLabel::drawRow(int row_)
{ drawRows(row_,row_); }
void MSLabel::drawRows(int rs_,int re_)
{
  if (frozen()==MSFalse&&mapped()==MSTrue)
   {
     int nr=numRows();
     if (nr>0)
      {
	MSString buffer;
	for (int i=rs_;i<=re_&&i<nr;i++)
	 {
           const char *pString=formatOutput(buffer.removeAll(),i);
	   int len=numColumns(i);
	   drawRow(i,0,pString,len);
	 }
      }
   }
}

void MSLabel::drawRow(int row_,int column_,const char *pString_,int len_)
{
  if (pString_!=0&&len_>0)
   {
     int y=computeYCoord(row_);
     int x=computeXCoord(row_,column_>=0?column_:0,pString_,len_);
     XDrawString(display(),window(),textGC(),textFontStruct(),x,y+textAscent(),pString_,len_);
   }
}

void MSLabel::clearRow(int row_)
{
  if (row_<numRows())
   {
     int y=computeYCoord(row_);
     int x=highlightThickness()+shadowThickness()+margin();
     XFillRectangle(display(),window(),backgroundShadowGC(),x,y,drawWidth(),textHeight());
   }
}

int MSLabel::computeYCoord(int row_)
{
  int offset=highlightThickness()+shadowThickness()+margin();
  int r;
  
  if (alignment()&MSTop) r=offset+row_*textHeight();
  else if (alignment()&MSBottom) r=height()-offset-(numRows()-row_)*textHeight();
  else 
   {
     int delta=drawHeight()-numRows()*textHeight();
     delta=(delta>0)?delta>>1:0;
     r=offset+row_*textHeight()+delta;
   }
  return r;
}

int MSLabel::computeXCoord(int,int column_,const char *pString_,int len_)
{
  int offset=highlightThickness()+shadowThickness()+margin();
  int r=0;
  if (pString_!=0)
   {
     if (alignment()&MSLeft) r=offset+textWidth(pString_,column_);
     else if (alignment()&MSRight) 
      {
        int xs=width()-offset-textWidth(pString_,len_);	
	r=xs+textWidth(pString_,column_);
      }
     else 
      {
	int delta=drawWidth()-textWidth(pString_,len_);
	delta=(delta>0)?delta>>1:0;
	r=offset+delta+textWidth(pString_,column_);
      }
   }
  return r;
}

void MSLabel::updateBackground(unsigned long oldbg_)
{ 
  MSPrimitiveText::updateBackground(oldbg_);
  redraw();
}

void MSLabel::updateForeground(unsigned long oldfg_)
{ 
  MSPrimitiveText::updateForeground(oldfg_);
  redraw();
}

void MSLabel::updateFont(Font oldfid_) 
{
  MSPrimitiveText::updateFont(oldfid_); 
  if (dynamic()==MSTrue) computeSize();
  else redraw(); 
}

void MSLabel::updateSensitivity(void)
{ 
  MSPrimitiveText::updateSensitivity();
  redraw();
}

void MSLabel::updatePixmap(void)
{
  // Make sure both sensitive and insensitive pixmap is defined
  // we're not wasting X Resource since MSPixmap is reference counted
  if (insensitivePixmap()==0&&pixmap()!=0) _insensitivePixmap=new MSPixmap(*pixmap());
  if (pixmap()==0&&insensitivePixmap()!=0) _pixmap=new MSPixmap(*insensitivePixmap());
  if (firstMap()==MSTrue)
   {
     if (dynamic()==MSTrue) computeSize();
     else redraw();
   }
}

int MSLabel::maxPixelWidth(void)
{
  unsigned n=numRows();
  unsigned longest=0,len=0;
  MSString buffer;
  for (unsigned i=0;i<n;i++)
   {
     len=textWidth(formatOutput(buffer.removeAll(),i),numColumns(i));
     if (len>longest) longest=len;
   }
  return longest;
}

void MSLabel::displayToolTip(MSBoolean displayToolTip_)
{
  if (_displayToolTip!=displayToolTip_)
   {
     _displayToolTip=displayToolTip_;
     unsigned long mask;
     if (displayToolTip()==MSTrue) mask=eventMask()|EnterWindowMask|LeaveWindowMask;
     else mask=(eventMask()&~EnterWindowMask)&~LeaveWindowMask;
     selectInput(mask);
   }
}

void MSLabel::enterNotify(const XEvent *)
{
  MSToolTip *toolTipWidget=server()->toolTip();
  toolTipWidget->toolTip(tip(),this);
  int x,y;
  toolTipXY(x,y);
  toolTipWidget->moveTo(x,y);
  toolTipWidget->show();
}

void MSLabel::leaveNotify(const XEvent *)
{
  server()->toolTip()->unmap();
}

void MSLabel::toolTipXY(int &x,int &y)
{
  MSToolTip *toolTipWidget=server()->toolTip();
  int w=toolTipWidget->width();
  int h=toolTipWidget->height();
  int tailPosition=0;
  int rootx,rooty;
  rootXY(rootx,rooty);
  x=rootx+(width()/2);
  y=rooty+height();
  if (x+w>server()->width())
   {
     x=rootx+width()/2-w;
     tailPosition=MSToolTip::Right;
   }
  else tailPosition=MSToolTip::Left;
  if (y+h>server()->height())
   {
     y=rooty-h;
     tailPosition|=MSToolTip::Bottom;
   }
  else tailPosition|=MSToolTip::Top;
  toolTipWidget->tailPosition(tailPosition);
}


const MSStringVector &MSLabel::tip(void) const
{
  MSHashTable *hashTbl=server()->toolTipHashTable();
  MSStringVector *tip=(MSStringVector *)hashTbl->lookup((unsigned long)this);
  if ((unsigned long)tip==hashTbl->notFound()) return (*(MSStringVector *)hashTbl->notFound());
  else return *tip;
}

void MSLabel::toolTip(const MSStringVector &toolTip_)
{
  MSHashTable *hashTbl=server()->toolTipHashTable();
  MSStringVector *tip=(MSStringVector *)hashTbl->lookup((unsigned long)this);
  if ((unsigned long)tip==hashTbl->notFound())
   {
     hashTbl->add((unsigned long)this,new MSStringVector(toolTip_));
   }
  else
   {
     (*tip)=toolTip_;
   }
  if (displayToolTip()==MSTrue)
   {
     MSToolTip *toolTipWidget=server()->toolTip();
     if (toolTipWidget->mapped()==MSTrue&&toolTipWidget->displayFor()==this)
      {
	toolTipWidget->toolTip(toolTip_,this);
      }
   }
}

void MSLabel::set(MSAttrValueList& avList_)
{
  MSPrimitiveText::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="label") label(MSAttrValue::stringToStringVector(avList_[i].value())),index<<i;
     else if (avList_[i].attribute()=="alignment") alignment(MSAttrValue::stringToAlignment(avList_[i].value())),index<<i;
     else if (avList_[i].attribute()=="margin") margin(avList_[i].value().asInt()),index<<i;
     else if (avList_[i].attribute()=="toolTip") toolTip(MSAttrValue::stringToStringVector(avList_[i].value())),index<<i;
     else if (avList_[i].attribute()=="displayToolTip") displayToolTip(avList_[i].value().asBoolean()),index<<i;
   }
  avList_.remove(index);
}

MSAttrValueList& MSLabel::get(MSAttrValueList& avList_)
{
  MSStringVector aStringVector("MSFalse\nMSTrue");
  MSStringVector alignmentVector("MSNone\nMSCenter\nMSTop\nMSBottom\nMSLeft\nMSRight");
  avList_<<MSAttrValue("label",MSAttrValue::stringVectorToString(label()),MSAttrValue::String);
  avList_<<MSAttrValue("alignment",MSAttrValue::alignmentToString(alignment()),
                       alignmentVector, MSAttrValue::List);
  avList_<<MSAttrValue("margin",MSString(margin()));
  avList_<<MSAttrValue("toolTip",MSAttrValue::stringVectorToString(toolTip()),MSAttrValue::String);
  avList_<<MSAttrValue("displayToolTip",aStringVector(displayToolTip()),aStringVector);
  return MSPrimitiveText::get(avList_);
}


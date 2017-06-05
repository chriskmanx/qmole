///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSIcon.H>
#include <MSTypes/MSMessageLog.H>

const int MSIconDefaultHighlightThickness=1;
const int MSIconDefaultLabelSpacing=4;
const int MSIconMinimumWidth=4;
const int MSIconMinimumHeight=4;
const unsigned long MSIconEventMask=(ExposureMask|ButtonPressMask);

MSIcon::MSIcon(MSWidget *owner_,const char *label_,const MSPixmap &pixmap_,
	       const MSPixmap &insensitivePixmap_) :
MSLabel(owner_,label_)
{
  init(pixmap_,insensitivePixmap_);
}

MSIcon::MSIcon(MSWidget *owner_,const MSStringVector& label_,const MSPixmap &pixmap_,
	       const MSPixmap &insensitivePixmap_) :
MSLabel(owner_,label_)
{ 
  init(pixmap_,insensitivePixmap_);
}

MSIcon::MSIcon(MSWidget *owner_,MSStringVector& label_,const MSPixmap &pixmap_,
	       const MSPixmap &insensitivePixmap_) :
MSLabel(owner_,label_)
{ 
  init(pixmap_,insensitivePixmap_);
}

MSIcon::MSIcon(MSWidget *owner_,const char *label_) :
MSLabel(owner_,label_)
{ 
  init();
}

MSIcon::MSIcon(MSWidget *owner_,const MSStringVector& label_) :
MSLabel(owner_,label_)
{ 
  init();
}

MSIcon::MSIcon(MSWidget *owner_,MSStringVector& label_) :
MSLabel(owner_,label_)
{ 
  init();
}

MSIcon::MSIcon(MSWidget *owner_,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_) :
MSLabel(owner_)
{ 
  init(pixmap_,insensitivePixmap_);
}

MSIcon::~MSIcon(void)
{}

void MSIcon::init(void)
{
  _showPixmap=MSTrue;
  _showLabel=MSTrue;
  _labelAlignment=MSBottom;
  _labelSpacing=MSIconDefaultLabelSpacing;
  _highlightThickness=MSIconDefaultHighlightThickness;
  selectInput(MSIconEventMask);
}

void MSIcon::init(const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_)
{ 
  init();
  if (pixmap_.server()==server()) _pixmap=new MSPixmap(pixmap_);
  else
   {
     MSMessageLog::warningMessage("Warning : Pixmap supplied for MSIcon is invalid, using default");
     createDefaultPixmap(pixmap_.width(),pixmap_.height(),
			 pixmap_.foreground(),pixmap_.background());
   }
  if (insensitivePixmap_.server()==server()) 
  _insensitivePixmap=new MSPixmap(insensitivePixmap_);
  else
   {
     MSMessageLog::warningMessage("Warning : Insensitive Pixmap supplied for MSIcon is invalid, using default");
     createDefaultInsensitivePixmap(insensitivePixmap_.width(),
				    insensitivePixmap_.height(),
				    insensitivePixmap_.foreground(),
				    insensitivePixmap_.background());
   }
}

void MSIcon::computeSize(void)
{
  int oldW=width();
  int oldH=height();
  int offset=(highlightThickness()+shadowThickness()+margin())<<1;
  int pixmapW=pixmapWidth();
  int pixmapH=pixmapHeight();
  int labelW=labelWidth();
  int labelH=labelHeight();
  int labelSp=computeLabelSpacing();
  int w,h;
  if (labelAlignment()==MSTop||labelAlignment()==MSBottom)
   {
     w=(pixmapW>labelW?pixmapW:labelW)+offset;
     h=pixmapH+labelH+offset+labelSp;
   }
  else
   {
     w=pixmapW+labelW+offset+labelSp;
     h=(pixmapH>labelH?pixmapH:labelH)+offset;
   }
  w=w>0?w:MSIconMinimumWidth;
  h=h>0?h:MSIconMinimumHeight;
  if(w==oldW&&h==oldH) redraw();
  else resize(w,h);
}

void MSIcon::redraw(void)
{
  if (mapped()==MSTrue&&frozen()==MSFalse)
   {
     drawBackground();
     drawShadow();
     if (showPixmap()==MSTrue) drawPixmap();
     if (showLabel()==MSTrue) drawLabel();
     if (highlighted()==MSTrue) drawHighlight();
     else undrawHighlight();
   }
}

int MSIcon::computePixmapYCoord(const MSPixmap *pixmap_)
{
  int offset=highlightThickness()+shadowThickness()+margin();
  int labelH=labelHeight();
  int pixmapH=pixmap_->height();
  int labelSp=computeLabelSpacing();
  int r=0;
  if (alignment()&MSTop)
   {
     if (labelAlignment()==MSTop) r=offset+labelH+labelSp;
     else if (labelAlignment()==MSBottom) r=offset;
     else
      {
	if (labelH<pixmapH) r=offset;
	else r=offset+(labelH-pixmapH)/2;
      }
   }
  else if (alignment()&MSBottom)
   {
     if (labelAlignment()==MSTop) r=height()-pixmapH-offset;
     else if (labelAlignment()==MSBottom) r=height()-labelH-pixmapH-labelSp-offset;
     else
      {
	if (labelH<pixmapH) r=height()-pixmapH-offset;
	else r=height()-labelH-offset+(labelH-pixmapH)/2;
      }
   }
  else
   {
     if (labelAlignment()==MSTop) r=(height()-(pixmapH+labelH+labelSp))/2+labelH+labelSp;
     else if (labelAlignment()==MSBottom) r=(height()-(pixmapH+labelH+labelSp))/2;
     else r=(height()-pixmapH)/2;
   }
  return r;
}

int MSIcon::computeYCoord(int row_)
{
  int pixmapH=0;
  // Checking if _pixmap is equal to zero is good enough because the existence of
  // _pixmap implies the existence of _insensitivePixmap
  if (showPixmap()==MSTrue&&pixmap()!=0)
   {
     if (sensitive()==MSTrue) pixmapH=pixmap()->height();
     else pixmapH=insensitivePixmap()->height();
   }
  return computeYCoordinate(row_,pixmapH);
}

int MSIcon::computeXCoord(int row_,int column_,const char *string_,int len_)
{
  int pixmapW=0;
  // Checking if _pixmap is equal to zero is good enough because the existence of
  // _pixmap implies the existence of _insensitivePixmap
  if (showPixmap()==MSTrue&&pixmap()!=0)
   {
     if (sensitive()==MSTrue) pixmapW=pixmap()->width();
     else pixmapW=insensitivePixmap()->width();
   }
  return computeXCoordinate(row_,column_,pixmapW,string_,len_);
}


int MSIcon::computePixmapXCoord(const MSPixmap *pixmap_)
{
  int offset=highlightThickness()+shadowThickness()+margin();
  int labelW=labelWidth();
  int pixmapW=pixmap_->width();
  int labelSp=computeLabelSpacing();
  int r=0;
  if (alignment()&MSLeft)
   {
     if (labelAlignment()==MSLeft) r=offset+labelW+labelSp;
     else if (labelAlignment()==MSRight) r=offset;
     else
      {
	if (labelW<pixmapW) r=offset;
	else r=offset+(labelW-pixmapW)/2;
      }
   }
  else if (alignment()&MSRight)
   {
     if (labelAlignment()==MSLeft) r=width()-pixmapW-offset;
     else if (labelAlignment()==MSRight) r=width()-labelW-pixmapW-labelSp-offset;
     else
      {
	if (labelW<pixmapW) r=width()-pixmapW-offset;
	else r=width()-labelW-offset+(labelW-pixmapW)/2;
      }
   }
  else
   {
     if (labelAlignment()==MSLeft) r=(width()-(pixmapW+labelW+labelSp))/2+labelW+labelSp;
     else if (labelAlignment()==MSRight) r=(width()-(pixmapW+labelW+labelSp))/2;
     else r=(width()-pixmapW)/2;
   }
  return r;
}

int MSIcon::computeXCoordinate(int,int,int pixmapW_,const char *string_,int len_)
{
  int offset=highlightThickness()+shadowThickness()+margin();
  int r=0;
  int labelW=labelWidth();
  int textW=textWidth(string_,len_);
  int labelSp=computeLabelSpacing();
  if (alignment()&MSLeft)
   {
     if (labelAlignment()==MSLeft) r=offset+labelW-textW;
     else if (labelAlignment()==MSRight) r=offset+pixmapW_+labelSp;
     else
      {
	if (labelW<pixmapW_) r=offset+(pixmapW_-textW)/2;
	else r=offset+(labelW-textW)/2;
      }
   } 
  else if (alignment()&MSRight)
   {
     if (labelAlignment()==MSLeft) r=width()-pixmapW_-textW-labelSp-offset;
     else if (labelAlignment()==MSRight) r=width()-offset-labelW;
     else
      {
	if (labelW<pixmapW_) r=width()-offset-pixmapW_+(pixmapW_-textW)/2;
	else r=width()-offset-textW-(labelW-textW)/2;
      }
   }
  else
   {
     if (labelAlignment()==MSLeft) r=(width()-(pixmapW_+labelW+labelSp))/2+labelW-textW;
     else if (labelAlignment()==MSRight) r=(width()-(pixmapW_+labelW+labelSp))/2+pixmapW_+labelSp;
     else
      {
	if (textW<pixmapW_) r=(width()-pixmapW_)/2+(pixmapW_-textW)/2;
	else r=(width()-textW)/2;
      }
   }
  return r;
}

int MSIcon::computeYCoordinate(int row_,int pixmapH_)
{
  int offset=highlightThickness()+shadowThickness()+margin();
  int r=0;
  int labelH=labelHeight();
  int labelSp=computeLabelSpacing();
  if (alignment()&MSTop)
   {
     if (labelAlignment()==MSTop) r=offset+row_*textHeight();
     else if (labelAlignment()==MSBottom) r=offset+pixmapH_+labelSp+row_*textHeight();
     else
      {
	if (labelH<pixmapH_) r=offset+(pixmapH_-labelH)/2+row_*textHeight();
	else r=offset+row_*textHeight();
      }
   } 
  else if (alignment()&MSBottom)
   {
     if (labelAlignment()==MSTop) r=height()-pixmapH_-labelSp-offset-(rows()-row_)*textHeight();
     else if (labelAlignment()==MSBottom) r=height()-offset-(rows()-row_)*textHeight();
     else
      {
	if (labelH<pixmapH_) r=height()-(pixmapH_-labelH)/2-offset-(rows()-row_)*textHeight();
	else r=height()-offset-(rows()-row_)*textHeight();
      }
   }
  else
   {
     if (labelAlignment()==MSTop) r=(height()-(pixmapH_+labelH+labelSp))/2+row_*textHeight();
     else if (labelAlignment()==MSBottom) r=(height()-(pixmapH_+labelH+labelSp))/2+pixmapH_+labelSp+row_*textHeight();
     else
      {
	if (labelH<pixmapH_) r=(height()-pixmapH_)/2+(pixmapH_-labelH)/2+row_*textHeight();
	else r=(height()-labelH)/2+row_*textHeight();
      }
   }
  return r;
}

void MSIcon::showPixmap(MSBoolean showPixmap_)
{
  if (_showPixmap!=showPixmap_)
   {
     _showPixmap=showPixmap_;
     if (dynamic()==MSTrue) computeSize();
     else redraw();
   }
}

void MSIcon::showLabel(MSBoolean showLabel_)
{
  if (_showLabel!=showLabel_)
   {
     _showLabel=showLabel_;
     if (dynamic()==MSTrue) computeSize();
     else redraw();
   }
}

void MSIcon::labelSpacing(int labelSpacing_)
{
  if (_labelSpacing!=labelSpacing_)
   {
     _labelSpacing=labelSpacing_;
     if (dynamic()==MSTrue) computeSize();
     else redraw();
   }
}

void MSIcon::labelAlignment(MSAlignment labelAlignment_)
{
  if (_labelAlignment!=labelAlignment_)
   {
     _labelAlignment=labelAlignment_;
     if (dynamic()==MSTrue) computeSize();
     else redraw();
   }
}

void MSIcon::button1Press(const XEvent *event_)
{
  MSBoolean focus=MSTrue;
  if (acceptFocus()==MSTrue) focus=traverseFocus(this);
  if (focus==MSTrue&&doubleClick().isDoubleClick(event_)==MSTrue) doubleClickNotify();
  else buttonSelectionNotify();
}

void MSIcon::doubleClickNotify(void)
{
  activateCallback(MSWidgetCallback::doubleclick);
}

void MSIcon::buttonSelectionNotify(void)
{
  activateCallback(MSWidgetCallback::selection);
}

int MSIcon::computeLabelSpacing(void)
{
  if (showLabel()==MSFalse) return 0;
  else if (showPixmap()==MSFalse) return 0;
  else if (pixmap()==0) return 0;
  else return labelSpacing();
}

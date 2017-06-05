///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSPane.H>
#include <MSGUI/MSDisplayCursor.H>

static const int MSSashMSDefaultWidth=8;
static const int MSSashMSDefaultHeight=8;
static const int MSSashDefaultShadowThickness=2;
static const int MSSashDefaultIndent=10;
static const int MSPaneDefaultRowSpacing=8;
//static const int MSPaneDefaultColumnSpacing=8;

static const unsigned long MSSashEventMask=(ExposureMask|ButtonPressMask|ButtonReleaseMask);

MSSash::MSSash(MSPane *owner_) : MSWidgetCommon(owner_)
{
  _highlightThickness=0;
  _shadowThickness=0;
  _row=0,_column=0,_min=0,_max=0;
  acceptFocus(MSFalse);

  unsigned long whitePixel=server()->pixel("white");
  unsigned long blackPixel=server()->pixel("black");

  _crossCursor=new MSDisplayCursor(server(),XC_tcross,blackPixel,whitePixel);

  XSetWindowAttributes attrib;
  attrib.cursor=_crossCursor->cursor();
  XChangeWindowAttributes(display(),window(),CWCursor,&attrib);

  XGCValues values;
  values.foreground=whitePixel^background();
  values.background=blackPixel;
  values.line_width=2;
  values.function=GXxor;
  values.subwindow_mode=IncludeInferiors;
  _drawGC.setGCValues(server(),MSTrue,&values,
                      GCForeground|GCBackground|GCLineWidth|GCFunction|GCSubwindowMode);
  selectInput(MSSashEventMask);
}

MSSash::~MSSash(void) 
{
  delete _crossCursor;
}

GC MSSash::gc(void) const
{ return _drawGC.gc(); }

void MSSash::drawSash(void) {}
void MSSash::drawSeparator(void) {}
void MSSash::size(int) {}
void MSSash::moveSash(int) {}

void MSSash::redraw(void)
{
  if (mapped()==MSTrue)
   {
     drawBackground();
     drawSeparator();
     drawSash();
   }
}

void MSSash::updateBackground(unsigned long oldbg_) 
{ 
  MSWidgetCommon::updateBackground(oldbg_);
  redraw(); 
}

MSHSash::MSHSash(MSPane *owner_) : MSSash(owner_)
{ height(MSSashMSDefaultHeight); }

MSHSash::~MSHSash(void) {}

void MSHSash::size(int size_)
{ width(size_); }
void MSHSash::moveSash(int delta_)
{ moveTo(x_origin(),y_origin()+delta_); }

void MSHSash::drawSeparator(void)
{
  if (mapped()==MSTrue)
   {
     int offset=highlightThickness()+shadowThickness();
     int x=offset;
     int thickness=MSSashDefaultShadowThickness;
     if (thickness>1)
      {
	int y=height()>>1;
	XFillRectangle(display(),window(),bottomShadowGC(),
		       x,y,width()-2*offset,1);
	XFillRectangle(display(),window(),topShadowGC(),
		       x,y+1,width()-2*offset,1);
      }
     else 
      {
	XFillRectangle(display(),window(),backgroundShadowGC(),
		       x,height()>>1,width()-2*offset,1);
      }
   }
}

void MSHSash::drawSash(void)
{
  if (mapped()==MSTrue)
   {
     int offset=highlightThickness()+shadowThickness();
     int sht=MSSashDefaultShadowThickness;
     
     MSRect aRect(width()-offset-MSSashDefaultIndent,offset,
		  MSSashMSDefaultWidth,MSSashMSDefaultHeight);
     drawBevel(aRect,MSRaised,sht);
     XFillRectangle(display(),window(),backgroundShadowGC(),
		    aRect.x()+sht,aRect.y()+sht,
		    aRect.width()-2*sht,aRect.height()-2*sht);
   }
}

void MSHSash::button1Press(const XEvent *ev_)
{
  int yoffset=height()>>1;
  int starty=y()+yoffset;
  int delta=yoffset-ev_->xbutton.y;
  int ymin=min();
  int ymax=max()-yoffset;
  unsigned int mask=Button1Mask;
  unsigned int keys=mask;
  int ix=0,iy=0;
  int rx=0,ry=0;
  int oldY=y_origin();
  int lastY=starty;
  Window root,child;

  server()->grabPointer(window(),False,ButtonPressMask|ButtonReleaseMask,
			GrabModeAsync,GrabModeAsync,None,
			_crossCursor->cursor(),ev_->xbutton.time);

  XDrawLine(display(),owner()->window(),gc(),0,lastY,owner()->width(),lastY);
  int sameScreen=XQueryPointer(display(),owner()->window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
  while (keys&mask)
   {
     if (sameScreen==True)
      {
	iy+=delta;
	if (iy<ymin) iy=ymin;
	else if (iy>ymax) iy=ymax;
	if (lastY!=iy)
	 {
	   XDrawLine(display(),owner()->window(),gc(),0,lastY,owner()->width(),lastY);
	   lastY=iy;
	   XDrawLine(display(),owner()->window(),gc(),0,lastY,owner()->width(),lastY);
	 }
      }
     sameScreen=XQueryPointer(display(),owner()->window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
   }
  XDrawLine(display(),owner()->window(),gc(),0,lastY,owner()->width(),lastY);
  moveTo(x_origin(),lastY-yoffset);

  int deltaY=oldY-y_origin();
  MSPane *p=(MSPane *)owner(); 
  p->adjustRowHeight(row(),deltaY);
  server()->ungrabPointer(window(),CurrentTime);
}

MSVSash::MSVSash(MSPane *owner_) : MSSash(owner_)
{ width(MSSashMSDefaultWidth); }

MSVSash::~MSVSash(void) {}

void MSVSash::size(int size_)
{ height(size_); }
void MSVSash::moveSash(int delta_)
{ moveTo(x_origin()+delta_,y_origin()); }

void MSVSash::drawSeparator(void)
{
  if (mapped()==MSTrue)
   {
     int offset=highlightThickness()+shadowThickness();
     int y=offset;
     int thickness=MSSashDefaultShadowThickness;
     if (thickness>1)
      {
	int x=width()>>1;
	XFillRectangle(display(),window(),bottomShadowGC(),
		       x,y,1,height()-2*offset);
	XFillRectangle(display(),window(),topShadowGC(),
		       x+1,y,1,height()-2*offset);
      }
     else 
      {
	XFillRectangle(display(),window(),backgroundShadowGC(),
		       height()>>1,y,height()-2*offset,1);
      }
   }
}

void MSVSash::drawSash(void)
{
  if (mapped()==MSTrue)
   {
     int offset=highlightThickness()+shadowThickness();
     int sht=MSSashDefaultShadowThickness;
     
     MSRect aRect(offset,height()-offset-MSSashDefaultIndent,
		  MSSashMSDefaultWidth,MSSashMSDefaultHeight);
     drawBevel(aRect,MSRaised,sht);
     XFillRectangle(display(),window(),backgroundShadowGC(),
		    aRect.x()+sht,aRect.y()+sht,
		    aRect.width()-2*sht,aRect.height()-2*sht);
   }
}

void MSVSash::button1Press(const XEvent *ev_)
{
  int xoffset=width()>>1;
  int startx=x()+xoffset;
  int delta=xoffset-ev_->xbutton.x;
  int xmin=min();
  int xmax=max()-xoffset;
  unsigned int mask=Button1Mask;
  unsigned int keys=mask;
  int ix=0,iy=0;
  int rx=0,ry=0;
  int oldX=x_origin();
  int lastX=startx;
  Window root,child;

  server()->grabPointer(window(),False,ButtonPressMask|ButtonReleaseMask,
			GrabModeAsync,GrabModeAsync,None,
			_crossCursor->cursor(),ev_->xbutton.time);

  XDrawLine(display(),owner()->window(),gc(),lastX,0,lastX,owner()->height());
  int sameScreen=XQueryPointer(display(),owner()->window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
  while (keys&mask)
   {
     if (sameScreen==True)
      {
	ix+=delta;
	if (ix<xmin) ix=xmin;
	else if (ix>xmax) ix=xmax;
	if (lastX!=ix)
	 {
	   XDrawLine(display(),owner()->window(),gc(),lastX,0,lastX,owner()->height());
	   lastX=ix;
	   XDrawLine(display(),owner()->window(),gc(),lastX,0,lastX,owner()->height());
	 }
      }
     sameScreen=XQueryPointer(display(),owner()->window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
   }
  XDrawLine(display(),owner()->window(),gc(),lastX,0,lastX,owner()->height());
  moveTo(lastX-xoffset,y_origin());

  int deltaX=oldX-x_origin();
  MSPane *p=(MSPane *) owner(); 
  p->adjustColumnWidth(column(),deltaX);
  server()->ungrabPointer(window(),CurrentTime);
}

At MSPane::childPosition(MSWidget *widget_)
{ return MSLayout::childPosition(widget_); }

void MSPane::childPosition(MSWidget *widget_,const At& at_)
{
  if(ignoreResizeConstraints()==MSTrue)
   {
     unsigned long opts=at_.constraints();
     At a(at_);
     if (orientation()==MSLayoutManager::Vertical)
      {
        a.constraints(opts&(At::Left|At::Right|At::MinimizeWidth|At::MaintainWidth));
      }
     else if (orientation()==MSLayoutManager::Horizontal)
      {
        a.constraints(opts&(At::Top|At::Bottom|At::MinimizeHeight|At::MaintainHeight));
      }
     MSLayout::childPosition(widget_,a);
   }
  else
   {
     MSLayout::childPosition(widget_,at_);
   }
}

void MSPane::childResizeConstraints(MSWidget *widget_)
{
  if(ignoreResizeConstraints()==MSTrue)
   {
     MSLayoutEntry *entry=getEntry(widget_);  // get entry from childList
     if (entry!=0&&widget_!=this)
      {
        unsigned long opts=widget_->resizeConstraints();
        if (entry->at().constraints()!=opts)
         {
           if (orientation()==MSLayoutManager::Vertical)
            {
              entry->at().constraints(opts&(At::Left|At::Right|At::MinimizeWidth|At::MaintainWidth));
            }
           else if (orientation()==MSLayoutManager::Horizontal)
            {
              entry->at().constraints(opts&(At::Top|At::Bottom|At::MinimizeHeight|At::MaintainHeight));
            }
           if (entry->mapped()==MSTrue) adjustSize();
         }
      }
   }
  else
   {
     MSLayout::childResizeConstraints(widget_);
   }
}

void MSPane::updateOrientation(void)
{
  if (firstMap()==MSTrue)
   {
     removeAllSashes();
     if (setPositions()==MSTrue) adjustSize();
   }
}

MSPane::MSPane(MSWidget *owner_,const char *title_) : 
MSLayout(owner_,title_)
{ init(); }

MSPane::MSPane(MSWidget *owner_,const MSStringVector& title_) : 
MSLayout(owner_,title_)
{ init(); }

void MSPane::init(void)
{
  _rowSpacing=MSPaneDefaultRowSpacing;
  _ignoreResizeConstraints=MSTrue;
  sashAction(MSFalse);
}

MSPane::~MSPane(void)
{ removeAllSashes(); }

void MSPane::updateBackground(unsigned long oldbg_)
{
  MSLayout::updateBackground(oldbg_);
  for (unsigned i=0;i<sashList().length();i++) _sashList(i)->background(background());
}

void MSPane::adjustRowHeight(int row_,int height_)
{
  freeze();
  MSLayoutEntry *entry;
  int span=0;
  int c;
  int r=row_;  

  for (c=0;c<columns();c+=span)
   {
     entry=getEntry(r,c);
     if (entry!=0)
      {
	entry->widget()->height(entry->widget()->height()-height_);
        span=entry->at().columnSpan();
      }
     else span=1;
   }

  r++;
  for (c=0;c<columns();c+=span)
   {
     entry=getEntry(r,c);
     if (entry!=0)
      {
	entry->widget()->height(entry->widget()->height()+height_);
        span=entry->at().columnSpan();
      }
     else span=1;
   }

  freezeStatus(MSFalse);
  recomputeVectors();
  placement();
  activateCallback(MSWidgetCallback::childresize);
}

void MSPane::adjustColumnWidth(int col_,int width_)
{
  freeze();
  MSLayoutEntry *entry;
  int span=0;
  int c=col_;
  int r;
  
  for (r=0;r<rows();r+=span)
   {
     entry=getEntry(r,c);
     if (entry!=0)
      {
	entry->widget()->width(entry->widget()->width()-width_);
        span=entry->at().rowSpan();
      }
     else span=1;
   }

  c++;
  for (r=0;r<rows();r+=span)
   {
     entry=getEntry(r,c);
     if (entry!=0)
      {
	entry->widget()->width(entry->widget()->width()+width_);
        span=entry->at().rowSpan();
      }
     else span=1;
   }
  freezeStatus(MSFalse);
  recomputeVectors();
  placement();
  activateCallback(MSWidgetCallback::childresize);
}

int MSPane::numSashes(void)
{ return sashList().length(); }

void MSPane::childCreate(MSWidget *widget_)
{ if (sashAction()==MSFalse) MSLayout::childInsert(widget_); }
void MSPane::childDestroy(MSWidget *widget_)
{ if (sashAction()==MSFalse) MSLayout::childRemove(widget_); }
void MSPane::childMap(MSWidget *widget_)
{ if (sashAction()==MSFalse) MSLayout::childMap(widget_); }
void MSPane::childUnmap(MSWidget *widget_)
{ if (sashAction()==MSFalse) MSLayout::childUnmap(widget_); }
void MSPane::childConfigure(MSWidget *widget_)
{ if (sashAction()==MSFalse) MSLayout::childConfigure(widget_); }

void MSSash::buttonPress(const XEvent *event_)
{ buttonPressNotify(this,event_); }

void MSPane::placement(void)
{
  MSLayout::placement();
  recomputeVectors();
  if ((rows()>0||columns()>0)&&mapped()==MSTrue) placeSashes();
}

// delete the sash associated with row,col
void MSPane::removeSash(int row_,int col_)
{
  sashAction(MSTrue);
  MSSash *pSash;
  for (unsigned i=0;i<sashList().length();i++)
   {
     pSash=(MSSash *)_sashList(i);
     if (pSash->row()==row_&&pSash->column()==col_)
      {
        sashList().removeAt(i);
	delete pSash;
	break;
      }
   }
  sashAction(MSFalse);
}

void MSPane::removeAllSashes(void)
{
  sashAction(MSTrue);
  for (unsigned i=0;i<sashList().length();i++) delete _sashList(i);
  sashList().removeAll();
  sashAction(MSFalse);
}

// delete extra sashes
void MSPane::removeExtraSashes(void)
{
  sashAction(MSTrue);
  MSSash *pSash;
  MSWidgetVector newSashList;
  for (unsigned i=0;i<sashList().length();i++)
   {
     pSash=(MSSash *)_sashList(i);
     if ((orientation()==MSLayoutManager::Vertical&&pSash->row()>=rows()-1)||
         (orientation()==MSLayoutManager::Horizontal&&pSash->column()>=columns()-1))
      {
	delete pSash;
      }
     else newSashList.append(pSash);
   }
  _sashList=newSashList;
  sashAction(MSFalse);
}

void MSPane::placeSashes(void)
{
  int offset=highlightThickness()+shadowThickness();
  MSSash *pSash;
  
  sashAction(MSTrue);
  if (orientation()==MSLayoutManager::Vertical)
   {
     int y=offset+innerHeight();
     for (int i=0;i<rows()-1;i++)
      {
	pSash=sash(i,0);   
	if (pSash==0)
	 {
	   pSash=new MSHSash(this);
	   sashList().append(pSash);
	   pSash->row(i);
	   pSash->column(0);
	 }
	pSash->width(width()-2*offset);
	pSash->moveTo(offset,y+rowHeight(i));
	pSash->min(y+rowSpacing());
	pSash->max(pSash->y_origin()+rowSpacing()+rowHeight(i+1));
	if (pSash->mapped()==MSFalse) pSash->map();
	pSash->raise();
	y+=rowHeight(i)+rowSpacing();
      }
   }
  else if (orientation()==MSLayoutManager::Horizontal)
   {
     int x=offset+innerWidth();
     for (int i=0;i<columns()-1;i++)
      {
	pSash=sash(0,i);   
	if (pSash==0)
	 {
	   pSash=new MSVSash(this);
	   sashList().append(pSash);	   
	   pSash->row(0);
	   pSash->column(i);
	 }
	pSash->height(height()-2*offset-innerHeight());
	pSash->moveTo(x+columnWidth(i),offset+innerHeight());
	pSash->min(x+columnSpacing());
	pSash->max(pSash->x_origin()+columnSpacing()+columnWidth(i+1));
	if (pSash->mapped()==MSFalse) pSash->map();
	pSash->raise();
	x+=columnWidth(i)+columnSpacing();
      }     
   }
  removeExtraSashes();
  sashAction(MSFalse);
}

MSSash *MSPane::sash(int row_,int col_)
{
  MSSash *pSash;
  for (unsigned i=0;i<sashList().length();i++)
   {
     pSash=(MSSash *)_sashList(i);
     if (pSash->row()==row_&&pSash->column()==col_) return pSash;
   }
  return 0;
}

void MSPane::ignoreResizeConstraints(MSBoolean ignoreResizeConstraints_)
{
  if(_ignoreResizeConstraints != ignoreResizeConstraints_)
   {
     _ignoreResizeConstraints = ignoreResizeConstraints_;
   }
}

void MSPane::set(MSAttrValueList& avList_)
{
  MSLayout::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="ignoreResizeConstraints")
      {
        ignoreResizeConstraints(avList_[i].value().asBoolean());
        index<<i;
      }
   }
  avList_.remove(index);
}

MSAttrValueList& MSPane::get(MSAttrValueList& avList_)
{
   MSStringVector aStringVector("MSTrue\nMSFalse");

  avList_<<MSAttrValue("ignoreResizeConstraints",
		       (ignoreResizeConstraints()==MSTrue?"MSTrue":"MSFalse"),
		       aStringVector);
 avList_<<MSAttrValue("childresize","",MSAttrValue::Callback);
  return MSLayout::get(avList_);
}




///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <math.h>
#if HAVE_NEW
#include <new>
#else
#include <new.h>
#endif
#include <stdlib.h>
#include <MSGUI/MSKeysym.H>
#include <MSGUI/MSKeysymdef.H>
#include <MSGUI/MSPage.H>
#include <MSGUI/MSBusy.H>

static const unsigned long MSPageEventMask=(ExposureMask|ButtonPressMask|
                                     ButtonReleaseMask|Button1MotionMask);
static const char *MSPageDefaultForeground="green";
static const char *MSPageDefaultBackground="black";
static const int MSPageDefaultHighlightThickness=2;
static const int MSPageDefaultShadowThickness=0;
static const int MSPageDefaultMargin=2;
//static const char *MSPageDefaultFont="courier-14";
static const unsigned long MSPageDefaultBlinkRate=250;  // 250 milliseconds

MSPageBlinkTimer::MSPageBlinkTimer(MSPage *page_,unsigned long interval_) : 
MSIntervalTimer(interval_) { _page=page_; }
void MSPageBlinkTimer::process(void) { page()->processBlinkTimer(); }

MSPage::MSPage(MSWidget *owner_) : MSCompositeText(owner_) 
{
  init();
}

MSPage::MSPage(MSWidget *owner_,MSCharMatrix& model_) : MSCompositeText(owner_) 
{
  init();
  couple(&model_);
}

void MSPage::model(MSCharMatrix& model_)
{
  couple(&model_);
}

void MSPage::init(void)
{
  foreground(MSPageDefaultForeground);
  background(MSPageDefaultBackground);
  _highlightThickness=MSPageDefaultHighlightThickness;
  _shadowThickness=MSPageDefaultShadowThickness;
  _margin=MSPageDefaultMargin;
  _blinkTimer=0;
  _blinkRate=MSPageDefaultBlinkRate;
  _blinkPhase=Normal;
  _lineWidth=10;  // 10 percent
  _boldFontID=0;
  _x_cursor=-1;
  _y_cursor=-1;
  
  backingStore(WhenMapped);
  shadowStyle(MSEtchedOut);
  acceptFocus(MSTrue);
  sensitive(MSTrue);
  addColor(0,foreground(),background());
  selectInput(MSPageEventMask); 
  freeze();
}

MSPage::~MSPage(void)
{
  stopBlinkTimer();
  MSNodeItem *hp=colorListHead();
  MSNodeItem *np;
  ColorCell  *cc;

  while((np=hp->next())!=hp)
   {
     cc=(ColorCell *)np->data();
     delete np;
     delete cc;
   }
}

ColorCell *MSPage::colorCell(int index_)
{
  MSNodeItem  *hp=colorListHead();
  MSNodeItem  *np=hp;
  ColorCell *cc=0;

  while((np=np->next())!=hp)
   {
     cc=(ColorCell *)np->data();
     if (cc->id()==index_) np=hp->prev();
     else cc=0;
   }
  return cc;
}

void MSPage::addColor(int index_,unsigned long fg_,unsigned long bg_)
{
  ColorCell *cc=colorCell(index_);
  if (cc!=0)
   {
     cc->fg(fg_);
     cc->bg(bg_); 
   }  
  else
   {
     cc=new ColorCell(index_,fg_,bg_);
     MSNodeItem *np=new MSNodeItem((void *)cc);
     np->insert(colorListHead()); // fifo list
   }
}

void MSPage::addColor(int index_,const char *fg_,const char *bg_)
{ addColor(index_,server()->pixel(fg_),server()->pixel(bg_)); }

void MSPage::computeSize(void)
{
  int offset=(highlightThickness()+shadowThickness()+margin()+1)<<1;
  resize(matrix().columns()*charWidth()+offset,matrix().rows()*textHeight()+offset);
}

void MSPage::naturalSize(void)
{ computeSize(); }

void MSPage::configure(void)
{
   int offset=highlightThickness()+shadowThickness()+margin();
   XRectangle clipRect[1];
   clipRect[0].x=0;
   clipRect[0].y=0;
   clipRect[0].width=drawWidth();
   clipRect[0].height=drawHeight();
   XSetClipRectangles(display(),textGC(),offset,offset,&clipRect[0],1,Unsorted);
   refresh();
}

void MSPage::redraw(void)
{
  if (mapped()==MSTrue&&frozen()==MSFalse)
   {
     screenRedraw();
     drawShadow();
   }
}

void MSPage::refresh(void)
{
  if (mapped()==MSTrue)
   {
     clear();
     blinkPhase(Normal);
     redraw();
   }
}

void MSPage::updateForeground(unsigned long oldfg_)
{
  MSCompositeText::updateForeground(oldfg_);
  refresh();
}

void MSPage::updateBackground(unsigned long oldbg_)
{
  MSCompositeText::updateBackground(oldbg_);
  refresh();
}

void MSPage::updateFont(Font oldfid_)
{
  MSCompositeText::updateFont(oldfid_);

  Font fid=font();
  const char *fontString=server()->fontName(fid);
  if (fontString!=0)
  {
     MSString fs(fontString);
     unsigned pos=0;
     if ((pos=fs.indexOf("-bold",0))==fs.length())
     {
	unsigned ipos=fs.length();
	if ((pos=fs.indexOf('-',0))<fs.length()) ipos=pos;
	fs.insert("-bold",ipos);
	Font fsid=server()->fontID(fs);
	if (fsid!=server()->defaultFont()) boldFontID(fsid);
	else boldFontID(0);
     }
     else boldFontID(fid);
  }

  if (firstMap()==MSTrue)
   {
     if (dynamic()==MSTrue) computeSize();
     else refresh();
   }
}

void MSPage::firstMapNotify(void)
{
   freeze();
   updateFont(font());
   computeSize();
   unfreeze();  
}

void
MSPage::unfreeze()
{
   freezeStatus(MSFalse); 
   screenUpdate ();
}

void MSPage::updateData(void) { refresh(); }

int MSPage::computeYCoord(int row_)
{
  int offset=highlightThickness()+shadowThickness()+margin()+textAscent();
  return offset+(row_*textHeight());
}

int MSPage::computeXCoord(int,int col_)
{
  int offset=highlightThickness()+shadowThickness()+margin();
  return offset+(col_*charWidth());
}
  
int MSPage::drawWidth(void)
{
  int offset=(highlightThickness()+shadowThickness()+margin())<<1;
  return width()-offset;
}  

int MSPage::drawHeight(void)
{
  int offset=(highlightThickness()+shadowThickness()+margin())<<1;
  return height()-offset;
}  

void MSPage::blinkRate(unsigned long rate_)
{
  if (rate_!=blinkRate())
   {
     MSBoolean stop=(blinkTimer()==0)?MSTrue:MSFalse;
     if (blinkTimer()!=0) delete _blinkTimer;
     _blinkRate=rate_;
     _blinkTimer=new MSPageBlinkTimer(this,(unsigned long)blinkRate());
     if (stop==MSTrue) blinkTimer()->stop();
   }
}

void MSPage::startBlinkTimer(void)
{
  if (blinkTimer()!=0) blinkTimer()->reset();
  else _blinkTimer=new MSPageBlinkTimer(this,(unsigned long)blinkRate());
}

void MSPage::stopBlinkTimer(void)
{
  if (blinkTimer()!=0) delete blinkTimer();
  _blinkTimer=0;
}

void MSPage::processBlinkTimer(void) { blinkTimeOut(); }

void MSPage::update(const MSIndexVector& index_)
{
  if (frozen())
     return;
  if (index_.length()>0)
   {
     int n=index_.length(); 
     int nc=matrix().columns();
     int nr=matrix().rows();
     int i=0,k=0,r=0;
     int co=0;
     int cc=0;
     MSIntVector row(1);
     
     for (r=0;r<nr;r++)
      {
	for (i=0;i<n;i++) { if (co<=index_(i)&&index_(i)<co+nc) cc++; }
	if (cc>0)
	 {
	   MSIntVector cols(cc);
	   for (i=0,k=0;i<n;i++) 
	    { if (co<=index_(i)&&index_(i)<co+nc) cols[k++]=(index_(i)-co); }
           row[0]=r;
	   screenUpdate(row,cols);
	 }
	co+=nc;
	cc=0;
      }
   }
  else screenUpdate();
}  

MSBoolean MSPage::verify(const MSIntMatrix& m_ ) const
{ return (m_.rows()==matrix().rows()&&m_.columns()==matrix().columns())?MSTrue:MSFalse; }
MSBoolean MSPage::verify(const MSBinaryMatrix& m_ ) const
{ return (m_.rows()==matrix().rows()&&m_.columns()==matrix().columns())?MSTrue:MSFalse; }

void MSPage::drawAllRowsCols(void)
{
  if (MSView::model()!=0) 
   {
     const MSIntMatrix& colorM=colorMatrix();
     const MSBinaryMatrix& boldM=boldMatrix();
     const MSBinaryMatrix& ulineM=underlineMatrix();
     MSBoolean useColor=verify(colorM);
     MSBoolean useBold=verify(boldM);
     MSBoolean useUline=verify(ulineM);
     const char *cp=matrix().data();
     int color,nextColor;
     MSBoolean bold,nextBold;
     MSBoolean uline,nextUline;
     int nc=matrix().columns();
     int nr=matrix().rows();
     int offset=0;
     int len=1;
     
     for(int i=0;i<nr;i++)
      {
	for(int j=0;j<nc;j+=len)
	 {
	   color=(useColor==MSTrue)?(int)colorM(offset+j):0;
	   bold=(useBold==MSTrue&&boldM(offset+j)==1)?MSTrue:MSFalse;
	   uline=(useUline==MSTrue&&ulineM(offset+j)==1)?MSTrue:MSFalse;
	   len=1;
	   while(j+len<nc)
	    {
	      nextColor=(useColor==MSTrue)?(int)colorM(offset+(len+j)):0;
	      nextBold=(useBold==MSTrue&&boldM(offset+(len+j))==1)?MSTrue:MSFalse;
	      nextUline=(useUline==MSTrue&&ulineM(offset+(len+j))==1)?MSTrue:MSFalse;
	      if (nextColor==color&&nextBold==bold&&nextUline==uline) len++;
	      else break;
	    }
	   drawRow(i,j,cp+(offset+j),len,color,bold,Normal,uline);
	 }
	offset+=nc;
      }
   }
}

void MSPage::drawAllRows(const MSIntVector& columns_)
{
  if (columns_.length()>0)  // update all rows
   {
     const MSIntMatrix& colorM=colorMatrix();
     const MSBinaryMatrix& boldM=boldMatrix();
     const MSBinaryMatrix& ulineM=underlineMatrix();
     MSBoolean useColor=verify(colorM);
     MSBoolean useBold=verify(boldM);
     MSBoolean useUline=verify(ulineM);
     const char *cp=matrix().data();
     int col,len,color,nextCol,nextColor;
     MSBoolean bold,nextBold;
     MSBoolean uline,nextUline;
     int nr=matrix().rows();
     int nc=matrix().columns();
     int offset=0;
     
     for(int i=0;i<nr;i++)
      {
	len=1;
	for(int j=0;j<columns_.length();j+=len)
	 {
	   col=columns_(j);
	   color=(useColor==MSTrue)?(int)colorM(offset+col):0;
	   bold=(useBold==MSTrue&&boldM(offset+col)==1)?MSTrue:MSFalse;
	   uline=(useUline==MSTrue&&ulineM(offset+col)==1)?MSTrue:MSFalse;
	   len=1;
	   
	   while(col+len<nc&&j+len<columns_.length())
	    {
	      nextCol=(int)columns_(j+len);
	      if (nextCol==col+len)
	       {
		 nextColor=(useColor==MSTrue)?(int)colorM(offset+(col+len)):0;
		 nextBold=(useBold==MSTrue&&boldM(offset+(col+len))==1)?MSTrue:MSFalse;
		 nextUline=(useUline==MSTrue&&ulineM(offset+(col+len))==1)?MSTrue:MSFalse;
		 if (nextColor==color&&nextBold==bold&&nextUline==uline) len++; 
		 else break;
	       }
	      else break;
	    }
	   drawRow(i,col,cp+(offset+col),len,color,bold,Normal,uline);
	 }
	offset+=nc;
      }
   }
}

void MSPage::drawAllCols(const MSIntVector& rows_)
{
  if (rows_.length()>0) // update all cols
   {
     const MSIntMatrix& colorM=colorMatrix();
     const MSBinaryMatrix& boldM=boldMatrix();
     const MSBinaryMatrix& ulineM=underlineMatrix();
     MSBoolean useColor=verify(colorM);
     MSBoolean useBold=verify(boldM);
     MSBoolean useUline=verify(ulineM);
     const char *cp=matrix().data();
     int len,color,nextColor;
     MSBoolean bold,nextBold;
     MSBoolean uline,nextUline;
     int nr=matrix().rows();
     int nc=matrix().columns();
     int offset=0;
     int row;
     
     for(int i=0;i<rows_.length();i++)
      {
	len=1;
	row=rows_(i);
	offset=row*nc;
	for(int j=0;j<nc;j+=len)
	 {
	   color=(useColor==MSTrue)?(int)colorM(offset+j):0;
	   bold=(useBold==MSTrue&&boldM(offset+j))?MSTrue:MSFalse;
	   uline=(useUline==MSTrue&&ulineM(offset+j)==1)?MSTrue:MSFalse;
	   len=1;
	   while(j+len<nc)
	    {
	      nextColor=(useColor==MSTrue)?(int)colorM(offset+(j+len)):0;
	      nextBold=(useBold==MSTrue&&boldM(offset+(j+len))==1)?MSTrue:MSFalse;
	      nextUline=(useUline==MSTrue&&ulineM(offset+(j+len))==1)?MSTrue:MSFalse;
	      if (nextColor==color&&nextBold==bold&&nextUline==uline) len++;
	      else break;
	    }
	   drawRow(row,j,cp+(offset+j),len,color,bold,Normal,uline);
	 }
      }
   }
}

void MSPage::drawIndexed(const MSIntVector& rows_,const MSIntVector& columns_)
{
  if (MSView::model()!=0)
   {
     const MSIntMatrix& colorM=colorMatrix();
     const MSBinaryMatrix& boldM=boldMatrix();
     const MSBinaryMatrix& ulineM=underlineMatrix();
     MSBoolean useColor=verify(colorM);
     MSBoolean useBold=verify(boldM);
     MSBoolean useUline=verify(ulineM);
     const char *cp=matrix().data();
     int row,col,color,nextColor;
     MSBoolean bold,nextBold;
     MSBoolean uline,nextUline;
     int nr=matrix().rows();
     int nc=matrix().columns();
     int offset;
     
     for(int i=0;i<rows_.length();i++)
      {
	row=(int)rows_(i);
	offset=row*nc;
        if (columns_.length()<=2) // optimize for the length 1 case
	 {
	   for(int j=0;j<columns_.length();j++)
	    {
	      col=(int)columns_(j);
	      color=(useColor==MSTrue)?(int)colorM(offset+col):0;
	      bold=(useBold==MSTrue&&boldM(offset+col)==1)?MSTrue:MSFalse;
	      uline=(useUline==MSTrue&&ulineM(offset+col)==1)?MSTrue:MSFalse;
	      drawRow(row,col,cp+(offset+col),1,color,bold,Normal,uline);
	    }
	 }
        else
	 {
           int sc=columns_(0);
           int ec=sc;
           int j;
           for (j=0;j<columns_.length();j++)
            {
              int cv=columns_(j);
              sc=(cv<sc)?cv:sc;
	      ec=(cv>ec)?cv:ec;
            }	   
           ec=(ec<nc)?ec:nc-1;
	   sc=(sc>=0)?sc:0;
           int len=0;
           for (j=sc;j<=ec;j+=len)
	    {
	      color=(useColor==MSTrue)?(int)colorM(offset+j):0;
	      bold=(useBold==MSTrue&&boldM(offset+j))?MSTrue:MSFalse;
	      uline=(useUline==MSTrue&&ulineM(offset+j)==1)?MSTrue:MSFalse;
	      len=1;
	      while(j+len<=ec)
	       {
		 nextColor=(useColor==MSTrue)?(int)colorM(offset+(j+len)):0;
		 nextBold=(useBold==MSTrue&&boldM(offset+(j+len))==1)?MSTrue:MSFalse;
		 nextUline=(useUline==MSTrue&&ulineM(offset+(j+len))==1)?MSTrue:MSFalse;
		 if (nextColor==color&&nextBold==bold&&nextUline==uline) len++;
		 else break;
	       }
	      drawRow(row,j,cp+(offset+j),len,color,bold,Normal,uline);
	    }
	 }
      }
   }
}

// called after each data update
void MSPage::blinkUpdate(void)
{
  const MSBinaryMatrix& newBlink=blinkMatrix();
  if (verify(newBlink)==MSTrue)
   {
     MSBoolean stopTimer=MSTrue;
     if (blinkTimer()!=0)
      {
	if (blinkPhase()==Normal)
	 {
	   for (int i=0;i<newBlink.length();i++)
	    {
	      if (newBlink(i)==1) 
	       {
		 stopTimer=MSFalse;
		 break;
	       }
	    }
	 }
	else // reverse
	 {
	   if (currentBlinkMatrix().rows()>0)
	    {
	      int k=0;
	      int delta;
	      int color;
	      MSBoolean bold;
	      MSBoolean uline;
	      const MSIntMatrix& colorM=colorMatrix();
	      const MSBinaryMatrix& boldM=boldMatrix();
	      const MSBinaryMatrix& ulineM=underlineMatrix();
	      MSBoolean useColor=verify(colorM);
	      MSBoolean useBold=verify(boldM);
	      MSBoolean useUline=verify(ulineM);
	      const char *cp=matrix().data();
	      
	      for (int i=0;i<newBlink.rows();i++)        
	       {
		 for (int j=0;j<newBlink.columns();j++)
		  {
		    if (newBlink(k)==1) stopTimer=MSFalse;
		    delta=int(newBlink(k)-(int)(currentBlinkMatrix()(k)));
		    if (delta==-1||delta==1)
		     {
		       color=(useColor==MSTrue)?(int)colorM(k):0;
		       bold=(useBold==MSTrue&&boldM(k)==1)?MSTrue:MSFalse;
		       uline=(useUline==MSTrue&&ulineM(k)==1)?MSTrue:MSFalse;
		       drawRow(i,j,cp+k,1,color,bold,(delta==1)?Reverse:Normal,uline);
		     }
		    k++;
		  }
	       }
	    }
	 }
	currentBlinkMatrix(newBlink);
	if (stopTimer==MSTrue) stopBlinkTimer();
      }
     else // timer not running
      {
	if (drawBlink(newBlink,Reverse)==MSTrue)
	 {
	   blinkPhase(Reverse);
	   currentBlinkMatrix(newBlink);
	   startBlinkTimer();
	 }
      }
   }
}

void MSPage::blinkTimeOut(void)
{
  if (blinkPhase()==Normal)
   {
     const MSBinaryMatrix& blinkM=currentBlinkMatrix();
     if (verify(blinkM)==MSTrue)
      {
	MSBoolean stopTimer=(drawBlink(blinkM,Reverse)==MSTrue)?MSFalse:MSTrue;
	blinkMatrix(blinkM);
	toggleBlinkPhase();
	if (stopTimer==MSTrue) stopBlinkTimer();
      }
   }
  else  // reverse
   {
     (void)drawBlink(currentBlinkMatrix(),Normal);  
     toggleBlinkPhase();
   }
}

MSBoolean MSPage::drawBlink(const MSBinaryMatrix& blink_,BlinkPhase phase_)
{
  MSBoolean status=MSFalse;
  if (verify(blink_)==MSTrue)
   {
     int k=0;
     int color,nextColor;
     MSBoolean bold,nextBold;
     MSBoolean uline,nextUline;
     int len;
     int nr=matrix().rows();
     int nc=matrix().columns();

     const MSIntMatrix& colorM=colorMatrix();
     const MSBinaryMatrix& boldM=boldMatrix();
     const MSBinaryMatrix& ulineM=underlineMatrix();
     MSBoolean useColor=verify(colorM);
     MSBoolean useBold=verify(boldM);
     MSBoolean useUline=verify(ulineM);
     const char *cp=matrix().data();
     
     for (int i=0;i<blink_.rows();i++)        
      {
	len=1;
	for (int j=0;j<blink_.columns();j+=len)
	 {
	   len=1;
	   if (blink_(k)==1)
	    {
	      status=MSTrue;
	      color=(useColor==MSTrue)?colorM(k):0;
	      bold=(useBold==MSTrue&&boldM(k)==1)?MSTrue:MSFalse;
	      uline=(useUline==MSTrue&&ulineM(k)==1)?MSTrue:MSFalse;
	      
	      while(j+len<nc)
	       {
		 if (blink_(k+len)==1)
		  {
		    nextColor=(useColor==MSTrue)?(int)colorM(k+len):0;
		    nextBold=(useBold==MSTrue&&boldM(k+len)==1)?MSTrue:MSFalse;
		    nextUline=(useUline==MSTrue&&ulineM(k+len)==1)?MSTrue:MSFalse;
		    if (nextColor==color&&nextBold==bold&&nextUline==uline) len++;
		    else break;
		  }
		 else break;
	       }              
	      drawRow(i,j,cp+k,len,color,bold,phase_,uline);
	    }
	   k+=len;
	 }
      }
   }
  if (status==MSTrue) drawBoxes(MSFalse);
  return status;
}

void MSPage::screenUpdate(void)
{ 
  if (mapped()==MSTrue)
   {
     drawAllRowsCols(); 
     drawLines(MSFalse);
     blinkUpdate();
     drawBoxes(MSFalse);
   }
}

void MSPage::screenUpdate(const MSIntVector& rows_,const MSIntVector& cols_)
{
  if (mapped()==MSTrue)
   {
     drawIndexed(rows_,cols_);
     drawLines(MSFalse);
     blinkUpdate();
     drawBoxes(MSFalse);
   }
}

void MSPage::drawRow(int row_,int col_,const char *str_,int len_,
                     int index_,MSBoolean bold_,BlinkPhase phase_,MSBoolean underline_)
{
  if (str_!=0)
   {
     int y=computeYCoord(row_);
     int x=computeXCoord(row_,col_>=0?col_:0);
     int tw=len_*charWidth();
     int lastRowAdjustment=(matrix().rows()-1==row_)?1:0;
     int lastColAdjustment=(matrix().columns()==col_+len_)?1:0;
     ColorCell *cc=colorCell(index_);
     unsigned long fg;
     unsigned long bg;
     if (cc!=0)
      {
        fg=cc->fg();
	bg=cc->bg();
      }
     else
      {
        fg=foreground();
	bg=background();
      }	
     if (phase_==Reverse)
      {
	XSetForeground(display(),textGC(),fg);
	XSetBackground(display(),textGC(),bg);
      }
     else
      {
	XSetForeground(display(),textGC(),bg);
	XSetBackground(display(),textGC(),fg);
      }
     XFillRectangle(display(),window(),textGC(),x,y-textAscent(),
		    tw+lastColAdjustment,textHeight()+lastRowAdjustment);
     if (phase_==Reverse)
      {
	XSetForeground(display(),textGC(),bg);
	XSetBackground(display(),textGC(),fg);
      }
     else
      {
	XSetForeground(display(),textGC(),fg);
	XSetBackground(display(),textGC(),bg);
      }
     Font fid=(bold_==MSTrue&&boldFontID()!=0)?boldFontID():font();
     XSetFont(display(),textGC(),fid);
     const XFontStruct *fs=server()->fontStruct(fid);
     XDrawString(display(),window(),textGC(),fs,x,y,str_,len_);
     if (bold_==MSTrue&&boldFontID()==0) XDrawString(display(),window(),textGC(),fs,x+1,y,str_,len_);
     if (underline_==MSTrue) 
      {
        XDrawLine(display(),window(),textGC(),x,y+textDescent()-1,x+tw-1,y+textDescent()-1);
      }
   }
}

void MSPage::screenRedraw(void)
{
  if (mapped()==MSTrue)
   {
     screenUpdate();
     drawLines(MSFalse);
     drawBoxes(MSFalse);
   }
}

int MSPage::yToRow(int y_)
{
  y_-=highlightThickness()+shadowThickness()+margin();     
  if (y_<0) return 0;
  else if (y_>drawHeight()) return matrix().rows();
  return (y_/textHeight());
}

int MSPage::xToCol(int x_)
{
  x_-=highlightThickness()+shadowThickness()+margin();     
  if (x_<0) return 0;
  else if (x_>drawWidth()) return matrix().columns();
  return (x_/charWidth());
}

// this should be probably be the default in MSWidget
void MSPage::buttonPress(const XEvent *event_)
{ buttonPressNotify(this,event_); }
void MSPage::buttonRelease(const XEvent *event_)
{ buttonReleaseNotify(this,event_); }

void MSPage::button1Press(const XEvent *event_)
{
  x_cursor(xToCol(event_->xbutton.x));
  y_cursor(yToRow(event_->xbutton.y));
  if (isProtected()==MSFalse&&acceptFocus()==MSTrue&&traverseFocus(this)==MSTrue) 
   {
     unsigned int keys;
     unsigned int mask=Button1Mask;
     MSBoolean    moved=MSTrue;  // change in spec--always produce callback
     Window       root,child;
     int 	  ix,iy;
     int 	  rx,ry;
     
     GC gc=XCreateGC(display(),window(),0,0);
     XSetFunction(display(),gc,GXxor);
     XSetForeground(display(),gc,WhitePixelOfScreen(screen())^background());
     XSetBackground(display(),gc,0);

     int offset=highlightThickness()+shadowThickness()+margin();     
     int ex=offset+xToCol(event_->xbutton.x)*charWidth();
     int ey=offset+yToRow(event_->xbutton.y)*textHeight();
     int ox=ex;
     int oy=ey;
     XEvent ne,*ce;

     drawRect(gc,ex,ey,0,0);
     XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);

     while (keys&mask)
      {
	if (abs(ex-ix)>5&&abs(ey-iy)>5) moved=MSTrue;
	if (ix!=ox||iy!=oy)
	 {
	   if (ox>ex)
	    {
	      (oy>ey)?drawRect(gc,ex,ey,(ox-ex),(oy-ey)):
	              drawRect(gc,ex,oy,(ox-ex),(ey-oy));
	    }
	   else
	    {
	      (oy>ey)?drawRect(gc,ox,ey,(ex-ox),(oy-ey)):
	              drawRect(gc,ox,oy,(ex-ox),(ey-oy));
	    }
	   ox=ix;
	   oy=iy;
	   if (ox>ex)
	    {
	      (oy>ey)?drawRect(gc,ex,ey,(ox-ex),(oy-ey)):
	              drawRect(gc,ex,oy,(ox-ex),(ey-oy));
	    }
	   else
	    {
	      (oy>ey)?drawRect(gc,ox,ey,(ex-ox),(oy-ey)):
	              drawRect(gc,ox,oy,(ex-ox),(ey-oy));
	    }
	 }
        XNextEvent(display(),&ne);
        if (ne.type==MotionNotify) 
         {
           ce=compressMotion(&ne);
           ix=ce->xmotion.x;
	   iy=ce->xmotion.y;
	 }
	else if (ne.type==ButtonRelease) 
	 {
           keys=0;
           ix=ne.xbutton.x;
	   iy=ne.xbutton.y;
	 }
      } 
     if (ox>ex)
      {
	(oy>ey)?drawRect(gc,ex,ey,(ox-ex),(oy-ey)):
	        drawRect(gc,ex,oy,(ox-ex),(ey-oy));
      }
     else
      {
	(oy>ey)?drawRect(gc,ox,ey,(ex-ox),(oy-ey)):
	        drawRect(gc,ox,oy,(ex-ox),(ey-oy));
      }
     XFreeGC(display(),gc);
     if (moved==MSTrue) 
      {
        int x=xToCol((ix>ex)?ex:ix);
        int xs=xToCol((ix>ex)?ix:ex)-x;	
	int y=yToRow((iy>ey)?ey:iy);
	int ys=yToRow((iy>ey)?iy:ey)-y;	   
        ys=(y+ys<=matrix().rows())?ys:matrix().rows()-y;
        xs=(x+xs<=matrix().columns())?xs:matrix().columns()-x;	
        rubberBandRect().configure(x,y,xs,ys);
        rubberBand();
      }
     else rubberBandRect().configure(0,0,0,0);
   }
  else if (isProtected()==MSFalse) buttonPressNotify(this,event_);
}

	
void MSPage::rubberBand(void)
{ activateCallback("rubberband"); }

void MSPage::button1Release(const XEvent *event_)
{
  if (isProtected()==MSFalse)
   {
     if (event_->xbutton.button==Button2)      activateCallback("button2up");
     else if (event_->xbutton.button==Button3) activateCallback("button3up");
   } 
}

void MSPage::boxes(const MSIntMatrix& boxMatrix_)
{
  drawBoxes(MSTrue);
  _boxMatrix=boxMatrix_; 
  drawBoxes(MSFalse);
}

void MSPage::lines(const MSIntMatrix& lineMatrix_)
{
  drawLines(MSTrue);
  _lineMatrix=lineMatrix_; 
  drawLines(MSFalse);
}

void MSPage::drawBoxes(MSBoolean clear_)
{
  if (boxMatrix().columns()==4)
   {
     int x,y,ys,xs,w,h;
     int k=0;
     int offset=highlightThickness()+shadowThickness()+margin();
     for (int i=0;i<boxMatrix().rows();i++)
      {
        y=(int)boxMatrix()(k);
	x=(int)boxMatrix()(k+1);
        ys=(int)boxMatrix()(k+2);
	xs=(int)boxMatrix()(k+3);
        h=ys*textHeight();
	w=xs*charWidth();
        y=offset+y*textHeight();
	x=offset+x*charWidth();
        (clear_==MSTrue)?XSetForeground(display(),textGC(),background()):
                       XSetForeground(display(),textGC(),boxColor(i));
        k+=(int)boxMatrix().columns();
	drawRect(textGC(),x,y,w,h);
      }
   }
}

void MSPage::drawLines(MSBoolean clear_)
{
  if (lineMatrix().columns()==4)
   {
     int x,y,w,h,ys,xs;
     int k=0;
     int offset=highlightThickness()+shadowThickness()+margin();
     int lw;
     int delta;
     for(int i=0;i<lineMatrix().rows();i++)
      {
        y=(int)lineMatrix()(k);
	x=(int)lineMatrix()(k+1);
        ys=(int)lineMatrix()(k+2);
	xs=(int)lineMatrix()(k+3);
        if (xs!=0&&ys!=0) xs=0;
        y=offset+(y*textHeight());
	x=offset+(x*charWidth());
        if (xs==0)
	 {
           lw=(int)((lineWidth()*charWidth())/100);
           lw=(lw>0)?lw:1;
           delta=charWidth()-lw;
           delta=(delta>0)?delta>>1:0;
           x+=delta;
           w=lw;
           h=(ys*textHeight());
	 }
	else
	 {
           lw=(int)((lineWidth()*textHeight())/100);
           lw=(lw>0)?lw:1;
           delta=textHeight()-lw;
           delta=(delta>0)?delta>>1:0;
           y+=delta;
           h=lw;
	   w=(xs*charWidth());
	 }
        k+=(int)lineMatrix().columns();
        (clear_==MSTrue)?XSetForeground(display(),textGC(),background()):
                       XSetForeground(display(),textGC(),foreground());
        XFillRectangle(display(),window(),textGC(),x,y,w,h);
      }
   }
}

void MSPage::lineWidth(int lw_) 
{ 
  if (lw_!=lineWidth()) 
   { 
     _lineWidth=lw_;    
     redraw(); 
   }
}

void MSPage::margin(int margin_) 
{ 
  if (margin()!=margin_) 
   { 
     _margin=margin_; 
     refresh(); 
   } 
}

void MSPage::print(const char *file_)
{
  MSBoolean fileOpen=MSFalse;
  MSBoolean open=MSTrue; 

  MSApplicationBusy busy;
  if (outputMode()==Draw)
   {
     if (file_!=0) displayPrintFileName(file_);
     if ((open=displayPrintOpen(this))==MSTrue)
      {
	fileOpen=MSTrue;
	outputMode(Print);
	displayPrintXorigin(0);
	displayPrintYorigin(0);
      }
   }
  if (open==MSTrue)
   {
     redraw();
     if (fileOpen==MSTrue) 
      {
	displayPrintClose();
	outputMode(Draw);
      }
   }
}








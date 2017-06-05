///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusPage.H>

extern MSBoolean isMatrix(A);
extern void setBusyState(MSBoolean);
extern long dbg_tmstk;

const unsigned long AplusPageEventMask=(ExposureMask|ButtonPressMask|
                                       ButtonReleaseMask|Button1MotionMask);

const int AplusPageDefaultHighlightThickness=2;
const int AplusPageDefaultShadowThickness=0;
const int AplusPageDefaultMargin=2;
const int AplusPageDefaultRows=80;
const int AplusPageDefaultCols=25;
const char *AplusPageDefaultFont="courier-14";
const unsigned long AplusPageDefaultBlinkRate=250;  // 250 milliseconds

static MSBoolean isMatrix(A a_,I type_)
{ return (a_!=0&&QA(a_)&&a_->t==type_&&a_->r==2)?MSTrue:MSFalse; }

AplusBlinkTimer::AplusBlinkTimer(AplusPage *page_,unsigned long interval_) :
MSIntervalTimer(interval_)
{ _page = page_; }

AplusBlinkTimer::~AplusBlinkTimer(void)
{}

void AplusBlinkTimer::process(void)
{
  page()->processBlinkTimer();  
}

AplusPage::AplusPage(MSWidget *parent_) : MSCompositeText(parent_) 
{
  _highlightThickness=AplusPageDefaultHighlightThickness;
  _shadowThickness=AplusPageDefaultShadowThickness;
  _margin=AplusPageDefaultMargin;
  _blinkTimer=0;
  _blinkRate=AplusPageDefaultBlinkRate;
  _blink=aplus_nl;
  _blinkPhase=Normal;
  _boxMatrix=aplus_nl;
  _lineMatrix=aplus_nl;
  _lineWidth=10;  // 10 percent
  _boxColors=aplus_nl;
  _keyBuf=aplus_nl;
  _boldFontID=0;
  _x_cursor=-1;
  _y_cursor=-1;

  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
  
  backingStore(WhenMapped);
  shadowStyle(MSEtchedOut);
  acceptFocus(MSTrue);
  acceptTab(MSTrue);
  sensitive(MSTrue);
  addColor(0,foreground(),background());
  selectInput(AplusPageEventMask); 
  freeze();
}

AplusPage::~AplusPage(void)
{
  stopBlinkTimer();
  MSNodeItem  *hp=colorListHead();
  MSNodeItem  *np;
  ColorCell *cc;

  while((np=hp->next())!=hp)
   {
     cc=(ColorCell *)np->data();
     delete np;
     delete cc;
   }
  if (isNull(blink())==MSFalse)       dc(blink());
  if (isNull(boxMatrix())==MSFalse)   dc(boxMatrix());
  if (isNull(lineMatrix())==MSFalse)  dc(lineMatrix());
  if (isNull(boxColors())==MSFalse)   dc(boxColors());
  if (isNull(keyBuffer())==MSFalse)   dc(keyBuffer());
}

ColorCell *AplusPage::colorCell(int index_)
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

void AplusPage::addColor(int index_,unsigned long fg_,unsigned long bg_)
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

void AplusPage::colorTable(A ct_)
{
  if (QA(ct_))
   {
     MSNodeItem  *hp=colorListHead();
     MSNodeItem  *np=hp;
     ColorCell *cc;

     if (ct_->t==It&&ct_->r==(I)2&&ct_->d[1]==(I)2)
      {
	while((np=np->next())!=hp)
	 {
	   cc=(ColorCell *)np->data();
	   if (cc->id()>=(int)ct_->d[0])
	    {
	      delete np;
	      delete cc;
	      np=hp;
	    }
	 }
	P p; p.i=ct_->p;
	for (int i=0;i<(int)ct_->d[0];i++) addColor(i,(unsigned long)p.i[2*i],
						    (unsigned long)p.i[(2*i)+1]);
	redraw();
      }
     else if (ct_->t==Et&&ct_->n==0)
      {
	while((np=hp->next())!=hp)
	 {
	   cc=(ColorCell *)np->data();
	   delete np;
	   delete cc;
	 }
	redraw();
      }
   }
}

A AplusPage::rBand(void)
{
  A r=gv(It,4);
  r->p[0]=(I)rubberBand()->y();
  r->p[1]=(I)rubberBand()->x();
  r->p[2]=(I)rubberBand()->ys();
  r->p[3]=(I)rubberBand()->xs();
  return r;  
}

A AplusPage::colorTable(void)
{
  MSNodeItem  *hp=colorListHead();
  MSNodeItem  *np=hp;
  ColorCell *cc=0;
  int        count=0;  

  while((np=np->next())!=hp) count++;
  np=hp;
  A r=gm(It,count,2);
  count=0;
  while((np=np->next())!=hp)
   {
     cc=(ColorCell *)np->data();
     r->p[count++]=(I)cc->fg();
     r->p[count++]=(I)cc->bg();
   }
  return r;
}

void AplusPage::computeSize(void)
{
  int offset=(highlightThickness()+shadowThickness()+margin()+1)<<1;
  resize(numCols()*charWidth()+offset,numRows()*textHeight()+offset);
}

void AplusPage::configure(void)
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

void AplusPage::redraw(void)
{
  if (mapped()==MSTrue&&frozen()==MSFalse)
   {
     screenRedraw();
     drawShadow();
   }
}

void AplusPage::refresh(void)
{
  if (mapped()==MSTrue)
   {
     clear();
     blinkPhase(Normal);
     redraw();
   }
}

void AplusPage::updateForeground(unsigned long)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  A a = (model()!=0)?((AplusModel*)model())->a():0;
  AColorFunction *fgFunc=AplusModel::getFgFunc(v);
  if (fgFunc!=0) foreground(fgFunc->invoke(v,a));   
}

extern const char *DefaultFont;
extern const char *AltDefaultFont;

void AplusPage::updateFont(Font oldfid_)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  A a = (model()!=0)?((AplusModel*)model())->a():0;
  Font fid=font();
  AFontFunction *fontFunc=AplusModel::getFontFunc(v);
  if (fontFunc!=0) fid=(Font)fontFunc->invoke(v,a);   
  if (oldfid_!=font())
   {
     MSCompositeText::updateFont(oldfid_); 
     const char *fontString=server()->fontName(fid);
     if (fontString!=0)
      {
        MSString fs(fontString);
	int pos=fs.indexOf("-bold", 0);
	if (pos==fs.length())
         {
           int ipos=fs.length();
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
}

void AplusPage::firstMapNotify(void)
{
  freeze();
  updateFont(font());
  computeSize();
  unfreeze();  
}

void AplusPage::updateData(void)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  if (v!=0) refresh();
}
void AplusPage::update(V,int,int,UpdateType type_)
{
  if (type_==ShapeUpdate) updateData();
}

int AplusPage::computeYCoord(int row_)
{
  int offset=highlightThickness()+shadowThickness()+margin()+textAscent();
  return offset+(row_*textHeight());
}

int AplusPage::computeXCoord(int,int col_)
{
  int offset=highlightThickness()+shadowThickness()+margin();
  return offset+(col_*charWidth());
}
  
int AplusPage::drawWidth(void)
{
  int offset=(highlightThickness()+shadowThickness()+margin())<<1;
  return width()-offset;
}  

int AplusPage::drawHeight(void)
{
  int offset=(highlightThickness()+shadowThickness()+margin())<<1;
  return height()-offset;
}  

int AplusPage::numRows(void)
{
  A a = (model()!=0)?((AplusModel*)model())->a():0;
  int d0 = (model()!=0)?((AplusModel*)model())->d0():0;
  return (a==0||isNull(a)==MSTrue)?0:d0;
}
int AplusPage::numCols(void)
{
  A a = (model()!=0)?((AplusModel*)model())->a():0;
  int d1 = (model()!=0)?((AplusModel*)model())->d1():0;
  return (a==0||isNull(a)==MSTrue)?0:d1;
}
MSBoolean AplusPage::verifyData(V,A a_)
{ return (isNull(a_)||isMatrix(a_,Ct))?MSTrue:MSFalse; }

char *AplusPage::string(int row_)
{
  A a = (model()!=0)?((AplusModel*)model())->a():0;
  char *cp=0;
  if (a!=0&&row_<numRows())
   {
     P p; p.i=(model()!=0)?((AplusModel*)model())->data():0;
     int len=numCols();
     cp=new char[len+1];
     int offset=row_*len;
     strncpy(cp,p.c+offset,len);
     cp[len]='\0';
   }
  return cp;
}

void AplusPage::blinkRate(unsigned long rate_)
{
  if (rate_!=blinkRate())
   {
     MSBoolean stop=(blinkTimer()==0)?MSTrue:MSFalse;
     if (blinkTimer()!=0) delete _blinkTimer;
     _blinkRate=rate_;
     _blinkTimer=new AplusBlinkTimer(this, (unsigned long) blinkRate());
     if (stop==MSTrue) blinkTimer()->stop();
   }
}

void AplusPage::startBlinkTimer(void)
{
  if (blinkTimer()!=0) blinkTimer()->reset();
  else _blinkTimer=new AplusBlinkTimer(this,(unsigned long)blinkRate());
}

void AplusPage::stopBlinkTimer(void)
{
  if (blinkTimer()!=0) delete blinkTimer();
  _blinkTimer=0;
}

void AplusPage::processBlinkTimer(void) { blinkTimeOut(); }


void AplusPage::commonUpdate(V v_,A index_,A,I ravel_)
{
  if(index_==(A)MP(22)) update(v_,-1,-1,AppendUpdate);
  else if(!index_) update(v_,-1,-1,ShapeUpdate);
  else if(ravel_) // ravel update
   {
     A a=(A)v_->a;
     if(a->r==2&&a->n==1) 
      { 
        int n=(int)a->d[1];
        int k=(int)index_->p[0];
        int j=k/n;
        update(v_,j,k-n*j,ValueUpdate); 
      }
     else update(v_,-1,-1,ValueUpdate);
   }
  else 
   {
     A r=index_->t==It?index_:index_->n?(A)*index_->p:aplus_nl;
     A c=index_->t==Et&&index_->n>1?(A)index_->p[1]:aplus_nl;
     if(isNull(c)==MSTrue) // c is aplus_nl - all cols are updated
      {     
        if(isNull(r)==MSTrue) update(v_,-1,-1,ValueUpdate);
        else 
         {      
           for(int i=0;i<(int)r->n;i++) update(v_,(int)r->p[i],-1,ValueUpdate);
         }
      }
     else if(isNull(r)==MSTrue) // r is aplus_nl - all rows are updated
      { 
        for(int i=0;i<(int)c->n;i++) update(v_,-1,(int)c->p[i],ValueUpdate);
      }
     else 
      {
	for(int j=0;j<(int)r->n;j++)
	 {
	   for(int i=0;i<(int)c->n;i++)
	    {
	      update(v_,(int)r->p[j],(int)c->p[i],ValueUpdate);
	    }
	 }
      }
   }
}



void AplusPage::update(V v_,A index_,A pick_,I ravel_)
{
  if (ravel_)
   {
     A av=(A)v_->a;
     if(av->r==2&&index_->r==1) 
      { 
        int nc=(int)av->d[1];
        int i=0,k=0;
        int co=0;
        int row=0,cc=0;
        A index,col;

        for (row=0;row<(int)av->d[0];row++)
	 {
           for (i=0;i<index_->n;i++) 
            { if (co<=index_->p[i]&&index_->p[i]<co+nc) cc++; }
           if (cc>0)
	    {
	      index=gv(Et,2);
	      index->p[0]=(I)gi(row);
	      col=gv(It,cc);
	      index->p[1]=(I)col;
	      for (i=0,k=0;i<index_->n;i++) 
               { if (co<=index_->p[i]&&index_->p[i]<co+nc) col->p[k++]=(index_->p[i]-co); }
	      screenUpdate(index);
	      dc(index);
	    }
           co+=nc;
           cc=0;
	 }
      }
     else screenUpdate(aplus_nl);
   }
  else if (!index_) commonUpdate(v_,index_,pick_,ravel_);
  else screenUpdate(index_);
}  

MSBoolean AplusPage::verifyA(A a_)
{
  A a      = (model()!=0)?((AplusModel*)model())->a():0;
  int rank = (model()!=0)?((AplusModel*)model())->rank():0;
  int n    = (model()!=0)?((AplusModel*)model())->numElmts():0;

  return (0!=a_&&QA(a_)&&a_->t==It&&a_->r==rank&&a_->n==n&&
          a_->d[0]==numRows()&&a_->d[1]==numCols())?MSTrue:MSFalse;
}

MSBoolean AplusPage::verifyA(A a_,A r_,A c_)
{
  if (isNull(r_)==MSTrue&&isNull(c_)==MSTrue) return verifyA(a_);
  else if (isNull(r_)==MSTrue)  
   {
     if (0!=a_&&QA(a_)&&a_->n==c_->n*numRows()&&a_->d[0]==numRows()) return MSTrue;
   }
  else if (isNull(c_)==MSTrue)  
   {
     if (0!=a_&&QA(a_)&&a_->n==r_->n*numCols()&&a_->d[0]==numCols()) return MSTrue;
   }
  else if (0!=a_&&QA(a_)&&a_->n==r_->n*c_->n) return MSTrue;
  return MSFalse;
}

void AplusPage::drawAllRowsCols(A index_)
{
  V v      = (model()!=0)?((AplusModel*)model())->aplusVar():0;

  A r=index_->t==It?index_:index_->n?(A)*index_->p:aplus_nl;
  A c=index_->t==Et&&index_->n>1?(A)index_->p[1]:aplus_nl;

  if (isNull(r)==MSTrue&&isNull(c)==MSTrue) 
   {
     A colorA=indexFunc()->invoke(v,aplus_nl);
     A boldA=boldFunc()->invoke(v,aplus_nl);
     A ulineA=underlineFunc()->invoke(v,aplus_nl);
     int color,nextColor;
     MSBoolean bold,nextBold;
     MSBoolean uline,nextUline;
     P p; p.i=((AplusModel*)model())->data();
     int nc=numCols();
     int nr=numRows();
     int offset=0;
     int len=1;
     MSBoolean useColor=verifyA(colorA,aplus_nl,aplus_nl);
     MSBoolean useBold=verifyA(boldA,aplus_nl,aplus_nl);
     MSBoolean useUline=verifyA(ulineA,aplus_nl,aplus_nl);
     
     for(int i=0;i<nr;i++)
      {
	for(int j=0;j<nc;j+=len)
	 {
	   color=(useColor==MSTrue)?(int)colorA->p[offset+j]:0;
	   bold=(useBold==MSTrue&&boldA->p[offset+j]==1)?MSTrue:MSFalse;
	   uline=(useUline==MSTrue&&ulineA->p[offset+j]==1)?MSTrue:MSFalse;
	   len=1;
	   while(j+len<nc)
	    {
	      nextColor=(useColor==MSTrue)?(int)colorA->p[offset+(len+j)]:0;
	      nextBold=(useBold==MSTrue&&boldA->p[offset+(len+j)]==1)?MSTrue:MSFalse;
	      nextUline=(useUline==MSTrue&&ulineA->p[offset+(len+j)]==1)?MSTrue:MSFalse;
	      if (nextColor==color&&nextBold==bold&&nextUline==uline) len++;
	      else break;
	    }
	   drawRow(i,j,p.c+(offset+j),len,color,bold,Normal,uline);
	 }
	offset+=nc;
      }
     dc(colorA);
     dc(boldA);
     dc(ulineA);
   }
}

void AplusPage::drawAllRows(A index_)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;

  A r=index_->t==It?index_:index_->n?(A)*index_->p:aplus_nl;
  A c=index_->t==Et&&index_->n>1?(A)index_->p[1]:aplus_nl;

  if (isNull(r)==MSTrue)  // update all rows
   {
     A colorA=indexFunc()->invoke(v,aplus_nl);
     A boldA=boldFunc()->invoke(v,aplus_nl);
     A ulineA=underlineFunc()->invoke(v,aplus_nl);
     int col,len,color,nextCol,nextColor;
     MSBoolean bold,nextBold;
     MSBoolean uline,nextUline;
     P p; p.i=((AplusModel*)model())->data();
     int nc=numCols();
     int nr=numRows();
     int offset=0;
     MSBoolean useColor=verifyA(colorA,aplus_nl,aplus_nl);
     MSBoolean useBold=verifyA(boldA,aplus_nl,aplus_nl);
     MSBoolean useUline=verifyA(ulineA,aplus_nl,aplus_nl);
     
     for(int i=0;i<nr;i++)
      {
	len=1;
	for(int j=0;j<(int)c->n;j+=len)
	 {
	   col=(int)c->p[j];
	   color=(useColor==MSTrue)?(int)colorA->p[offset+col]:0;
	   bold=(useBold==MSTrue&&boldA->p[offset+col]==1)?MSTrue:MSFalse;
	   uline=(useUline==MSTrue&&ulineA->p[offset+col]==1)?MSTrue:MSFalse;
	   len=1;
	   
	   while(col+len<nc&&j+len<(int)c->n)
	    {
	      nextCol=(int)c->p[j+len];
	      if (nextCol==col+len)
	       {
		 nextColor=(useColor==MSTrue)?(int)colorA->p[offset+(col+len)]:0;
		 nextBold=(useBold==MSTrue&&boldA->p[offset+(col+len)]==1)?MSTrue:MSFalse;
		 nextUline=(useUline==MSTrue&&ulineA->p[offset+(col+len)]==1)?MSTrue:MSFalse;
		 if (nextColor==color&&nextBold==bold&&nextUline==uline) len++; 
		 else break;
	       }
	      else break;
	    }
	   drawRow(i,col,p.c+(offset+col),len,color,bold,Normal,uline);
	 }
	offset+=nc;
      }
     dc(colorA);
     dc(boldA);
     dc(ulineA);
   }
}

void AplusPage::drawAllCols(A index_)
{
  V v    = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  A r=index_->t==It?index_:index_->n?(A)*index_->p:aplus_nl;
  A c=index_->t==Et&&index_->n>1?(A)index_->p[1]:aplus_nl;

  if (isNull(c)==MSTrue) // update all cols
   {
     A colorA=indexFunc()->invoke(v,aplus_nl);
     A boldA=boldFunc()->invoke(v,aplus_nl);
     A ulineA=underlineFunc()->invoke(v,aplus_nl);
     int len,color,nextColor;
     MSBoolean bold,nextBold;
     MSBoolean uline,nextUline;
     P p; p.i=((AplusModel*) model())->data();
     int nc=numCols();
     int nr=numRows();
     int offset=0;
     int row;
     MSBoolean useColor=verifyA(colorA,aplus_nl,aplus_nl);
     MSBoolean useBold=verifyA(boldA,aplus_nl,aplus_nl);
     MSBoolean useUline=verifyA(ulineA,aplus_nl,aplus_nl);
     
     for(int i=0;i<(int)r->n;i++)
      {
	len=1;
	row=(int)r->p[i];
	offset=row*nc;
	for(int j=0;j<nc;j+=len)
	 {
	   color=(useColor==MSTrue)?(int)colorA->p[offset+j]:0;
	   bold=(useBold==MSTrue&&boldA->p[offset+j])?MSTrue:MSFalse;
	   uline=(useUline==MSTrue&&ulineA->p[offset+j]==1)?MSTrue:MSFalse;
	   len=1;
	   while(j+len<nc)
	    {
	      nextColor=(useColor==MSTrue)?(int)colorA->p[offset+(j+len)]:0;
	      nextBold=(useBold==MSTrue&&boldA->p[offset+(j+len)]==1)?MSTrue:MSFalse;
	      nextUline=(useUline==MSTrue&&ulineA->p[offset+(j+len)]==1)?MSTrue:MSFalse;
	      if (nextColor==color&&nextBold==bold&&nextUline==uline) len++;
	      else break;
	    }
	   drawRow(row,j,p.c+(offset+j),len,color,bold,Normal,uline);
	 }
      }
     dc(colorA);
     dc(boldA);
     dc(ulineA);
   }
}

void AplusPage::drawIndexed(A index_)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  A r = index_->t==It?index_:index_->n?(A)*index_->p:aplus_nl;
  A c = index_->t==Et&&index_->n>1?(A)index_->p[1]:aplus_nl;
  
  if (isNull(r)==MSFalse&&isNull(c)==MSFalse) 
   {
     A colorA=indexFunc()->invoke(v,aplus_nl);
     A boldA=boldFunc()->invoke(v,aplus_nl);
     A ulineA=underlineFunc()->invoke(v,aplus_nl);
     int row,col,color,nextColor;
     MSBoolean bold,nextBold;
     MSBoolean uline,nextUline;
     P p; p.i=((AplusModel*)model())->data();
     MSBoolean useColor=verifyA(colorA,aplus_nl,aplus_nl);
     MSBoolean useBold=verifyA(boldA,aplus_nl,aplus_nl);
     MSBoolean useUline=verifyA(ulineA,aplus_nl,aplus_nl);
     int nc=numCols();
     int nr=numRows();
     int offset;
     
     for(int i=0;i<(int)r->n;i++)
      {
	row=(int)r->p[i];
	offset=row*nc;
        if (c->n<=2)
	 {
	   for(int j=0;j<(int)c->n;j++)
	    {
	      col=(int)c->p[j];
	      color=(useColor==MSTrue)?(int)colorA->p[offset+col]:0;
	      bold=(useBold==MSTrue&&boldA->p[offset+col]==1)?MSTrue:MSFalse;
	      uline=(useUline==MSTrue&&ulineA->p[offset+col]==1)?MSTrue:MSFalse;
	      drawRow(row,col,p.c+(offset+col),1,color,bold,Normal,uline);
	    }
	 }
        else
	 {
           int sc=(int)c->p[0];
           int ec=(int)c->p[0];
           int j;
           for (j=0;j<(int)c->n;j++)
            {
              sc=(c->p[j]<sc)?(int)c->p[j]:sc;
	      ec=(c->p[j]>ec)?(int)c->p[j]:ec;
            }	   
           ec=(ec<nc)?ec:nc-1;
	   sc=(sc>=0)?sc:0;
           int len=0;
           for (j=sc;j<=ec;j+=len)
	    {
	      color=(useColor==MSTrue)?(int)colorA->p[offset+j]:0;
	      bold=(useBold==MSTrue&&boldA->p[offset+j])?MSTrue:MSFalse;
	      uline=(useUline==MSTrue&&ulineA->p[offset+j]==1)?MSTrue:MSFalse;
	      len=1;
	      while(j+len<=ec)
	       {
		 nextColor=(useColor==MSTrue)?(int)colorA->p[offset+(j+len)]:0;
		 nextBold=(useBold==MSTrue&&boldA->p[offset+(j+len)]==1)?MSTrue:MSFalse;
		 nextUline=(useUline==MSTrue&&ulineA->p[offset+(j+len)]==1)?MSTrue:MSFalse;
		 if (nextColor==color&&nextBold==bold&&nextUline==uline) len++;
		 else break;
	       }
	      drawRow(row,j,p.c+(offset+j),len,color,bold,Normal,uline);
	    }
	 }
      }
     dc(colorA);
     dc(boldA);
     dc(ulineA);
   }
}

MSBoolean AplusPage::verifyBlink(A b_)
{
  return verifyA(b_);
}

// called after each data update
void AplusPage::blinkUpdate(void)
{
  V v    = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  if (blinkFunc()->func()!=0)
   {
     A newBlink=blinkFunc()->invoke(v,aplus_nl);
     if (verifyBlink(newBlink)==MSTrue)
      {
        MSBoolean stopTimer=MSTrue;
	if (blinkTimer()!=0)
	 {
	   if (blinkPhase()==Normal)
	    {
	      for (int i=0;i<newBlink->n;i++)
	       {
		 if (newBlink->p[i]==1) 
		  {
		    stopTimer=MSFalse;
		    break;
		  }
	       }
	    }
	   else // reverse
	    {
	      if (isNull(blink())==MSFalse)
	       {
		 int k=0;
		 P p; p.i=((AplusModel*)model())->data();
                 int delta;
		 int color;
		 MSBoolean bold;
		 MSBoolean uline;
		 A colorA=indexFunc()->invoke(v,aplus_nl);
		 A boldA=boldFunc()->invoke(v,aplus_nl);
		 A ulineA=underlineFunc()->invoke(v,aplus_nl);
		 MSBoolean useColor=verifyA(colorA);
		 MSBoolean useBold=verifyA(boldA);
		 MSBoolean useUline=verifyA(ulineA);

		 for (int i=0;i<newBlink->d[0];i++)        
		  {
		    for (int j=0;j<newBlink->d[1];j++)
		     {
		       if (newBlink->p[k]==1) stopTimer=MSFalse;
                       delta=(int)(newBlink->p[k]-blink()->p[k]);
		       if (delta==-1||delta==1)
			{
			  color=(useColor==MSTrue)?(int)colorA->p[k]:0;
			  bold=(useBold==MSTrue&&boldA->p[k]==1)?MSTrue:MSFalse;
			  uline=(useUline==MSTrue&&ulineA->p[k]==1)?MSTrue:MSFalse;
			  drawRow(i,j,(char *)p.c+k,1,color,bold,
                                  (delta==1)?Reverse:Normal,uline);
			}
		       k++;
		     }
		  }
		 dc(colorA);
		 dc(boldA);
		 dc(ulineA);     
	       }
	    }
	   blink(newBlink);
	   if (stopTimer==MSTrue) stopBlinkTimer();
	 }
	else // timer not running
	 {
	   if (drawBlink(newBlink,Reverse)==MSTrue)
	    {
              blinkPhase(Reverse);
	      blink(newBlink);
	      startBlinkTimer();
	    }
	 }
      }
     dc(newBlink);
   }
}

void AplusPage::blinkTimeOut(void)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  if (blinkPhase()==Normal)
   {
     A newBlink=blinkFunc()->invoke(v,aplus_nl);
     if (verifyBlink(newBlink)==MSTrue)
      {
	MSBoolean stopTimer=(drawBlink(newBlink,Reverse)==MSTrue)?MSFalse:MSTrue;
	blink(newBlink);
	toggleBlinkPhase();
	if (stopTimer==MSTrue) stopBlinkTimer();
      }
     dc(newBlink);
   }
  else  // reverse
   {
     (void)drawBlink(blink(),Normal);  
     toggleBlinkPhase();
   }
}

MSBoolean AplusPage::drawBlink(A blink_,BlinkPhase phase_)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  MSBoolean status=MSFalse;
  if (verifyBlink(blink_)==MSTrue)
   {
     int k=0;
     P p; p.i=((AplusModel*)model())->data();
     int color,nextColor;
     MSBoolean bold,nextBold;
     MSBoolean uline,nextUline;
     int len;
     int nc=numCols();
     int nr=numRows();

     A colorA=indexFunc()->invoke(v,aplus_nl);
     A boldA=boldFunc()->invoke(v,aplus_nl);
     A ulineA=underlineFunc()->invoke(v,aplus_nl);
     MSBoolean useColor=verifyA(colorA);
     MSBoolean useBold=verifyA(boldA);
     MSBoolean useUline=verifyA(ulineA);
     
     for (int i=0;i<blink_->d[0];i++)        
      {
	len=1;
	for (int j=0;j<blink_->d[1];j+=len)
	 {
	   len=1;
	   if ((int)blink_->p[k]==1)
	    {
	      status=MSTrue;
	      color=(useColor==MSTrue)?(int)colorA->p[k]:0;
	      bold=(useBold==MSTrue&&boldA->p[k]==1)?MSTrue:MSFalse;
	      uline=(useUline==MSTrue&&ulineA->p[k]==1)?MSTrue:MSFalse;
	      
	      while(j+len<nc)
	       {
		 if ((int)blink_->p[k+len]==1)
		  {
		    nextColor=(useColor==MSTrue)?(int)colorA->p[k+len]:0;
		    nextBold=(useBold==MSTrue&&boldA->p[k+len]==1)?MSTrue:MSFalse;
		    nextUline=(useUline==MSTrue&&ulineA->p[k+len]==1)?MSTrue:MSFalse;
		    if (nextColor==color&&nextBold==bold&&nextUline==uline) len++;
		    else break;
		  }
		 else break;
	       }              
	      drawRow(i,j,(char *)p.c+k,len,color,bold,phase_,uline);
	    }
	   k+=len;
	 }
      }
     dc(colorA);
     dc(boldA);
     dc(ulineA);     
   }
  if (status==MSTrue) drawBoxes(MSFalse);
  return status;
}

void AplusPage::screenUpdate(A index_)
{
  if (mapped()==MSTrue)
   {
     A r=index_->t==It?index_:index_->n?(A)*index_->p:aplus_nl;
     A c=index_->t==Et&&index_->n>1?(A)index_->p[1]:aplus_nl;
     
     if (isNull(r)==MSTrue&&isNull(c)==MSTrue) drawAllRowsCols(index_);
     else if (isNull(r)==MSTrue) drawAllRows(index_);
     else if (isNull(c)==MSTrue) drawAllCols(index_);
     else drawIndexed(index_);
     drawLines(MSFalse);
     blinkUpdate();
     drawBoxes(MSFalse);
   }
}

void AplusPage::drawRow(int row_,int col_,const char *str_,int len_,
                     int index_,MSBoolean bold_,BlinkPhase phase_,MSBoolean underline_)
{
  if (str_!=0)
   {
     int y=computeYCoord(row_);
     int x=computeXCoord(row_,col_>=0?col_:0);
     int tw=len_*charWidth();
     int lastRowAdjustment=(numRows()-1==row_)?1:0;
     int lastColAdjustment=(numCols()==col_+len_)?1:0;
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
       XDrawLine(display(),window(),textGC(),x,y+textDescent()-1,x+tw-1,y+textDescent()-1);
   }
}

void AplusPage::screenRedraw(void)
{
  A a = (model()!=0)?((AplusModel*)model())->a():0;
  if (mapped()==MSTrue&&a!=0)
   {
     screenUpdate(aplus_nl);
     drawLines(MSFalse);
     drawBoxes(MSFalse);
   }
}

int AplusPage::yToRow(int y_)
{
  y_-=highlightThickness()+shadowThickness()+margin();     
  if (y_<0) return 0;
  else if (y_>drawHeight()) return numRows();
  return (y_/textHeight());
}

int AplusPage::xToCol(int x_)
{
  x_-=highlightThickness()+shadowThickness()+margin();     
  if (x_<0) return 0;
  else if (x_>drawWidth()) return numCols();
  return (x_/charWidth());
}

void AplusPage::buttonPress(const XEvent *event_)
{
  x_cursor(xToCol(event_->xbutton.x));
  y_cursor(yToRow(event_->xbutton.y));
  if (sensitive()==MSTrue&&event_->xbutton.button==Button1&&
      acceptFocus()==MSTrue&&traverseFocus(this)==MSTrue) 
   {
     unsigned int keys;
     unsigned int mask=Button1Mask;
     MSBoolean      moved=MSTrue;  // change in spec--always produce callback
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
        ys=(y+ys<=numRows())?ys:numRows()-y;
        xs=(x+xs<=numCols())?xs:numCols()-x;
        rubberBand()->configure(x,y,xs,ys);
	activateCallback(MSSymbol("rband"));
      }
     else rubberBand()->configure(0,0,0,0);
   }
  else if (sensitive()==MSTrue)
   {
     if (event_->xbutton.button==Button2)
     {
       activateCallback(MSSymbol("button2down"));
     }
     else if (event_->xbutton.button==Button3)
     {
       activateCallback(MSSymbol("button3down"));
     }
   } 
}

void AplusPage::buttonRelease(const XEvent *event_)
{
  if (sensitive()==MSTrue)
   {
     if (event_->xbutton.button==Button2)
     {
       activateCallback(MSSymbol("button2up"));
     }
     else if (event_->xbutton.button==Button3)
     {
       activateCallback(MSSymbol("button3up"));
     }
   } 
}

void AplusPage::drawBoxes(MSBoolean clear_)
{
  if (isNull(boxMatrix())==MSFalse)
   {
     int x,y,ys,xs,w,h;
     int k=0;
     int offset=highlightThickness()+shadowThickness()+margin();
     for(int i=0;i<boxMatrix()->d[0];i++)
      {
        y=(int)boxMatrix()->p[k];
	x=(int)boxMatrix()->p[k+1];
        ys=(int)boxMatrix()->p[k+2];
	xs=(int)boxMatrix()->p[k+3];
        h=ys*textHeight();
	w=xs*charWidth();
        y=offset+y*textHeight();
	x=offset+x*charWidth();
        (clear_==MSTrue)?XSetForeground(display(),textGC(),background()):
                       XSetForeground(display(),textGC(),boxColor(i));
        k+=(int)boxMatrix()->d[1];           
	drawRect(textGC(),x,y,w,h);
      }
   }
}

void AplusPage::drawLines(MSBoolean clear_)
{
  if (isNull(lineMatrix())==MSFalse && lineMatrix()->d[0]>0)
   {
     int x,y,w,h,ys,xs;
     int k=0;
     int offset=highlightThickness()+shadowThickness()+margin();
     int lw;
     int delta;
     for(int i=0;i<lineMatrix()->d[0];i++)
      {
        y=(int)lineMatrix()->p[k];
	x=(int)lineMatrix()->p[k+1];
        ys=(int)lineMatrix()->p[k+2];
	xs=(int)lineMatrix()->p[k+3];
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
        k+=(int)lineMatrix()->d[1];           
        (clear_==MSTrue)?XSetForeground(display(),textGC(),background()):
                       XSetForeground(display(),textGC(),foreground());
        XFillRectangle(display(),window(),textGC(),x,y,w,h);
      }
   }
}

A AplusPage::boxColorVector(void)
{
  return (isNull(boxColors())==MSTrue) ? aplus_nl : (A)ic(boxColors());
}

void AplusPage::boxColorVector(A colors_)
{
  if (QA(colors_))
   {
     drawBoxes(MSTrue);
     if (colors_->t==It&&colors_->r==1)
      {
        if (isNull(boxColors())==MSFalse) dc(boxColors());
	_boxColors=(A)ic(colors_);
      }
     else if (isNull(colors_)==MSTrue)
      {
        if (isNull(boxColors())==MSFalse) dc(boxColors());
        _boxColors=aplus_nl;        
      }
     else cerr << "Page Widget: invalid bounding box color vector specified." << endl;
     drawBoxes(MSFalse);
   }
}

A AplusPage::lines(void)
{
  return (isNull(lineMatrix())==MSTrue) ? aplus_nl : (A)ic(lineMatrix());
}

void AplusPage::lines(A lines_)
{
  if (QA(lines_))
   {
     if (lines_->t==It&&lines_->r==2&&lines_->d[1]==4)
      {
        dc(lineMatrix());
	_lineMatrix=(A)ic(lines_);
      }
     else if (isNull(lines_)==MSTrue)
      {
        dc(lineMatrix());
        _lineMatrix=aplus_nl;        
      }
     else cerr << "Page Widget: invalid line specified." << endl;
     redraw();
   }
}

A AplusPage::boxes(void)
{
  return (isNull(boxMatrix())==MSTrue) ? gm(It,0,4) : (A)ic(boxMatrix());
}

void AplusPage::boxes(A boxes_)
{
  if (QA(boxes_))
   {
     if (boxes_->t==It&&boxes_->r==2&&boxes_->d[1]==4)
      {
        dc(boxMatrix());
	_boxMatrix=(A)ic(boxes_);
      }
     else if (isNull(boxes_)==MSTrue)
      {
        dc(boxMatrix());
        _boxMatrix=aplus_nl;        
      }
     else cerr << "Page Widget: invalid bounding box specified." << endl;
     redraw();
   }
}

A AplusPage::keyBuf(void)
{
  return (isNull(keyBuffer())==MSTrue) ? aplus_nl : (A)ic(keyBuffer());
}

void AplusPage::keyPress(const XEvent *,KeySym k_,unsigned int s_,const char *b_)
{
  if (sensitive()==MSTrue)
   {
     if (strlen(b_)>0)
      {
        if (isNull(keyBuffer())==MSFalse) dc(keyBuffer());
	_keyBuf=gv(Et,2);
	A k=gs(It);
	k->p[0]=(I)(0xff&k_);
	A state=gv(It,8);
        unsigned int mask=ShiftMask;
        for (int i=0;i<8;i++,mask<<=1) state->p[i]=(I)((mask&s_)?1:0);
        _keyBuf->p[0]=(I)k;
	_keyBuf->p[1]=(I)state;
	activateCallback(MSSymbol("keypress"));
      }
   }
}

void AplusPage::addSenderNotify(MSEventSender *m_)
{
  INTERNAL_COUPLE(((AplusModel *) m_));
}

void AplusPage::receiveEvent(MSEvent &event_)
{
  if (event_.type() == AplusEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received UpdateEvent in AplusPage"  << endl;
     AplusEvent *ave = (AplusEvent *) &event_;
     V v     = ((AplusModel *)model())->aplusVar();
     A index = ave->index();
     A pick  = ave->pick();
     I ravel = ave->ravel();;
     update(v,index, pick, ravel);
   }
  if (event_.type() == AplusVerifyEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received VerifyEvent in AplusPage"  << endl;

     AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
     ave->result(verifyData(ave->aplusVar(), ave->a()));
   }
}


void AplusPage::print(const char *file_)
{
  MSBoolean fileOpen=MSFalse;
  MSBoolean open=MSTrue; 

  setBusyState(MSTrue);
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
  setBusyState(MSFalse);
}


const MSSymbol& AplusPage::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusPage::symbol(void)
{
  static MSSymbol sym("AplusPage");
  return sym;
}

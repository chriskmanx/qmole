///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/AplusMatrix.H>

static const int AplusMatrixDefaultColumnSpacing = 9;

AplusMatrix::AplusMatrix(MSWidget *w_) : AplusArray(w_)
{
  unsigned long icolor = AVariableData::defaultMatrixColor();
  _numHeadings=1;
  _labelLength=AplusMatrixDefaultColumnSpacing;
  _spaceVector=aplus_nl;
  _rowIndex=(A)gv(It,0);
  _colIndex=(A)gv(It,0);
  _cornerIndex=MSFalse;
  _rowIndexBg    = icolor;
  _colIndexBg    = icolor;
  _cornerIndexBg = icolor;
  _rowIndexXGC=0;
  _colIndexXGC=0;
  _cornerIndexXGC=0;
}

AplusMatrix::~AplusMatrix(void)
{
  if (rowIndexXGC()!=0)    delete _rowIndexXGC;
  if (colIndexXGC()!=0)    delete _colIndexXGC;  
  if (cornerIndexXGC()!=0) delete _cornerIndexXGC;  
  dc(spaceVector());
  dc(rowIndex());
  dc(colIndex());
}

MSBoolean AplusMatrix::verifyData(V v_,A a_)
{
  return (0!=v_&&0!=a_&&QA(a_)&&(Et==a_->t||((a_->t==Ft||a_->t==It)&&a_->r==2)))?MSTrue:MSFalse;
}

void AplusMatrix::spaceFunc(AFunc func_,AClientData *arg_)
{
  spaceFunc()->func(func_);
  spaceFunc()->arg(arg_); 
  updateColSpace();
  redrawImmediately();
}

void AplusMatrix::colSpace(A a_)
{
  dc(spaceVector());
  _spaceVector=aplus_nl;
  if (a_!=0&&a_->t==It&&a_->r<=1) _spaceVector=(A)ic(a_);
  if (isNull(spaceVector())==MSFalse) _labelLength=(int)spaceVector()->p[0];
  else 
   {
     V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
     if (v)
      {
	AVariableData *vd = ::pAVarDataFromV(v);
	if (vd!=0) _labelLength=vd->colWidth();
	else _labelLength=AplusMatrixDefaultColumnSpacing;
      }
   }
  sizeUpdate();
}

void AplusMatrix::numHeadings(int n_) 
{
  if (n_!=numHeadings())
   {
     _numHeadings=n_;placement();
     redrawImmediately();
   }
}


// We have to override redrawImmediately() to ensure that the corner label gets drawn
// every time the window is redrawn.
//
void AplusMatrix::redrawImmediately(void)
{
  MSArrayView::redrawImmediately();
  drawLabel(panner()->window());
}

void AplusMatrix::drawLabel(Window xid_)
{
  if (mapped()==MSTrue&&frozen()==MSFalse)
   {
     int offset=panner()->highlightThickness()+panner()->shadowThickness();
     int y=offset;
     int x=offset;
     int nc=numColumns();
     MSRect rect;    
     
     A str=rowLabel(-1);
     int cw=labelWidth();
     int w=panner()->width()-(offset<<1); 
     XFillRectangle(display(),xid_,(cornerIndex()==MSTrue)?cornerIndexGC():backgroundShadowGC(),
		    x,y,(cw>w)?w:cw-columnSpacing(),headingsHeight()-rowSpacing());
     if (isNull(str)==MSFalse && ((str->t==Ct&&str->r<=2)||(str->t==Et)))
      {
	unsigned long fg=labelColor(-1);
	Font fid=labelFont(-1);
	int len=labelLength();
	rect.configuration(x+columnSpacing(),y+rowSpacing(),
			   (cw>w)?w:cw,headingsHeight()-(rowSpacing()<<1));
	drawFieldHeading(xid_,str,fg,fid,rect);
      }
     if (str!=0) dc(str);
   }
}

void AplusMatrix::drawLabels(Window xid_,int rs_,int re_)
{
  if (mapped()==MSTrue&&frozen()==MSFalse&&inRowRange(rs_)==MSTrue&&inRowRange(re_)==MSTrue)
   { 
     int po=panner()->highlightThickness()+panner()->shadowThickness();	   
     int x=po;
     int y=computeYCoord(rs_);
     int iy=y;
     int nr=numRows();
     int w=labelWidth(),h;
     int rthickness=(rowSpacing()>>1);
     int cthickness=(columnSpacing()>>1);
     int n=0;
     int count=re_-rs_+1;
     XRectangle *top=new XRectangle[count];
     XRectangle *bottom=new XRectangle[count];     
     A str;
     int len=labelLength();
     
     w=(w<=panner()->width()-(po<<1))?w:panner()->width()-(po<<1);   

     XRectangle clipRect[1];
     clipRect[0].x=0;
     clipRect[0].y=0;
     clipRect[0].width=w;
     clipRect[0].height=rowHeight();
     for (int i=rs_;i<=re_&&i<nr;i++)
      {
        str=rowLabel(i);
        if (needRowSep(i)==MSTrue)
	 {
           h=rowHeight()-rowSpacing();
           top[n].x=x;
           top[n].y=y+h;
           top[n].height=rthickness;
           top[n].width=w;	   
           bottom[n].x=x;
           bottom[n].y=top[n].y+rthickness;
           bottom[n].height=rthickness;
           bottom[n].width=w;	  
           n++; 
	 }
        else h=rowHeight();
        XFillRectangle(display(),xid_,
                       (isRowIndexed(i)==MSTrue)?rowIndexGC():backgroundShadowGC(),
                       x,y,w,h);
	XSetForeground(display(),textGC(),labelColor(i));
	XSetFont(display(),textGC(),labelFont(i));
	XSetClipRectangles(display(),textGC(),x,y,&clipRect[0],1,Unsorted);
	XDrawString(display(),xid_,textGC(),textFontStruct(),x+columnSpacing(),
		    y+textAscent()+rowSpacing(),(char *)str->p,(int)str->n);
        y+=rowHeight();         
        dc(str);
      }
     clipRect[0].width=panner()->width()-(po<<1);
     clipRect[0].height=panner()->height()-(po<<1);
     XSetClipRectangles(display(),textGC(),po,po,&clipRect[0],1,Unsorted);
     if (n>0)
      {
	XBFillRectangles(display(),xid_,bottomShadowGC(),top,n);
	XFillRectangles(display(),xid_,topShadowGC(),bottom,n);
      }
     if (y>iy&&cthickness>0)
      {
        x+=labelWidth();
        if (x>panner()->width()-po) x=panner()->width()-po; 
        x-=columnSpacing();
	XBFillRectangle(display(),xid_,bottomShadowGC(),
			x,po,columnSpacing(),panner()->height()-(po<<1));
      }
     delete [] top;
     delete [] bottom;     
   }
}

void AplusMatrix::drawHeadings(Window xid_,int cs_,int ce_)
{
  if (mapped()==MSTrue&&frozen()==MSFalse&&inColRange(cs_)==MSTrue&&inColRange(ce_)==MSTrue)
   { 
     drawFieldHeadings(xid_,cs_,ce_);
   }
}


void AplusMatrix::drawFieldHeadings(Window xid_,int cs_,int ce_)
{
  V v = (model()!=0)?((AplusModel *)model())->aplusVar():0;
  A a = (model()!=0)?((AplusModel *)model())->a():aplus_nl;
  
  if (v!=0 && isNull(a)==MSFalse) 
   {
     int offset=panner()->highlightThickness()+panner()->shadowThickness();
     int y=offset;
     int x=computeXCoord(cs_) - (columnSpacing()<<1);
     int len=0,cw=0;
     int j,w=0;
     int nc=numColumns();
     MSRect rect;    
     unsigned long fg;
     Font fid;
     A str;
     
     for (j=cs_;j<=ce_&&j<nc;j++)
      {
        str=heading(j);
	cw=columnPixelWidth(j); 
	w=panner()->width()-(offset<<1); 
	if ((str->t==Ct&&str->r<=2)||(str->t==Et))
	 {
	   fg=headingColor(j);
	   fid=headingFont(j);
           len=columnLength(j);
           XFillRectangle(display(),xid_,
                          (isColIndexed(j)==MSTrue)?colIndexGC():backgroundShadowGC(),
                          x,y,(cw>w)?w:cw,headingsHeight()-rowSpacing());
           if(isNull(str)==MSFalse)
	     {
	       rect.configuration(x,y+rowSpacing(), (cw>w)?w:cw,headingsHeight()-2*rowSpacing());
	       drawFieldHeading(xid_,str,fg,fid,rect);
	     }
	 }
	x+=cw;
        if (str!=0) dc(str);
      }
     if (w>0) XBFillRectangle(display(),xid_,bottomShadowGC(), 
	 	              offset,offset+headingsHeight()-rowSpacing(),w,rowSpacing());
   }
}

A AplusMatrix::rowLabel(int row_)
{
  static MSString buf;
  A outStr=aplus_nl;
  V v = (model()!=0)?((AplusModel *)model())->aplusVar():0;

  if (v!=0)
   {
     AOutFunction *titleFunc=AplusModel::getTitleFunc(v);
     if (titleFunc!=0) 
      {
        outStr=(A)titleFunc->invoke(v,(A)aplus_nl,row_,-1);   
        outStr=(outStr->t==Ct)?outStr:aplus_nl;
      }
     else if (row_>=0)
      {
	buf = MSInt(row_).asString();
	outStr=gsv(0,(char *)(const char *) buf);
      }
   }
  return outStr;
}

Font AplusMatrix::labelFont(int row_)
{
  V v = (model()!=0)?((AplusModel *)model())->aplusVar():0;

  if (v!=0)
   {
     AFontFunction *titleFontFunc=AplusModel::getTitleFontFunc(v);
     if (titleFontFunc!=0) return titleFontFunc->invoke(v,(A)aplus_nl,row_,-1);
   }
  return font();
}

unsigned long AplusMatrix::labelColor(int row_)
{
  V v = (model()!=0)?((AplusModel *)model())->aplusVar():0;
  if (v!=0)
   {
     AColorFunction *titleColorFunc=AplusModel::getTitleColorFunc(v);
     if (titleColorFunc!=0) return titleColorFunc->invoke(v,(A)aplus_nl,row_,-1);
   }
  return foreground();
}

A AplusMatrix::heading(int col_)
{
  V v = (model()!=0)?((AplusModel *)model())->aplusVar():0;
  A outStr=aplus_nl;
  if (v!=0)
   {
     AOutFunction *titleFunc=AplusModel::getTitleFunc(v);
     if (titleFunc!=0) outStr=(A)titleFunc->invoke(v,(A)aplus_nl,-1,col_);   
     else
      {
	int ci=(col_!=0)?(col_%26):0;
	int l=(col_/26)+1;
	char *cp=new char[l+1];
	int i ;
	for (i=0;i<l;i++) cp[i]=(ci+'A');
	cp[i]='\0';
	outStr=gsv(0,cp);
	delete [] cp;
      }
   }
  return outStr;
}

Font AplusMatrix::headingFont(int col_)
{
  V v = (model()!=0)?((AplusModel *)model())->aplusVar():0;
  if (v!=0)
   {
     AFontFunction *titleFontFunc=AplusModel::getTitleFontFunc(v);
     if (titleFontFunc!=0) return titleFontFunc->invoke(v,(A)aplus_nl,-1,col_);
   }
  return font();
}

unsigned long AplusMatrix::headingColor(int col_)
{
  V v = (model()!=0)?((AplusModel *)model())->aplusVar():0;
  if (v!=0)
   {
     AColorFunction *titleColorFunc=AplusModel::getTitleColorFunc(v);
     if (titleColorFunc!=0) return titleColorFunc->invoke(v,(A)aplus_nl,-1,col_);
   }
  return foreground();
}

void AplusMatrix::drawFieldHeading(Window xid_,A str_,unsigned long fg_, Font fid_, MSRect& rect_)
{
  P p; p.i=str_->p;
  char *cp;
  int nc=0;
  int xdelta,ydelta;
  int y,tw,cw=rect_.width();
  
  XSetForeground(display(),textGC(),fg_);
  XSetBackground(display(),textGC(),background());
  XSetFont(display(),textGC(),fid_);

  if (str_->t==Ct)
   {
     if (str_->r<=1)
      {
        nc=(str_->r==0)?1:(int)str_->n;
        if (nc>0)
	 {
	   tw=XTextWidth(textFontStruct(),p.c,nc);
	   ydelta=rect_.height()-textHeight();
	   ydelta=(ydelta>0)?ydelta>>1:0;
           if (tw>cw) 
            {
              nc=computeMaxTextLength((XFontStruct*)textFontStruct(),p.c,cw,nc);
              xdelta=0;
	    }
           else xdelta=(cw>tw)?(cw-tw)>>1:0;
	   XDrawString(display(),xid_,textGC(),textFontStruct(),
		       rect_.x()+xdelta,rect_.y()+ydelta+textFontStruct()->max_bounds.ascent,p.c,nc);
	 }
      }
     else
      {
        ydelta=rect_.height()-numHeadings()*textHeight();
        ydelta=(ydelta>0)?ydelta>>1:0;
        y=rect_.y()+ydelta;
	for(int i=0;i<str_->d[0]&&i<numHeadings();i++)
	 {
	   nc=(int)str_->d[1];     
           cp=p.c+(i*nc);
	   tw=XTextWidth(textFontStruct(),cp,nc);
           if (tw>cw) 
            {
              nc=computeMaxTextLength((XFontStruct*)textFontStruct(),cp,cw,nc);
              xdelta=0;
	    }
           else xdelta=(cw>tw)?(cw-tw)>>1:0;
	   XDrawString(display(),xid_,textGC(),textFontStruct(),
                       rect_.x()+xdelta,y+textFontStruct()->max_bounds.ascent,cp,nc);
	   y+=textHeight();
	 }
      }
   }
  else if (str_->t==Et)
   {
     ydelta=rect_.height()-numHeadings()*textHeight();
     ydelta=(ydelta>0)?ydelta>>1:0;
     y=rect_.y()+ydelta;
     for (int i=0;i<str_->n&&i<numHeadings();i++)
      {
        tw=0;
        nc=0;
        cp=(char *)p.a[i]->p;
	if (p.a[i]->r==0) 
	 {
           nc=1;
	   tw=XTextWidth(textFontStruct(),cp,nc);
	 }
	else if (p.a[i]->r==1) 
	 {
           nc=(int)p.a[i]->n;
	   tw=XTextWidth(textFontStruct(),cp,nc);
	 }
        if (nc>0)
	 {
           if (tw>cw) 
            {
              nc=computeMaxTextLength((XFontStruct*)textFontStruct(),cp,cw,nc);
              xdelta=0;
	    }
           else xdelta=(cw>tw)?(cw-tw)>>1:0;
	   XDrawString(display(),xid_,textGC(),textFontStruct(),
                       rect_.x()+xdelta,y+textFontStruct()->max_bounds.ascent,cp,nc);
	 }
	y+=textHeight();
      }
   }
}

unsigned AplusMatrix::columnLength(unsigned col_)
{
  V v = (model()!=0)?((AplusModel *)model())->aplusVar():0;
  
  if (v!=0&&col_>=0)
   {
     if (isNull(spaceVector())==MSFalse) 
      {
        if (col_+1<spaceVector()->n) return (int)spaceVector()->p[col_+1];
	else return labelLength();
      }
   }     
  return AplusMatrixDefaultColumnSpacing;
}

MSBoolean AplusMatrix::isRowIndexed(int row_)
{
  A sv=rowIndex();
  for (int i=0;i<sv->n;i++) { if (sv->p[i]==row_) return MSTrue; }
  return MSFalse;
}
MSBoolean AplusMatrix::isColIndexed(int col_)
{
  A sv=colIndex();
  for (int i=0;i<sv->n;i++) { if (sv->p[i]==col_) return MSTrue; }
  return MSFalse;
}

void AplusMatrix::rowIndex(A sv_)
{
  if ((sv_->t==It&&sv_->r<=1)||(sv_->t==Et&&sv_->n==0))
   {
     A sv=rowIndex();
     int i,r;
     _rowIndex=(A)ic(sv_);
     for (i=0;i<sv->n;i++) 
      { 
        r=(int)sv->p[i];
        if (isRowIndexed(r)==MSFalse) drawLabels(panner()->window(),r,r); 
      }
     dc(sv);
     sv=rowIndex();
     if (rowIndexXGC()==0&&sv->n>0) createRowIndexXGC();
     for (i=0;i<sv->n;i++) drawLabels(panner()->window(),(int)sv->p[i],(int)sv->p[i]); 
   }  
}

void AplusMatrix::colIndex(A sv_)
{
  if ((sv_->t==It&&sv_->r<=1)||(sv_->t==Et&&sv_->n==0))
   {
     A sv=colIndex();
     int i,c;
     _colIndex=(A)ic(sv_);
     for (i=0;i<sv->n;i++) 
      { 
        c=(int)sv->p[i];
        if (isColIndexed(c)==MSFalse) drawHeadings(panner()->window(),c,c); 
      }
     dc(sv);
     sv=colIndex();
     if (colIndexXGC()==0&&sv->n>0) createColIndexXGC();
     for (i=0;i<sv->n;i++) drawHeadings(panner()->window(),(int)sv->p[i],(int)sv->p[i]); 
   }  
}

void AplusMatrix::cornerIndex(MSBoolean b_)
{
  if (cornerIndex()!=b_)
   {
     _cornerIndex=b_;
     if (cornerIndexXGC()==0) createCornerIndexXGC();
     drawLabel(panner()->window());
   }  
}

void AplusMatrix::rowIndexBg(unsigned long bg_)
{
  if (rowIndexBg()!=bg_)
   {
     _rowIndexBg=bg_;
     if (rowIndexXGC()==0) createRowIndexXGC();
     else rowIndexXGC()->foreground(rowIndexBg());
     A sv=rowIndex();
     for (int i=0;i<sv->n;i++) 
       drawLabels(panner()->window(),(int)sv->p[i],(int)sv->p[i]); 
   }
}

void AplusMatrix::colIndexBg(unsigned long bg_)
{
  if (colIndexBg()!=bg_)
   {
     _colIndexBg=bg_;
     if (colIndexXGC()==0) createColIndexXGC();
     else colIndexXGC()->foreground(colIndexBg());
     A sv=colIndex();
     for (int i=0;i<sv->n;i++) 
       drawHeadings(panner()->window(),(int)sv->p[i],(int)sv->p[i]); 
   }
}

void AplusMatrix::cornerIndexBg(unsigned long bg_)
{
  if (cornerIndexBg()!=bg_)
   {
     _cornerIndexBg=bg_;
     if (cornerIndexXGC()==0) createCornerIndexXGC();
     else cornerIndexXGC()->foreground(cornerIndexBg());
     drawLabel(panner()->window());
   }
}

void AplusMatrix::createRowIndexXGC(void)
{
  if (rowIndexXGC()==0)
   {
     XGCValues values;    
     values.foreground=rowIndexBg();
     _rowIndexXGC=new MSGC(server(),MSTrue,&values,GCForeground);
   }
}

void AplusMatrix::createColIndexXGC(void)
{
  if (colIndexXGC()==0)
   {
     XGCValues values;    
     values.foreground=colIndexBg();
     _colIndexXGC=new MSGC(server(),MSTrue,&values,GCForeground);
   }
}

void AplusMatrix::createCornerIndexXGC(void)
{
  if (cornerIndexXGC()==0)
   {
     XGCValues values;    
     values.foreground=cornerIndexBg();
     _cornerIndexXGC=new MSGC(server(),MSTrue,&values,GCForeground);
   }
}

void AplusMatrix::shapeUpdate(void)
{
  updateColSpace();
  AplusArray::shapeUpdate();
}

void AplusMatrix::firstMapNotify(void)
{
  updateColSpace();
  AplusArray::firstMapNotify();
}

void AplusMatrix::updateColSpace(void)
{
  A newColSpace;
  V v = (model()!=0)?((AplusModel *)model())->aplusVar():0;
  A a = (model()!=0)?((AplusModel *)model())->a():aplus_nl;

  if (spaceFunc()->func()!=0)
   {
     freeze();
     newColSpace=spaceFunc()->invoke(v,a);
     colSpace(newColSpace);
     dc(newColSpace);
     unfreeze();
   }
}

void AplusMatrix::updateTitle(void)
{
  AplusArray::updateTitle();
  if (mapped()==MSTrue&&frozen()==MSFalse)
   {
     drawLabel(panner()->window());
     drawHeadings(panner()->window(),firstColumn(),lastColumn());   
     drawLabels(panner()->window(),firstRow(),lastRow());
   }
}


void AplusMatrix::sizeUpdate(void) 
{
  if (mapped()==MSTrue)
   {
     adjustNumVisible();
     redrawImmediately();
   }
}


const MSSymbol& AplusMatrix::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusMatrix::symbol(void)
{
  static MSSymbol sym("AplusMatrix");
  return sym;
}

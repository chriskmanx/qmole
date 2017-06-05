///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSFloatVector.H>
#include <MSGUI/MSGraph.H>
#ifndef MSStringVectorTraceSet
#include <MSGUI/MSStringVectorTraceSet.H>
#endif
#ifndef MSFloatMatrixTraceSet
#include <MSGUI/MSFloatMatrixTraceSet.H>
#endif
#ifndef MSIntMatrixTraceSet
#include <MSGUI/MSIntMatrixTraceSet.H>
#endif

static const int   	DefaultChunkSize=3;
extern unsigned long 	MSGraphEventMask;

/*
 *    MSGraphNewtrace member functions
 *
 */
MSGraphNewtrace::MSGraphNewtrace(MSGraph *owner_) 
{
  _points=0;
  _pointCount=0;
  _pointAllocCount=0;
  _graph=owner_;
}  

MSGraphNewtrace::~MSGraphNewtrace(void) 
{
  if (_points!=0) delete [] _points;
  _pointCount=0;
  _pointAllocCount=0;
}

XPoint *MSGraphNewtrace::points(int index_)
{ 
  if (index_>=pointAllocCount()) pointsAlloc(); 
  return &_points[index_];
}

void MSGraphNewtrace::pointsAlloc(void)
{
  XPoint *newp=new XPoint[_pointAllocCount+DefaultChunkSize];
  for (int i=0;i<_pointAllocCount;i++) 
   {
     newp[i].x=_points[i].x;
     newp[i].y=_points[i].y;
   }
  if (_points!=0) delete [] _points;
  _points=newp;
  _pointAllocCount+=DefaultChunkSize;
}

MSGraphNewtrace *MSGraph::nt(int index_)
{ 
  if (index_>=_newtraceAllocCt) newtraceAlloc(); 
  return _nt[index_];
}

void MSGraph::newtraceAlloc(void)
{
  MSGraphNewtrace **newt=new MSGraphNewtrace*[_newtraceAllocCt+DefaultChunkSize];
  int i;
  for (i=0; i<_newtraceAllocCt; i++)
   {
     newt[i]=_nt[i];
     _nt[i]=0;
   }
  for (i=_newtraceAllocCt; i<_newtraceAllocCt+DefaultChunkSize; i++) 
   {
     newt[i]=new MSGraphNewtrace(this);
   }
  if (_nt!=0) delete [] _nt;
  _nt=newt;
  _newtraceAllocCt+=DefaultChunkSize;
}

void MSGraph::newtraceDealloc(void)
{
  if (_nt!=0) 
   {
     for (int i=0; i<_newtraceAllocCt; i++) delete _nt[i];
     delete [] _nt;
     _nt=0;
   }
  _newtraceCt=0;
  _newtraceAllocCt=0;
  _newtraceIndex=0;
  _selectLine=0;
  _selectPoint=0;
  _focusLine=0;
}

MSBoolean MSGraph::findLineHandle(int x_,int y_)
{
  static int size=8;
  for (int i=0; i<newtraceCt(); i++)
   {
     for (int j=0; j<nt(i)->pointCount(); j++)
      {
	if (((abs(x_-nt(i)->points(j)->x)<size)&&
	     (abs(y_-nt(i)->points(j)->y)<size)))
	 {
	   selectPoint(j);
	   selectLine(i);
	   return MSTrue;
	 }
      }
   }
  return MSFalse;
}

MSBoolean MSGraph::findSelectTraceLineHandle(int x_,int y_)
{
  static int size=8;
  MSTraceSet *ts=selectTrace()->traceSet();

  for (int j=0; j<ts->traceList().count(); j++)
   {
     MSTrace *trace=ts->trace(j);
     for (int i=0; i<ts->dataCount(); i++)
      {
	int x=xValueToPixel(xValue(trace,i),trace->xAxis());
	int y=yValueToPixel(trace->y(i),trace->yAxis());
	if (((abs(x_-x)<size)&&(abs(y_-y)<size)))
	 {
	   selectPoint(j);
	   selectLine(i);
	   return MSTrue;
	 }
      }
   }
  return MSFalse;
}

MSTraceSet *MSGraph::createTraceSet(MSFloatMatrix& aFloatMatrix_,const char *legend_, const MSSymbol& sym_)
{
  MSTraceSet *ts=new MSFloatMatrixTraceSet(this,aFloatMatrix_,legend_,sym_);
  ts->internalModel(MSTrue);
  return ts;
}

MSTraceSet *MSGraph::createTextTraceSet(MSFloatMatrix& aFloatMatrix_,MSStringVector& aStringVector_)
{
  freeze();
  MSStringVectorTraceSet *ts=new MSStringVectorTraceSet(this,aStringVector_);
  ts->textFont(selectTrace()!=0?selectTrace()->traceSet()->textFont():editor()->font());
  if(orientation()==Horizontal)
   {
     ts->moveTo(aFloatMatrix_(0,1),aFloatMatrix_(0,0));
   }
  else ts->moveTo(aFloatMatrix_(0,0),aFloatMatrix_(0,1));
  ts->internalModel(MSTrue);
  unfreeze();
  return ts;
}

MSFloatMatrix MSGraph::createInteractiveTraceData(unsigned long i_)
{
  MSFloatMatrix aFloatMatrix(interactivePixel().rows(),2);
  for (int i=0;i<interactivePixel().rows();i++) 
   {
     aFloatMatrix(i,0)=normalizedPixelToValue(interactivePixel()(i,0),i_&MSTop?1:0);
     aFloatMatrix(i,1)=yPixelToValue(interactivePixel()(i,1),i_&MSRight?1:0);
   }
  return aFloatMatrix;
}

MSTrace *MSGraph::findSelectableTrace(const XEvent *event_)
{
  MSTrace 	*trace;
  int		 x1,y1,x2,y2;
  int 		 sa,sb,left,right,top,bot;
  int 		 x=event_->xbutton.x;
  int 		 y=event_->xbutton.y;
  
  static double range=0.2;
  
  for (int i=0; i<traceList().count(); i++)
   {
     if ((trace=graphTrace(i))!=0&&trace->traceSet()->selectable()==MSTrue)
      {
	for (int k=0; k<trace->dataCount()-1; k++)
	 {
	   x1=xValueToPixel(xValue(trace,k),trace->xAxis());
	   y1=yValueToPixel(trace->y(k),trace->yAxis());
	   x2=xValueToPixel(xValue(trace,k+1),trace->xAxis());
	   y2=yValueToPixel(trace->y(k+1),trace->yAxis());
	   x1+=trace->xOffset(); x2+=trace->xOffset();
	   y1+=trace->yOffset(); y2+=trace->yOffset();
	   
	   sa=x2-x1; sb=y2-y1;
	   left=sa>0?x1:x2;
	   right=sa>0?x2:x1;
	   top=sb>0?y1:y2;
	   bot=sb>0?y2:y1;

	   if ((sa==0&&abs(x-x1)<3&&y<=top&&y>=bot||
	       (sb==0&&abs(y-y1)<3&&x<=right&&x>=left)))
	    {
	      return trace;
	    }
	   else if (sa!=0&&sb!=0&&x<=right&&x>=left&&y>=top&&y<=bot)
	    {
	      double csq=sqrt(double(sa*sa+sb*sb));
	      int asq=x-x1;
	      int bsq=y-y1;
	      double Csq=sqrt(double(asq*asq+bsq*bsq));
	      asq=x-x2;
	      bsq=y-y2;
	      Csq+=sqrt(double(asq*asq+bsq*bsq));
//	      cout<<"csq "<<csq<<" Csq "<<Csq<<" dif "<<Csq-csq<<endl;
	      if ((Csq-csq)<range) return trace;
	    }
	 }
      }
   }
  return 0;
}

MSBoolean MSGraph::findLineSegment(int x_,int y_)
{
  static double range=0.2;
  
  for (int i=0; i<newtraceCt(); i++)
   {
     for (int j=0; j<nt(i)->pointCount()-1; j++)
      {
	if ((nt(i)->sa(j)==0&&abs(x_-nt(i)->points(j)->x)<3&&
	     y_<=nt(i)->top(j)&&y_>=nt(i)->bot(j))||
	    (nt(i)->sb(j)==0&&abs(y_-nt(i)->points(j)->y)<3&&
	     x_<=nt(i)->right(j)&&x_>=nt(i)->left(j)))
	 {
	   selectPoint(j);
	   selectLine(i);
	   return MSTrue;
	 }
	else if (nt(i)->sa(j)!=0&&nt(i)->sb(j)!=0&&x_<=nt(i)->right(j)&&
		 x_>=nt(i)->left(j)&&y_>=nt(i)->top(j)&&y_<=nt(i)->bot(j))
	 {
	   double csq=sqrt(double(nt(i)->sa(j)*nt(i)->sa(j)+nt(i)->sb(j)*nt(i)->sb(j)));
	   int asq=x_-nt(i)->points(j)->x;
	   int bsq=y_-nt(i)->points(j)->y;
	   double Csq=sqrt(double(asq*asq+bsq*bsq));
	   asq=x_-nt(i)->points(j+1)->x;
	   bsq=y_-nt(i)->points(j+1)->y;
	   Csq+=sqrt(double(asq*asq+bsq*bsq));
	   if ((Csq-csq)<range)
	    {
	      selectPoint(j);
	      selectLine(i);
	      return MSTrue;
	    }
	 }
      }
   }
  return MSFalse;
}

void MSGraph::moveLegend(const XEvent *event_)
{
  int			x,y;
  int 			ix,iy;
  int 			rx,ry;
  int 			spacing=2;
  unsigned int		keys,mask=0;
  Window 		root,child;
  MSBoolean             bs;
  int                   lx,ly;
  if (event_->xbutton.button==1)mask=Button1Mask;
  else if (event_->xbutton.button==2)mask=Button2Mask;
  else mask=Button3Mask;
  keys=mask;
  int offset=highlightThickness()+shadowThickness();
  int borderOffset=legend()->offset();
  int xoffset=event_->xbutton.x,yoffset=event_->xbutton.y;
  x=legend()->x_origin()+xoffset; y=legend()->y_origin()+yoffset;
  // following casts for VISUAL C++
  unsigned long limit=legendAlignment()&Outside?(unsigned long)(legendAlignment()&Vertical?HoldY:HoldX):(unsigned long)MSNone;
  if ((bs=backingStore())==MSFalse)
   {
     redrawImmediately();
     selectInput(MSGraphEventMask^ExposureMask);
     lx=legend()->x_origin(); ly=legend()->y_origin();
   }
  while (keys&mask)
   {
     XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
     if (ix!=x||iy!=y)
      {
	if ((ix-xoffset)<offset+spacing+borderOffset)
	 {
	   ix=offset+xoffset+spacing+borderOffset;
	 }
	if ((ix-xoffset)>(width()-offset-legend()->width()-spacing-borderOffset))
	 {
	   ix=width()-offset-legend()->width()+xoffset-spacing-borderOffset;
	 }
	if ((iy-yoffset)>(height()-offset-legend()->height()-spacing-borderOffset))
	 {
	   iy=height()-offset-legend()->height()+yoffset-spacing-borderOffset;
	 }
	if ((iy-yoffset)<(offset+spacing+borderOffset))
	 {
	   iy=offset+yoffset+spacing+borderOffset;
	 }
	if (ix!=x||iy!=y)
	 {
	   legend()->moveTo(limit&HoldX?legend()->x_origin():ix-xoffset,
			    limit&HoldY?legend()->y_origin():iy-yoffset);
	   if (bs==MSFalse)
	    {
	      XCopyArea(display(),graphPixmap(),window(),clearGC(),lx,ly,
			legend()->width(),legend()->height(),lx,ly);
	      lx=legend()->x_origin(); ly=legend()->y_origin();
	      XSync(display(),False);
	    }
	 }
      }
     x=ix; y=iy;
   }
  if (abs(ix-event_->xbutton.x)>4||abs(iy-event_->xbutton.y)>4)
   {
     _xLegendPosition=(ix-xoffset)/(double)width();
     _yLegendPosition=(iy-yoffset)/(double)height();
   }
  selectInput(MSGraphEventMask);
}

void MSGraph::drawMoveTrace(MSTrace *trace_)
{
  if (trace_->style()==Segment) plotSegmentTrace(trace_,window(),windowGC()); 
  else drawMoveLineTrace(trace_); 
}

void MSGraph::drawMoveLineTrace(MSTrace *trace_)
{
  int i,k,xs,ys;
  int x,y;
  int pointCount=0;
  int xLast=0,yLast=0;
  int datacount=trace_->dataCount();
  int inc=datacount<50?1:datacount/50;
  int bufSize=datacount+2;
  MSTrace *trace;
  bufSize=bufSize>MSGraph::_MaxBufSize?MSGraph::_MaxBufSize:bufSize;
  XPoint   *points=new XPoint[bufSize];
  
  for(i=0;i<trace_->traceSet()->traceList().count();i++)
   {
     if((trace=trace_->traceSet()->trace(i))!=0)
      {
        xs=trace->xAxis(); ys=trace->yAxis();
        for (k=0; k<trace->dataCount()&&pointCount+1<bufSize; k+=inc)
         {
           x=xValueToPixel(xValue(trace,k),xs)+trace_->xOffset();
           y=yValueToPixel(trace->y(k),ys)+trace_->yOffset();
           if (x==xLast&&y==yLast) continue;
           xLast=x; yLast=y;
           points[pointCount].x=x;
           points[pointCount++].y=y;
         }
        if (inc!=1&&(datacount-1)%inc!=0)
         { // make sure to draw last point
           k=datacount-1;
           x=xValueToPixel(xValue(trace,k),xs)+trace_->xOffset();
           y=yValueToPixel(trace->y(k),ys)+trace_->yOffset();
           points[pointCount].x=x;
           points[pointCount++].y=y;
         }
        XDrawLines(display(),window(),windowGC(),points,pointCount,CoordModeOrigin);
        drawLineHandles(trace,trace_);
        pointCount=0;
        xLast=yLast=0;
      }
   }
  delete [] points;
}

void MSGraph::drawLineHandles(MSTrace *trace_, MSTrace *refTrace)
{
  XRectangle *rects=new XRectangle[trace_->dataCount()];
  double xoffset=refTrace?refTrace->xOffset():trace_->xOffset();
  double yoffset=refTrace?refTrace->yOffset():trace_->yOffset();
  int xs=trace_->xAxis();
  int ys=trace_->yAxis();
  int bufSize=trace_->dataCount();
  int inc=bufSize<50?1:bufSize/50;
  int x,y,size=6;  
  unsigned i,recs;
  for (i=0,recs=0;i<bufSize;i+=inc,recs+=1)
   {
     x=(int)(xValueToPixel(xValue(trace_,i),xs)+xoffset);
     y=(int)(yValueToPixel(trace_->y(i),ys)+yoffset);
     rects[recs].x=x-size/2;
     rects[recs].y=y-size/2;
     rects[recs].width=size;
     rects[recs].height=size;
   }
  if (inc!=1&&(bufSize-1)%inc!=0)
   { // make sure that handle is drawn for the last point
     i=bufSize-1;
     x=(int)(xValueToPixel(xValue(trace_,i),xs)+xoffset);
     y=(int)(yValueToPixel(trace_->y(i),ys)+yoffset);
     rects[recs].x=x-size/2;
     rects[recs].y=y-size/2;
     rects[recs].width=size;
     rects[recs++].height=size;
   }
  XFillRectangles(display(),window(),windowGC(),rects,recs);
  delete [] rects;
}

MSTrace *MSGraph::findDataPoint(const XEvent *event_)
{
  int 			i,j,k;
  int 			xs,ys;
  double 		d=0;
  double 		r,rr,s;
  double 		x,y;
  MSTrace 	       *trace;
  MSTrace 	       *traceNum=0;
  int 			offset;//lineWidth;
  int			stackCt=0;
  MSTrace 	       **stackTrace=new MSTrace*[traceList().count()<MSGraph::_MaxBufSize?
					      traceList().count():MSGraph::_MaxBufSize];
  
  double distance=_selectDistance*_selectDistance;
  int ix=event_->xbutton.x;
  int iy=event_->xbutton.y;

  // special case for pie chart
  if (graphMode()&PieChart)
   {
     for (i=0; i<traceList().count(); i++)
      {
        if ((trace=graphTrace(i))!=0&&trace->dataCount()>1)
         {
           int point=findSelectedSlice(trace,ix,iy);
           if (point!=-1) 
            {
              traceNum=trace;
              trace->point(point);
              trace->traceSet()->selectRow(point);
              trace->traceSet()->selectCol(trace->virtualCol());
            }
         }
      }
     return traceNum;
   }

  for (i=0; i<traceList().count(); i++)
   {
     if ((trace=graphTrace(i))!=0&&trace->dataCount()>1&&
	 (trace->style()==Stack||trace->style()==Area))
      {
	stackTrace[stackCt++]=trace;
      }
   }
  for (i=0; i<traceList().count(); i++)
   {
     if ((trace=graphTrace(i))!=0 &&
	 trace->dataCount()>1     &&
	 trace->style()!=0        &&
	 trace->traceSet()->isProtected()==MSFalse)
      {
	if (trace->style()==Bar||trace->style()==Stack)
	 {
           // for bar or stack traces calculate bar offset
           offset=(trace->barCount())*barWidth()-xBar();
         }
// 	else if (trace->style()>=Close)
// 	 {
// 	   lineWidth=trace->lineWidth();
// 	 }
	xs=trace->xAxis();
	ys=trace->yAxis();
	double xx=xPixelToValue(ix,xs);
	double yy=yPixelToValue(iy,ys);
	for (k=0; k<trace->dataCount(); k++)
	 {
           x=xValue(trace,k);
           y=yValue(trace,k);
           if (event_->xbutton.button==1&&trace->style()>=Close)	
	    {
	      int xxx=xValueToPixel(x,xs);
	      if (abs(ix-xxx)<=(selectDistance()/2)&&yy<=trace->y(k,1+trace->offset())&&
		  yy>=trace->y(k,1+trace->offset()))
	       {
		 trace->traceSet()->selectRow(k);
		 trace->traceSet()->selectCol(trace->virtualCol());
		 return trace;
	       }
	    }

           // calculate distance by x
	   if (orientation()!=Horizontal&&(trace->style()==Bar||trace->style()==Stack))
	    {
	      s=(int)(xScale(xs)*(xx-x))-offset;
	    }
           else if (orientation()==Horizontal&&(trace->style()==Area||trace->style()==Stack))
            {
              for (j=i,x=0.;j<stackCt;j++)
	       {
		 double xval=xValue(stackTrace[j],k);
		 x+=xx<0?xval<0?xval:0:xval<0?0:xval;
	       }
              s=(xx-x)*xScale(xs);
            }
           else s=(xx-x)*xScale(xs);

           // calculate distance by y
           if (orientation()==Horizontal&&(trace->style()==Bar||trace->style()==Stack))
            {
              r=(yy-y)*yScale(ys)+offset;
            }
           else if (orientation()!=Horizontal&&s*s<distance&&(trace->style()==Area||trace->style()==Stack))
	    {
	      for (j=i,y=0.;j<stackCt;j++)
	       {
		 double yval=stackTrace[j]->y(k);
		 y+=yy<0?yval<0?yval:0:yval<0?0:yval;
	       }
	      r=fabs(yy-y)*yScale(ys);
	    }
	   else if (s*s<distance&&(trace->style()>=Close))
	    {
	      j=trace->offset();
	      r=fabs(yy-y);
	      r=((rr=fabs(yy-trace->y(k,j))))<r?rr:r;
	      r=((rr=fabs(yy-trace->y(k,j+1))))<r?rr:r;
	      if (trace->style()>=HLC) r=((rr=fabs(yy-trace->y(k,j+2))))<r?rr:r;
	      r*=yScale(ys);
	    }
           else r=(yy-y)*yScale(ys);

           // calculate total distance
           r=r*r+s*s;
	   if (((d==0||r<d)&&r<=distance)||((distance==0.0)&&(d==0||r<d)))
	    {
	      d=r;
	      traceNum=trace;
	      trace->point(k);
	      trace->traceSet()->selectRow(k);
	      trace->traceSet()->selectCol(trace->virtualCol());
	    }
	 }
      }
   }
  delete [] stackTrace;
  return(traceNum);
}

void MSGraph::moveDataPoint(const XEvent *event_)
{
  MSBoolean 	update,bs;
  int 		xs,ys;
  unsigned int 	keys,mask=0;
  int 		offset=0;
  int 		ix,iy;
  int 		rx,ry;
  int 		ox,oy;
  int           oxSym,oySym,ixSym,iySym;  // correspond to the position of symbol point
  int    	min_ix,max_ix;
  double 	min_x,max_x;
  MSBoolean	hold_y=MSFalse;
  MSBoolean	hold_x=MSFalse;
  MSBoolean	constrain_x=MSFalse;
  MSRect        r;
  int 		min_iy;
  double 	min_y;
  double 	x,y,xx,yy;
  Window 	root,child;   
  MSTrace        *trace;
  const double 	factor=0.25;
  int           ht=highlightThickness()+shadowThickness();
  if ((trace=findDataPoint(event_))!=0)
   {
     xs=trace->xAxis(); ys=trace->yAxis();
     if (event_->xbutton.button==1) mask=Button1Mask;
     else if (event_->xbutton.button==2) mask=Button2Mask;
     else mask=Button3Mask;
     keys=mask;
     int bx=ox=event_->xbutton.x;
     int by=oy=event_->xbutton.y;
     
   start_again: 
     if ((bs=backingStore())==MSFalse)
      {
	redrawImmediately();
      }
     if (trace->style()==Bar||trace->style()==Stack)
      {
	offset=(trace->barCount())*barWidth()-xBar();
	offset+=xBar()/barCount();
        if (orientation()==Horizontal)
         {
           min_y=yValue(trace,trace->point());
	   min_iy=yValueToPixel(min_y,trace->yAxis());
           hold_y=MSTrue;
         } 
        else
         {
           min_x=xValue(trace,trace->point());
           min_ix=xValueToPixel(min_x,trace->xAxis());
           hold_x=MSTrue;
         }
      }
     else
      {
	if (trace->traceSet()->overlap()==MSFalse)
	 {
	   min_x=xValue(trace,trace->point()-trace->point()>0?1:0);
	   min_ix=xValueToPixel(min_x,trace->xAxis());
	   max_x=xValue(trace,trace->point()+trace->point()<(trace->dataCount()+1)?1:0);
	   max_ix=xValueToPixel(max_x,trace->xAxis());
	 }
	if (trace->traceSet()->constraint()==HoldX)
	 {
	   min_x=xValue(trace,trace->point());
	   min_ix=xValueToPixel(min_x,trace->xAxis());
	   hold_x=MSTrue;
	 }
	if (trace->traceSet()->constraint()==HoldY)
	 {
           min_y=yValue(trace,trace->point());
	   min_iy=yValueToPixel(min_y,trace->yAxis());
	   hold_y=MSTrue;
	 }
      }
     if (constrain_x==MSTrue)
      {
	ox=ox<min_ix?min_ix:ox;
	ox=ox>max_ix?max_ix:ox;
      }
     ox=hold_x==MSTrue?min_ix:ox;
     oy=hold_y==MSTrue?min_iy:oy;

     if (orientation()==Horizontal)
      {
        // offset is used in bar and stack. adjust symbol position depending on orientation
        oxSym=ox;oySym=oy+offset;
      }
     else
      {
        oxSym=ox+offset;oySym=oy;
      }
     
     dataWin()->setDataWin(xStringWidth(),yStringWidth());
     drawScanXYvalues(xValue(trace,trace->point()),yValue(trace,trace->point()),xs,ys);
     drawMoveDataPointValueBox(ox,oy);
     drawMoveDataPointSymbols(trace,oxSym,oySym);
     while (keys&mask)
      {
	update=MSFalse;
	XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
	if (constrain_x==MSTrue)
         {
	   ix=ix<min_ix?min_ix:ix;
	   ix=ix>max_ix?max_ix:ix;
         }
	ix=hold_x==MSTrue?min_ix:ix;
	iy=hold_y==MSTrue?min_iy:iy;
        
        if (trace->autoScale()==MSTrue)
         {
	   if (ix<x_org())
	    {
	      _xMin[xs]=xMinData(xs)-(x_org()/xScale(xs))*factor;
	      _xMax[xs]=xMaxData(xs);
	      update=MSTrue;
	    }
	   if (ix>x_end())
	    {
	      _xMax[xs]=xMaxData(xs)+(x_org()/xScale(xs))*factor;
	      _xMin[xs]=xMinData(xs);
	      update=MSTrue;
	    }
	   if (iy<y_end())
	    {
	      _yMax[ys]=yMaxData(ys)+(y_end()/yScale(ys))*factor;
	      _yMin[ys]=yMinData(ys);
	      update=MSTrue;
	    }
	   if (iy>y_org())
	    {
	      _yMin[ys]=yMinData(ys)-(y_end()/yScale(ys))*factor;
	      _yMax[ys]=yMaxData(ys);
	      update=MSTrue;
	    }
	   if (update==MSTrue)
	    {
	      redrawImmediately();
	      goto start_again;
	    }
         }
        if (orientation()==Horizontal)
         {
           // offset is used in bar and stack. adjust symbol position depending on orientation
           oxSym=ox;oySym=oy+offset;ixSym=ix;iySym=iy+offset;
         }
        else
         {
           oxSym=ox+offset;oySym=oy;ixSym=ix+offset;iySym=iy;
         }
       
	if (ix!=ox||iy!=oy)
         {
	   // The following no backingStore mode needs to fix the
	   // case where the dataWin overlaps the x or y cursor
	   if (bs==MSFalse)
	    {
	      XCopyArea(display(),graphPixmap(),window(),clearGC(),ht,ht,width()-2*ht,height()-2*ht,ht,ht);
              server()->flush();
	    }
	   xx=hold_x==MSTrue?xValue(trace,trace->point()):normalizedPixelToValue(ox,xs);
	   yy=hold_y==MSTrue?yValue(trace,trace->point()):yPixelToValue(oy,ys);
	   drawScanXYvalues(xx,yy,xs,ys);
	   drawMoveDataPointValueBox(ox,oy);
	   if (bs==MSTrue) drawMoveDataPointSymbols(trace,oxSym,oySym);
 	   ox=ix; oy=iy;
	   xx=hold_x==MSTrue?xValue(trace,trace->point()):normalizedPixelToValue(ox,xs);
	   yy=hold_y==MSTrue?yValue(trace,trace->point()):yPixelToValue(oy,ys);
	   drawScanXYvalues(xx,yy,xs,ys);
	   drawMoveDataPointValueBox(ix,iy);
	   drawMoveDataPointSymbols(trace,ixSym,iySym);
	 }
      }

     xx=hold_x==MSTrue?xValue(trace,trace->point()):normalizedPixelToValue(ix,xs);
     yy=hold_y==MSTrue?yValue(trace,trace->point()):yPixelToValue(iy,ys);
     
     drawScanXYvalues(xx,yy,xs,ys);
     drawMoveDataPointValueBox(ix,iy);
     drawMoveDataPointSymbols(trace,ixSym,iySym);
     // calculate current positions depending on mode
     if (orientation()==Horizontal)
      {
        y=yPixelToValue(oy,ys);
        if (trace->style()==Stack||trace->style()==Area)
         {
           //double xOffset=(bx-ox)/xScale(xs);
           double xOffset= x=xPixelToValue(bx,xs)-xPixelToValue(ox,xs);
           x=xValue(trace,trace->point());
           if (x>0&&(x+xOffset)<0) x=xPixelToValue(ox,xs);
           else if(x<0&&(x+xOffset)>0) x=xPixelToValue(ox,xs);
           else x-=xOffset;
         }
        else x=xPixelToValue(ox,xs);
        // in horizontal mode we reverse x and y when updating model
        trace->traceSet()->validate(trace->point(),trace->virtualCol(),y,x);
      }
     else
      {
        x=normalizedPixelToValue(ox,xs);
        if (trace->style()==Stack||trace->style()==Area)
         {
           double yOffset=(by-oy)/yScale(ys);
           y=yValue(trace,trace->point());
           if (y>0&&(y+yOffset)<0)
            {
              int y0=yValueToPixel(0.0,ys);
              yOffset=(by-y0)/yScale(ys);
            }
           y+=yOffset;
         }
        else y=yPixelToValue(oy,ys);
        trace->traceSet()->validate(trace->point(),trace->virtualCol(),x,y);
      }

     dataWin()->lower();
     dataWin()->unmap();
     dataWin()->MSWidget::clear();
   }	
}

void MSGraph::drawMoveDataPointSymbols(MSTrace *trace_,int x_,int y_)
{
  int 	x,y;

  XDrawLine(display(),window(),windowGC(),x_-trace_->symbolSize()/2,y_,
	    x_+trace_->symbolSize()/2,y_);
  XDrawLine(display(),window(),windowGC(),x_,y_-trace_->symbolSize()/2,x_,
	    y_+trace_->symbolSize()/2);
  if (Line&trace_->style()) //||(Line&trace_->style()&&Scatter&trace_->style()))
   {
     if (trace_->point()!=0)
      {
	x=xValueToPixel(xValue(trace_,trace_->point()-1),trace_->xAxis());
	y=yValueToPixel(trace_->y(trace_->point()-1),trace_->yAxis());
	XDrawLine(display(),window(),windowGC(),x_,y_,x,y);
      }	
     if (trace_->point()!=trace_->dataCount()-1)
      {	
	x=xValueToPixel(xValue(trace_,trace_->point()+1),trace_->xAxis());
	y=yValueToPixel(trace_->y(trace_->point()+1),trace_->yAxis());
	XDrawLine(display(),window(),windowGC(),x_,y_,x,y);
      }
   }
}

void MSGraph::drawMoveDataPointValueBox(int x_,int y_)
{
  drawScanXYvalueBox(x_,y_); 
}  

void MSGraph::drawScanXYvalueBox(int x_,int y_)
{
  int xOffset=10;
  int yOffset=10;

  double xx=normalizedPixelToValue(x_,0);
  double yy=yPixelToValue(y_,0);
  int x=(x_+xOffset+dataWin()->width())>width()-offset()?
        width()-offset()-dataWin()->width():x_+xOffset<offset()?offset():x_+xOffset;
  int y=(y_+yOffset+dataWin()->height())>height()-offset()?
        height()-dataWin()->height()-offset():y_+yOffset<offset()?offset():y_+yOffset;
  dataWin()->moveTo(x,y);
#ifdef MS_WINDOWS
  dataWin()->drawHighlight();
#endif
}  

MSTrace *MSGraph::findTextTrace(const XEvent *event_)
{
  XFontStruct  *fi;
  MSTrace      *trace;

  int button_x=event_->xbutton.x;
  int button_y=event_->xbutton.y;
  
  for (int i=0; i<traceList().count(); i++)
   {
     if ((trace=graphTrace(i))!=0&&trace->traceSet()->textLength()!=0&&trace->dataCount()==1)
      {
	MSStringVector aStringVector;
	unsigned lines=trace->traceSet()->textLength();
	MSString formatBuffer;
	for (int j=0;j<lines;j++) aStringVector<<trace->traceSet()->formatText(formatBuffer.removeAll(),j);
        fi=(XFontStruct *)server()->fontStruct(trace->traceSet()->textFont());
	int h=(fi->ascent+fi->descent)*aStringVector.length();
	int w=maxStringWidth(fi,aStringVector);
	int x=xValueToPixel(xValue(trace,0),trace->xAxis());
	int y=yValueToPixel(yValue(trace,0),trace->yAxis());
	if ((button_x>=x&&button_x<=(x+w))&&(button_y>=y&&button_y<=(y+h))) return trace;
      }
   }
  return 0;
}

int MSGraph::indexOfLongestString(MSStringVector& sv_)
{
  int index=0,maxlen=0;
  for (unsigned i=0;i<sv_.length();i++) 
   {
     int len=sv_(i).length();
     if (len>maxlen) 
      {
	maxlen=len;
	index=i;
      }
   }
  return index;
}

int MSGraph::maxStringWidth(XFontStruct*fi_,MSStringVector& aStringVector_)
{
  int maxWidth=0;
  for (unsigned i=0;i<aStringVector_.length();i++) 
   {
     int w=XTextWidth(fi_,aStringVector_(i).string(),aStringVector_(i).length());
     maxWidth=w>maxWidth?w:maxWidth;
   }
  return maxWidth;
}
  
void MSGraph::enterTrace(const XEvent *event_)
{
  if (event_!=0)
   {
     if (addLineSegment(event_)==MSTrue)
      {
	_newtraceCt++;
	drawLineHandles();
	_onLinePoint=MSFalse;
	selectInput(MSGraphEventMask|PointerMotionMask);
      }
     else
      {
	if (graphZoomStatus()==MSTrue) XDefineCursor(display(),window(),zoomCursor());
	else XUndefineCursor(display(),window());
	graphUIModeSet(Normal);
      }
   }
}

void MSGraph::enterTextTrace(void)
{
  int  length;
  
  graphUIModeSet(AddTextTrace);
  int x=interactivePixel()(0,0);
  int y=interactivePixel()(0,1);
  if (x+editor()->width()>x_end())
   {
     if ((length=x_end()-x)<50)
      { 
	length=50;
	x=x_end()-length;
      }
     editor()->width(length);
   }
  editor()->font(font());
  editor()->foreground(foreground());
  int w=x_end()-x-5;
  int h=y_org()-y;
  editor()->resize(w,h>height()?height()-5:h);
  editor()->moveTo(x,y);
  editor()->map();
  editor()->raise();
  focusInNotify(editor());
}

void MSGraph::editTextTrace(void)
{

  if (selectTrace()!=0 && selectTrace()->traceSet()->isProtected()==MSFalse)
   {
     XFontStruct *fi=(XFontStruct *)server()->fontStruct(selectTrace()->traceSet()->textFont());
     int x=xValueToPixel(xValue(selectTrace(),0),selectTrace()->xAxis());
     int y=yValueToPixel(yValue(selectTrace(),0),selectTrace()->yAxis());

     MSStringVector aStringVector;
     unsigned lines=selectTrace()->traceSet()->textLength();
     MSString formatBuffer;
     for (int j=0;j<lines;j++) aStringVector<<selectTrace()->traceSet()->formatText(formatBuffer.removeAll(),j);
     int h=aStringVector.length()*(fi->ascent+fi->descent);
     int w=maxStringWidth(fi,aStringVector);
     int maxw=x_end()-x-5;
     int maxh=y_org()-y;
     w=w>maxw?maxw:w;
     h=w>maxh?maxh:h;
     editor()->moveTo(x,y);
     editor()->stringVector(aStringVector);
     editor()->foreground(selectTrace()->traceSet()->textForeground());
     editor()->font(selectTrace()->traceSet()->textFont());
     editor()->resize(maxw,maxh);
     if (editor()->mapped()!=MSTrue) 
      {
	int xset=interactivePixel()(0,0);
	editor()->moveCursor(0,(xset-x)/fi->max_bounds.width);
      }
     if (inputFocus()==this)
      {
	editor()->map();
	editor()->raise();
	focusInNotify(editor());
      }
   }
}

void MSGraph::editorEscape(void)
{
  unHighlightTrace();
  editor()->unmap();
  editor()->lower();
  editor()->string("");
  focusOutNotify(editor());
}

void MSGraph::editorActivate(void)
{
  if (graphUIMode()==AddTextTrace&&editor()->length()>0)
   {
     if (activateCallback(MSWidgetCallback::addtexttrace)!=MSTrue)
      {
	MSFloatMatrix pFloatMatrix(createInteractiveTraceData((unsigned long)MSLeft));
	createTextTraceSet(pFloatMatrix,*(new MSStringVector(editorString())));
      }
   }     
  else
   {
     if (selectTrace()!=0)
      {
	selectTrace()->traceSet()->validateText(editorString());
	unHighlightTrace();
	selectTrace(0);
      }
   }
  editor()->string("");
  editor()->lower();
  editor()->unmap();
  graphUIModeSet(Normal);
}

void MSGraph::moveTextTrace(const XEvent *event_)
{
  int 		ix,iy;
  int 		rx,ry;
  int 		offset=1;
  unsigned int	keys,mask=0;
  Window 	root,child;
  MSTrace      *trace=selectTrace();
  MSBoolean     copyMode=MSFalse;
  
  if (event_->xbutton.button==1) mask=Button1Mask;
  else if (event_->xbutton.button==2) mask=Button2Mask;
  else mask=Button3Mask;
  keys=mask;
  
  XFontStruct *fi=(XFontStruct *)server()->fontStruct(trace->traceSet()->textFont());
  int x=event_->xbutton.x;
  int y=event_->xbutton.y;
  MSStringVector aStringVector;
  unsigned lines=trace->traceSet()->textLength();
  MSString formatBuffer;
  for (int j=0;j<lines;j++) aStringVector<<trace->traceSet()->formatText(formatBuffer.removeAll(),j);
  int h=(fi->ascent+fi->descent)*aStringVector.length()+1;
  int w=maxStringWidth(fi,aStringVector)+1;
  int xStart=xValueToPixel(xValue(trace,0),trace->xAxis());
  int yStart=yValueToPixel(yValue(trace,0),trace->yAxis());
  int xoffset=event_->xbutton.x-xStart;
  int yoffset=event_->xbutton.y-yStart;
  XDrawRectangle(display(),window(),windowGC(),x-xoffset-1,y-yoffset-1,w,h);
  while (keys&mask)
   {
     if (keys&ShiftMask) copyMode=MSTrue; else copyMode=MSFalse;
     XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
     if (ix!=x||iy!=y)
      {
	if ((ix-xoffset)<(x_org()+offset+2))
	 { 
	   ix=x_org()+offset+xoffset+2;
	 }
	if ((iy-yoffset)<(y_end()+offset+2))
	 { 
	   iy=y_end()+offset+yoffset+2;
	 }
	if ((ix-xoffset)>(x_end()-offset)) ix=x_end()-offset+xoffset;
	if ((iy-yoffset)>(y_org()-h-offset)) iy=y_org()-h-offset+yoffset;
	if (keys&ControlMask)
	 {
	   if (abs(ix-event_->xbutton.x)>abs(iy-event_->xbutton.y)) 
	    {
	      iy=event_->xbutton.y;
	    }
	   else ix=event_->xbutton.x;
	 }
	if (ix!=x||iy!=y)
	 {
	   XDrawRectangle(display(),window(),windowGC(),x-xoffset-1,y-yoffset-1,w,h);
	   XDrawRectangle(display(),window(),windowGC(),ix-xoffset-1,iy-yoffset-1,w,h);
	 }
      }
     y=iy;
     x=ix;
   }
  XDrawRectangle(display(),window(),windowGC(),x-xoffset-1,y-yoffset-1,w,h);
  if ((ix-xoffset)!=xStart||(iy-yoffset)!=yStart)
   {
     if (copyMode==MSTrue)
      {
	MSStringVector aStringVector;
	unsigned lines=trace->traceSet()->textLength();
	MSString formatBuffer;
	for (int j=0;j<lines;j++) aStringVector<<trace->traceSet()->formatText(formatBuffer.removeAll(),j);
	editor()->stringVector(aStringVector);
	MSTraceSet *ts=trace->traceSet();
	interactivePixel()(0,0)=ix-xoffset;
	interactivePixel()(0,1)=iy-yoffset;
	if (activateCallback(MSWidgetCallback::copytexttrace)!=MSTrue)
	 {
	   interactiveData().reshape(1,2);
	   interactiveData()(0,0)=normalizedPixelToValue(ix-xoffset,trace->xAxis());
	   interactiveData()(0,1)=yPixelToValue(iy-yoffset,trace->yAxis());
	   createTextTraceSet(interactiveData(),aStringVector);
	 }
	editor()->string("");
      }
     else
      {
	double x_val=normalizedPixelToValue(ix-xoffset,trace->xAxis());
	double y_val=yPixelToValue(iy-yoffset,trace->yAxis());
        if(orientation()==Horizontal)
         {
           trace->traceSet()->validate(0,trace->virtualCol(),y_val,x_val);
         }
        else
         {
           trace->traceSet()->validate(0,trace->virtualCol(),x_val,y_val);
         }
	redrawImmediately();
      }
   }
}

void MSGraph::deleteTrace(void)
{
  if (selectTrace()!=0)
   {
     int count=traceList().count();
     if (activateCallback(MSWidgetCallback::deletetrace)!=MSTrue)
      {
	MSTraceSet *ts=selectTrace()->traceSet();
	if (ts!=0) safeDestroy(ts);
      }
     if (traceList().count()<count) selectTrace(0);
     graphUIModeSet(Normal);
   }
}

void MSGraph::moveTrace(const XEvent *event_)
{
  int 		ix,iy;
  int 		rx,ry;
  int		offset=1;
  unsigned int	keys,mask=0;
  Window 	root,child;
  MSTrace      *trace=selectTrace();
  MSBoolean     copyMode=MSFalse;
 
  int bx=event_->xbutton.x;
  int by=event_->xbutton.y;
  int x_init=trace->xOffset();
  int y_init=trace->yOffset();
  if (event_->xbutton.button==1) mask=Button1Mask;
  else if (event_->xbutton.button==2) mask=Button2Mask;
  else mask=Button3Mask;
  keys=mask;
  selectInput(MSGraphEventMask);

  int x=bx+x_init;
  int y=by+y_init; 
  trace->xOffset(0); trace->yOffset(0);	  
  drawMoveTrace(trace);
  while (keys&mask)
   {
     if (keys&ShiftMask) copyMode=MSTrue; else copyMode=MSFalse;
     XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
     ix=ix<(x_org()+offset)?x_org()+offset:ix;
     ix=ix>(x_end()-offset)?x_end()-offset:ix;
     ix+=x_init; iy+=y_init;
     if (ix!=x||iy!=y)
      {
	trace->xOffset(x-bx); trace->yOffset(y-by);	  
	drawMoveTrace(trace);
 	trace->xOffset(ix-bx); trace->yOffset(iy-by);	  
 	drawMoveTrace(trace);
      }
     x=ix; y=iy; 
   }
  trace->xOffset(0);
  trace->yOffset(0);
  trace->traceSet()->xOffset((x-bx)/xScale(trace->xAxis()));
  trace->traceSet()->yOffset((y-by)/yScale(trace->yAxis()));
  if (copyMode==MSTrue)
   {
     freeze();
     copyTrace(trace);
     unHighlightTrace();
     unfreeze();
   }
  else if ((abs(x-bx)>0||abs(y-by)>0))
   {
     freeze();
     if (moveTraceValidate(trace)!=MSTrue) 
      {
	MSTraceSet *ts=trace->traceSet();
	ts->yOffset(0);
	ts->xOffset(0);
      }
     unfreeze();
     drawLineHandles(trace);
   }
  else
   {
     drawMoveTrace(trace);
//     drawLineHandles(trace);
   }
  selectInput(MSGraphEventMask|PointerMotionMask);
}

const MSString& MSGraph::interactiveTag(void) const
{
  static unsigned long count=0;
  unsigned long len=_interactiveTraceTags.length();
  if (len>0)
   {
     if (len<=count) count=0;
     return _interactiveTraceTags.elementAt(count++);
   }
  else
   {
     return _defaultTraceTag;
   }
}

void MSGraph::copyTrace(MSTrace* trace_)
{
  MSTraceSet *ts=trace_->traceSet();

  if (hasCallback(MSWidgetCallback::copytrace)==MSTrue)
   {
     interactiveData()=ts->asFloatMatrix();
     if(interactiveData().columns()==1)  // vector
      {
        interactiveData()-=ts->yOffset();
      }
     else  // real matrix
      {
        MSFloatVector fv(interactiveData().columnAt(0));
        interactiveData()-=ts->yOffset();
        fv+=ts->xOffset();
        interactiveData().assignColumn(0,fv);
      }
     activateCallback(MSWidgetCallback::copytrace);
   }
  else
   {
     MSString tag=interactiveTag();
     MSTraceSet *newTs = ts->copyTraceValidate(this,tag,MSSymbol(tag));
     if (newTs!=0)
      {
        newTs->moveTraceValidate(ts->xOffset(),ts->yOffset());
        newTs->selectable(MSTrue);
        newTs->constraint(MSNone);
      }
   }
}

MSBoolean MSGraph::moveTraceValidate(MSTrace* trace_)
{
  MSTraceSet *ts=trace_->traceSet();
  return ts->moveTraceValidate(ts->xOffset(),ts->yOffset());
}

void MSGraph::highlightPoint(MSTraceSet *tp_)
{
  int 	      index=tp_->selectRow();
  MSTrace    *trace=tp_->trace(tp_->selectCol());
  
  int x=xValueToPixel(xValue(trace,index),trace->xAxis());
  int y=yValueToPixel(trace->y(index),trace->yAxis());
  if (trace->symbol()==MSG::Text)
   {
     XSetFont(display(),legend()->gc(),trace->font());
   }
  drawLegendScatterSymbols(window(),windowGC(),trace,x,y);    
}

void MSGraph::highlightTrace(void)
{
  MSTrace *trace=selectTrace();

  if (trace!=0&&trace->traceSet()->textLength()>0&&trace->dataCount()==1)
   {
     XSetFont(display(),traceGC(),trace->traceSet()->textFont());
     XFontStruct *fontInfo=(XFontStruct*)server()->fontStruct(trace->traceSet()->textFont());
     int x=xValueToPixel(xValue(trace,0),trace->xAxis());
     int y=yValueToPixel(yValue(trace,0),trace->yAxis());
     MSStringVector aStringVector;
     unsigned lines=trace->traceSet()->textLength();
     MSString formatBuffer;
     int j;
     for (j=0;j<lines;j++) aStringVector<<trace->traceSet()->formatText(formatBuffer.removeAll(),j);
     int h=aStringVector.length()*(fontInfo->ascent+fontInfo->descent);
     int w=maxStringWidth(fontInfo,aStringVector);
     XSetForeground(display(),traceGC(),trace->traceSet()->textForeground());
     XFillRectangle(display(),window(),traceGC(),x,y,w,h);
     XSetForeground(display(),traceGC(),MSWidget::background());
     y+=fontInfo->ascent;
     for (j=0;j<aStringVector.length();j++)
      {
	const char *str=aStringVector(j).string();
	XDrawString(display(),window(),traceGC(),fontInfo,x,y,str,aStringVector(j).length());
	y+=fontInfo->ascent+fontInfo->descent;
      }
   }
  else if (trace!=0&&trace->dataCount()>0)
   { 
     graphUIModeSet(MoveTrace);
     XDefineCursor(display(),window(),drawCursor());
   }
}

void MSGraph::unHighlightTrace(void)
{
  selectTrace(0);
  graphUIModeSet(Normal);
  XSetForeground(display(),windowGC(),MSWidgetCommon::foreground()^background());
  redrawImmediately();
}

void MSGraph::scanXY(const XEvent *event_)
{
  int 			i;
  int			offset=1;
  unsigned int 		keys,mask;
  int 			lastAxis,axis=0;
  int 			ix,iy;
  int 			rx,ry;
  Window 		root,child;
  MSRect                r;
  MSBoolean 		update,bs,legendMapped=MSFalse;
  unsigned long         style=legendStyle();
  
  if ((bs=backingStore())==MSFalse)
   {
     redrawImmediately();
     r.x(dataWin()->x_origin()); r.y(dataWin()->y_origin());
   }
  int *lastIndex=new int[traceList().count()];
  for (i=0; i<traceList().count(); i++) lastIndex[i]=-1;
  _legendStyle=Vertical;
  int x=event_->xbutton.x,y=event_->xbutton.y;
  if (event_->xbutton.button==1) mask=Button1Mask;
  else if (event_->xbutton.button==2) mask=Button2Mask;
  else mask=Button3Mask;
  keys=mask;
  if (cursorType()==Xcursor&&traceList().count()>0&&!(graphMode()&PieChart)&&pieCount()==0)
   {
     if (legendAlignment()==MSNone)
      {
        legendMapped=MSTrue;
        legendAlignment(MSTop);
      }
     legend()->valueWidth(XTextWidth(legend()->fontInfo(),"0",1)*yStringWidth()+legend()->spacing()/2);
     legend()->redraw();
     dataWin()->setDataWin(xStringWidth(),0);  
     drawScanXYvalueBox(x,y);
     drawScanXvalues(x,y,axis,lastIndex);
   }
  else if (cursorType()==XYcursor)
   {
     dataWin()->setDataWin(xStringWidth(),yStringWidth());
     drawScanXYvalueBox(x,y);
     drawScanXYvalues(xPixelToValue(x,axis),yPixelToValue(y,axis),axis,axis);
   }
  else
   {
    delete [] lastIndex;
    return;
   }
  if (bs==MSFalse)
   {
     r.width(dataWin()->width()); r.height(dataWin()->height());
     XCopyArea(display(),graphPixmap(),window(),clearGC(),r.x(),r.y(),r.width(),r.height(),r.x(),r.y());
     r.x(dataWin()->x_origin()); r.y(dataWin()->y_origin());
   }
  if (cursorType()!=Xcursor)
   {
     y=y>(y_org()-offset)?y_org()-offset:y;
     y=y<(y_end()+offset)?y_end()+offset:y;
     XDrawLine(display(),window(),windowGC(),x_org(),y,x_end(),y);
   }
  if (cursorType()!=Ycursor)
   {
     x=x<(x_org()+offset)?x_org()+offset:x;
     x=x>(x_end()+offset)?x_end()+offset:x;
     XDrawLine(display(),window(),windowGC(),x,y_org(),x,y_end());
   }
  while (keys&mask)
   {
     XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
     lastAxis=axis;
     axis=keys&Button3Mask?1:0;
     update=axis!=lastAxis?MSTrue:MSFalse;
     if(bs==MSFalse) server()->flush();
     drawScanXYvalueBox(ix,iy);
#if defined(MS_WINDOWS)
     if (iy!=y||ix!=x)
      {
	int ht=highlightThickness()+shadowThickness();
	XCopyArea(display(),graphPixmap(),window(),clearGC(),ht,ht,width()-2*ht,height()-2*ht,ht,ht);
	if (cursorType()!=Xcursor)
	 {
	   y=y>(y_org()-offset)?y_org()-offset:y;
	   y=y<(y_end()+offset)?y_end()+offset:y;
	   XDrawLine(display(),window(),windowGC(),x_org(),y,x_end(),y);
	 }
	if (cursorType()!=Ycursor)
	 {
	   x=x<(x_org()+offset)?x_org()+offset:x;
	   x=x>(x_end()+offset)?x_end()+offset:x;
	   XDrawLine(display(),window(),windowGC(),x,y_org(),x,y_end());
	 }
	update=MSTrue;
      }
#else
     // The following no backingStore mode needs to fix the
     // case where the dataWin overlaps the x or y cursor
     if (bs==MSFalse&&(iy!=y||ix!=x))
      {
	XCopyArea(display(),graphPixmap(),window(),clearGC(),r.x(),r.y(),r.width(),r.height(),r.x(),r.y());
	r.x(dataWin()->x_origin()); r.y(dataWin()->y_origin());
      }
     if (iy!=y&&cursorType()!=Xcursor)
      { 
	iy=iy>(y_org()-offset)?y_org()-offset:iy;
	iy=iy<(y_end()+offset)?y_end()+offset:iy;
	if (iy!=y)
	 {
	   XDrawLine(display(),window(),windowGC(),x_org(),y,x_end(),y);
	   XDrawLine(display(),window(),windowGC(),x_org(),iy,x_end(),iy);
	   update=MSTrue;
	 }
      }
     if (ix!=x&&cursorType()!=Ycursor)
      {
	ix=ix<(x_org()+offset)?x_org()+offset:ix;
	ix=ix>(x_end()+offset)?x_end()+offset:ix;
	if (ix!=x)
	 {
	   XDrawLine(display(),window(),windowGC(),x,y_org(),x,y_end());
	   XDrawLine(display(),window(),windowGC(),ix,y_org(),ix,y_end());
	   update=MSTrue;
	 }
      }
#endif
     y=iy; x=ix;
     if (update==MSTrue)
      {
	if (cursorType()==Xcursor) drawScanXvalues(ix,iy,axis,lastIndex);
	else if (cursorType()==XYcursor) drawScanXYvalues(xPixelToValue(ix,axis),yPixelToValue(iy,axis),axis,axis);
      }	
   }
  if (cursorType()!=Xcursor) XDrawLine(display(),window(),windowGC(),x_org(),y,x_end(),y);
  if (cursorType()!=Ycursor) XDrawLine(display(),window(),windowGC(),x,y_org(),x,y_end());
  dataWin()->lower();
  dataWin()->unmap();
  dataWin()->MSWidget::clear();
  _legendStyle=style;
  if (cursorType()==Xcursor)
   {
     if (legendStyle()!=LastValue) legend()->valueWidth(0);
     legend()->redraw();
   }
  if (legendMapped==MSTrue) legendAlignment(MSNone);
  XEvent ev; 
  while(XCheckWindowEvent(display(),window(),ButtonPressMask,&ev));
  delete [] lastIndex;
}

void MSGraphStatusWin::setDataWin(int x_width_,int y_width_)
{
  int charWidth=XTextWidth(fontInfo(),"0",1); 
  highlightColor(graph()->foreground());
  background(graph()->background());
  leading(0);
  if (graph()->xLabelOut(0).format()==MSTime::MonthDayYear) 
   {
     valueWidth((x_width_+y_width_)*charWidth+spacing());
     width(valueWidth()+2*(insideMargin()+offset()));
     x1((valueWidth()-charWidth*(8+y_width_))/2);
     x2(x1()+charWidth*8+spacing());
     height(2*(textHeight()+insideMargin()));
   }
  else
   {
     valueWidth((x_width_+y_width_)*charWidth+spacing());
     width(valueWidth()+2*(insideMargin()+offset()));
     x1((valueWidth()-charWidth*(x_width_+y_width_)+spacing())/2);
     x2(x1()+charWidth*x_width_+spacing());
     height(textHeight()+2*insideMargin());
   }
  raise();
  map();
  drawHighlight();
}

void MSGraph::drawScanXvalues(int x_,int,int axis_,int *lastIndex)
{
  int 	    	i,j,xs,ys,index,minIndex,lastRow;
  double 	xx,delta;
  MSString      formatBuffer;
  MSTrace      *trace,*minTrace=0;
  MSBoolean	setX=MSFalse,updateX=MSFalse;
  
  if (legend()->mapped()!=MSTrue) return;
  int x=legend()->xOffset(); 
  int y=legend()->insideMargin()+2;
  for (i=0; i<traceList().count(); i++)
   {
     if ((trace=graphTrace(i))!=0)
      {
        xs=trace->xAxis();
        ys=trace->yAxis();
        if(trace->style()!=MSG::Text&&trace->dataCount()>1&&trace->legend()>0)
         {
           if (setX==MSFalse) 
            {
              setX=MSTrue;
              delta=trace->traceSet()->xDelta();
            }
           xx=xPixelToValue(x_,xs);
           if (lastIndex==0||(index=findYvalue(trace,xx))>=0)
            {
              if (lastIndex!=0)
               {
                 if (trace->traceSet()->xDelta()<=delta)
                  {
                    delta=trace->traceSet()->xDelta();
                    minTrace=trace;
                    minIndex=index;
                  } 
                 updateX=MSTrue;
               } 
              else index=trace->dataCount()-1;
              if (trace->style()>=Candle)
               {
                 lastRow=trace->style()==HL?2:trace->style()==HLC?3:4;
                 int offset=(trace->style()==HL||trace->style()==HLC)?trace->offset():0;
                 for (j=offset; j<lastRow+offset; j++)
                  {
                    y+=legend()->textHeight()+legend()->leading();
                    if (lastIndex==0||index!=lastIndex[i]) 
                     {
                       formatAxisLabel(formatBuffer.removeAll(),yLabelOut(ys),trace->y(index,j),yShowPos());
                       XFillRectangle(display(),legend()->window(),
                                      legend()->backgroundShadowGC(),
                                      x,y,
                                      legend()->valueWidth()-legend()->offset(),
                                      legend()->textHeight()
                         );
                       XDrawString(display(),legend()->window(),
                                   legend()->gc(),legend()->fontInfo(),
                                   x,y+legend()->fontInfo()->ascent,
                                   formatBuffer,formatBuffer.length());
                     }
                  }
	    }
              else if (trace->style()!=0&&(lastIndex==0||index!=lastIndex[i]))
               {
                 formatAxisLabel(formatBuffer.removeAll(),yLabelOut(ys),trace->y(index),yShowPos());
                 XFillRectangle(display(),legend()->window(),
                                legend()->backgroundShadowGC(),
                                x,y,
                                legend()->valueWidth()-legend()->offset(),
                                legend()->textHeight()
                   );
                 XDrawString(display(),legend()->window(),
                             legend()->gc(),legend()->fontInfo(),
                             x,y+legend()->fontInfo()->ascent,
                             formatBuffer,formatBuffer.length());
               }
            }
           else
            {
              if (index==-1)
               {
                 if (trace->style()>=Candle)
                  {
                    for (int j=0; j<4; j++)
                     {
                       if (trace->style()==HLC&&j==2) continue;
                       y+=legend()->textHeight()+legend()->leading();
                       XFillRectangle(display(),legend()->window(),
                                      legend()->backgroundShadowGC(),
                                      x,y,
                                      legend()->valueWidth()-legend()->offset(),
                                      legend()->textHeight()
                         );
                     }
                  }
                 else
                  {
                    XFillRectangle(display(),legend()->window(),
                                   legend()->backgroundShadowGC(),
                                   x,y,
                                   legend()->valueWidth()-legend()->offset(),
                                   legend()->textHeight() 
                      );
                  }
               }
            }
           if (trace->style()!=0) y+=legend()->textHeight()+legend()->leading();
           if (lastIndex!=0) lastIndex[i]=index;
         }
      }
   }
  xx=normalizedPixelToValue(x_,axis_);
  xx=minTrace!=0&&axis_==xs?minTrace->x(minIndex):xx;
  if (lastIndex!=0&&xLabelOut(xs).format()==MSTime::MonthDayYear&&updateX==MSTrue)
   {
     formatAxisLabel(formatBuffer.removeAll(),xLabelOut(xs),xx);
     y=dataWin()->insideMargin()+dataWin()->fontInfo()->ascent;
     x=(dataWin()->width()-XTextWidth(dataWin()->fontInfo(),formatBuffer,formatBuffer.length()))/2;
     XFillRectangle(display(),dataWin()->window(),
		    dataWin()->backgroundShadowGC(),
		    dataWin()->offset(),dataWin()->offset(),
		    dataWin()->width()-2*dataWin()->offset(),
		    dataWin()->height()-2*dataWin()->offset()
		 );
     XDrawString(display(),dataWin()->window(),dataWin()->gc(),dataWin()->fontInfo(),
		 x,y,formatBuffer,formatBuffer.length());
     formatTimeLabel(formatBuffer.removeAll(),xx);
     x=(dataWin()->width()-XTextWidth(dataWin()->fontInfo(),formatBuffer,formatBuffer.length()))/2;
     y+=dataWin()->textHeight()+dataWin()->leading();     
     XDrawString(display(),dataWin()->window(),dataWin()->gc(),dataWin()->fontInfo(),x,y,
		 formatBuffer,(formatBuffer.length()<_xStringWidth)?formatBuffer.length():_xStringWidth);
   }
  else if (updateX==MSTrue||index==-1)
   {
     formatAxisLabel(formatBuffer.removeAll(),xLabelOut(axis_),xx,xShowPos());
     y=dataWin()->insideMargin()+dataWin()->fontInfo()->ascent;
     XFillRectangle(display(),dataWin()->window(),
		    dataWin()->backgroundShadowGC(),
		    dataWin()->offset(),dataWin()->offset(),
		    dataWin()->width()-2*dataWin()->offset(),
		    dataWin()->height()-2*dataWin()->offset()
		 );
     XDrawString(display(),dataWin()->window(),
		 dataWin()->gc(),dataWin()->fontInfo(),dataWin()->x1(),y,
		 formatBuffer,(formatBuffer.length()<_xStringWidth)?formatBuffer.length():_xStringWidth);
   }
}  

void MSGraph::drawScanXYvalues(double x_,double y_,int xs_,int ys_)
{
  int         x,y; 
  MSString   formatBuffer;

  if (xLabelOut(xs_).format()==MSTime::MonthDayYear)
   {
     XFillRectangle(display(),dataWin()->window(),
		    dataWin()->backgroundShadowGC(),
		    dataWin()->offset(),
		    dataWin()->offset(),
		    dataWin()->width()-2*dataWin()->offset(),
		    dataWin()->height()-2*dataWin()->offset()
		 );
     formatAxisLabel(formatBuffer.removeAll(),xLabelOut(xs_),x_);
     y=dataWin()->insideMargin()+dataWin()->fontInfo()->ascent;
     x=(dataWin()->width()-XTextWidth(dataWin()->fontInfo(),formatBuffer,formatBuffer.length()))/2;
     XDrawString(display(),dataWin()->window(),
		 dataWin()->gc(),dataWin()->fontInfo(),x,y,formatBuffer,formatBuffer.length());
     formatTimeLabel(formatBuffer.removeAll(),x_);
     y+=dataWin()->textHeight()+dataWin()->leading();
     XDrawString(display(),dataWin()->window(),
		 dataWin()->gc(),dataWin()->fontInfo(),dataWin()->x1(),y,
		 formatBuffer,(formatBuffer.length()<_xStringWidth)?formatBuffer.length():_xStringWidth);
     formatAxisLabel(formatBuffer.removeAll(),yLabelOut(ys_),y_,yShowPos());
     XDrawString(display(),dataWin()->window(),
		 dataWin()->gc(),dataWin()->fontInfo(),dataWin()->x2(),y,
		 formatBuffer,(formatBuffer.length()<_yStringWidth)?formatBuffer.length():_yStringWidth);
   }
  else
   {
     XFillRectangle(display(),dataWin()->window(),
		    dataWin()->backgroundShadowGC(),
		    dataWin()->offset(),
		    dataWin()->offset(),
		    dataWin()->width()-2*dataWin()->offset(),
		    dataWin()->height()-2*dataWin()->offset()
		 );
     formatAxisLabel(formatBuffer.removeAll(),xLabelOut(xs_),x_,yShowPos());
     y=dataWin()->insideMargin()+dataWin()->fontInfo()->ascent;
     XDrawString(display(),dataWin()->window(),
		 dataWin()->gc(),dataWin()->fontInfo(),dataWin()->x1(),y,
		 formatBuffer,(formatBuffer.length()<_xStringWidth)?formatBuffer.length():_xStringWidth);
     formatAxisLabel(formatBuffer.removeAll(),yLabelOut(ys_),y_,yShowPos());
     XDrawString(display(),dataWin()->window(),
		 dataWin()->gc(),dataWin()->fontInfo(),dataWin()->x2(),y,
		 formatBuffer,(formatBuffer.length()<_yStringWidth)?formatBuffer.length():_yStringWidth);
   }
}  

int MSGraph::findYvalue(MSTrace *trace_,double x_)
{
  int i;

  if (trace_->traceSet()->overlap()==MSTrue) return -1;
  if (xValue(trace_,0)<xValue(trace_,trace_->dataCount()-1))
   {
     // ascending X values
     if (x_<xValue(trace_,0)-trace_->xShift()) return(-1);
     if (x_>xValue(trace_,trace_->dataCount()-1))
      {
	if (x_<=xValue(trace_,trace_->dataCount()-1)+trace_->traceSet()->xDelta())
	 {
	   return(trace_->dataCount()-1);
	 }
	else return(-1);
      }
     if ((x_-xValue(trace_,0))<(xValue(trace_,trace_->dataCount()-1)-x_))
      {
	for (i=0; x_>xValue(trace_,i)-trace_->xShift(); i++);
	return i==0?0:i-1;
      }
     else
      {
	for (i=trace_->dataCount()-1; x_<xValue(trace_,i)-trace_->xShift(); i--);
	return i;
      }
   }
  else
   {
     // descending X values
     if (x_>xValue(trace_,0)-trace_->xShift()||x_<xValue(trace_,trace_->dataCount()-1))
      {
	if (x_>xValue(trace_,0)+trace_->traceSet()->xDelta()) return (0);
	else return(-1);
      }
     if ((x_-xValue(trace_,trace_->dataCount()-1)<(trace_->x(0)-x_)))
      {
	for (i=trace_->dataCount()-1; x_>xValue(trace_,i)-trace_->xShift(); i--);
	return i+1;
      }
     else
      {
	for (i=0; x_<xValue(trace_,i)-trace_->xShift(); i++);
	return i;
      }
   }
}

MSBoolean MSGraph::drawZoomRegion(const XEvent *event_)
{
  unsigned int 	keys,mask;
  MSBoolean	moved=MSFalse;
  int 		x,y;
  int 		ix,iy;
  int 		rx,ry;
  Window 	root,child;

  if (graphMode()&PieChart||traceList().count()==pieCount()) return MSFalse;
  if (event_->xbutton.button==1) mask=Button1Mask;
  else if (event_->xbutton.button==2) mask=Button2Mask;
  else mask=Button3Mask;
  keys=mask;
  int bx=x=event_->xbutton.x;
  int by=y=event_->xbutton.y;
  
  XDrawRectangle(display(),window(),windowGC(),bx,by,0,0);
  while (keys&mask)
   {
     XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
     if (ix!=x||iy!=y)
      {
	if (x>bx)
         {
	   if (y>by) XDrawRectangle(display(),window(),windowGC(),bx,by,x-bx,y-by);
	   else XDrawRectangle(display(),window(),windowGC(),bx,y,x-bx,by-y);
         }
	else
         {
	   if (y>by) XDrawRectangle(display(),window(),windowGC(),x,by,bx-x,y-by);
	   else XDrawRectangle(display(),window(),windowGC(),x,y,bx-x,by-y);
         }
	x=ix; y=iy;
	if (x>bx)
         {
	   if (y>by) XDrawRectangle(display(),window(),windowGC(),bx,by,x-bx,y-by);
	   else XDrawRectangle(display(),window(),windowGC(),bx,y,x-bx,by-y);
         }
	else
         {
	   if (y>by) XDrawRectangle(display(),window(),windowGC(),x,by,bx-x,y-by);
	   else XDrawRectangle(display(),window(),windowGC(),x,y,bx-x,by-y);
         }
      }
   }  
  if (x>bx)
   {
     if (y>by)	XDrawRectangle(display(),window(),windowGC(),bx,by,x-bx,y-by);
     else XDrawRectangle(display(),window(),windowGC(),bx,y,x-bx,by-y);
   }
  else
   {
     if (y>by)	XDrawRectangle(display(),window(),windowGC(),x,by,bx-x,y-by);
     else XDrawRectangle(display(),window(),windowGC(),x,y,bx-x,by-y);
   }
  if (abs(bx-x)>5&&abs(by-y)>5&&ix<width()&&iy<height())
   {
     moved=MSTrue;
     xMinZoom(bx>x?x:bx); xMaxZoom(bx>x?bx:x);
     yMinZoom(by<y?y:by); yMaxZoom(by<y?by:y);
     if (activateCallback(this,MSWidgetCallback::graphzoom)!=MSTrue) zoom();
   }
  return moved;
}

void MSGraph::zoom(void)
{
  if (xMaxZoom()>xMinZoom()&&yMaxZoom()<yMinZoom())
   {
     for (int i=0; i<2; i++)
      {
	_xMin[i]=xPixelToValue(xMinZoom(),i);
	_xMax[i]=xPixelToValue(xMaxZoom(),i);
	_yMin[i]=yPixelToValue(yMode(i)==Ascending?yMinZoom():yMaxZoom(),i);
	_yMax[i]=yPixelToValue(yMode(i)==Ascending?yMaxZoom():yMinZoom(),i);
	if (graphMode()&Normalize)
	 {
	   _xMinReal[i]=normalizedPixelToValue(xMinZoom(),i);
	   _xMaxReal[i]=normalizedPixelToValue(xMaxZoom(),i);
	 }
      }
     graphZoomStatus(MSTrue);
     redrawImmediately();
     positionLegend(legendAlignment());
   }
}

void MSGraph::unzoom(void)
{  
  if (graphZoomStatus()==MSFalse) return;
  graphZoomStatus(MSFalse);
  for (int i=0; i<2; i++)
   {
     _xMin[i]=_xMinSet[i];
     _xMax[i]=_xMaxSet[i];
     _yMin[i]=_yMinSet[i];
     _yMax[i]=_yMaxSet[i];
   }
  redrawImmediately();
  positionLegend(legendAlignment());
}

MSBoolean MSGraph::addLineSegment(const XEvent *event_)
{
  unsigned int 	keys,mask;
  int 		ix,iy;
  int 		rx,ry;
  int 		x,y,bx,by;
  int		offset=1;
  MSBoolean 	ret=MSTrue;
  Window 	root,child;
  
  if (nt(selectLine())->pointCount()==0||_onLinePoint==MSTrue)
   {
     if (event_->xbutton.button==1) mask=Button1Mask;
     else if (event_->xbutton.button==2) mask=Button2Mask;
     else mask=Button3Mask;
     keys=mask;
     if (nt(selectLine())->pointCount()==0)
      {
	bx=event_->xbutton.x;
	by=event_->xbutton.y;
      }
     else 
      {
	bx=nt(selectLine())->points(selectPoint())->x;
	by=nt(selectLine())->points(selectPoint())->y;
      }
     XDrawLine(display(),window(),windowGC(),bx,by,bx,by);
     y=by; x=bx;
     while (keys&mask)
      {
	XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
	ix=ix<(x_org()+offset)?x_org()+offset:ix;
	ix=ix>(x_end()-offset)?x_end()-offset:ix;
	if (ix!=x||iy!=y)
	 { 
	   XDrawLine(display(),window(),windowGC(),bx,by,x,y);
	   XDrawLine(display(),window(),windowGC(),bx,by,ix,iy);
	 }
	y=iy; x=ix;
      }

    if (abs(y-by)<5&&abs(x-bx)<5)
      {
	XDrawLine(display(),window(),windowGC(),bx,by,ix,iy);
	ret=MSFalse;
      }
     else if (nt(selectLine())->pointCount()==0)
      {
	nt(selectLine())->enterPoint(bx,by);
	nt(selectLine())->enterPoint(x,y);
      }
     else nt(selectLine())->enterPoint(x,y);
   }
  return ret;
}

void MSGraphNewtrace::enterPoint(int x_,int y_)
{
  if (pointCount()>1&&graph()->selectPoint()==0)
   {
     for (int i=pointCount(); i>0; i--)
      {
	points(i)->x=points(i-1)->x;
	points(i)->y=points(i-1)->y;
      }
     points(0)->x=x_;
     points(0)->y=y_;
     _pointCount++;
   }
  else
   {
     points(pointCount())->x=x_;
     points(_pointCount++)->y=y_;
   }
}

void MSGraph::drawLineSegments(void)
{
  if (graphUIMode()==AddTrace)
   {
     for (int j=0; j<newtraceCt(); j++)
      {
	for (int i=0; i<nt(j)->pointCount()-1; i++)
	 {
	   XDrawLine(display(),window(),windowGC(),nt(j)->points(i)->x,
		     nt(j)->points(i)->y,nt(j)->points(i+1)->x,
		     nt(j)->points(i+1)->y);
	 }
      }
   }
}

void MSGraphNewtrace::drawLineSegments(int xOffset,int yOffset)
{
  for (int i=0; i<pointCount()-1; i++)
   {
     XDrawLine(graph()->display(),graph()->window(),graph()->windowGC(),
	       points(i)->x+xOffset,points(i)->y+yOffset,
	       points(i+1)->x+xOffset,points(i+1)->y+yOffset);
   }
}

void MSGraph::drawLineHandles(int point_)
{
  int count=0;
  int size=6;

  if (graphUIMode()==AddTrace)
   {
     int start=point_==-1?0:point_;
     int end=point_==-1?nt(focusLine())->pointCount():point_+1;
  
     XRectangle *rects=new XRectangle[nt(focusLine())->pointCount()];
     for (int i=start; i<end; i++)
      {
	rects[count].x=nt(focusLine())->points(i)->x-size/2;
	rects[count].y=nt(focusLine())->points(i)->y-size/2;
	rects[count].width=size;
	rects[count++].height=size;
      }
     XFillRectangles(display(),window(),windowGC(),rects,count);
     delete [] rects;
   }
}

void MSGraph::drawMoveLineHandleSymbols(int x_,int y_)
{
  int ox,oy;
  if (selectPoint()!=0)
   {
     ox=nt(selectLine())->points(selectPoint()-1)->x;
     oy=nt(selectLine())->points(selectPoint()-1)->y;
     XDrawLine(display(),window(),windowGC(),ox,oy,x_,y_);
   }
  if (selectPoint()!=nt(selectLine())->pointCount()-1)
   {
     ox=nt(selectLine())->points(selectPoint()+1)->x;
     oy=nt(selectLine())->points(selectPoint()+1)->y;
     XDrawLine(display(),window(),windowGC(),ox,oy,x_,y_);
   }
}

void MSGraph::moveLineHandle(const XEvent *event_)
{
  unsigned int 	keys,mask;
  int 		ix,iy;
  int 		rx,ry;
  int 		x,y;
  int		offset=1;
  Window 	root,child;
  
  if (_onLinePoint==MSTrue)
   {
     if (event_->xbutton.button==1) mask=Button1Mask;
     else if (event_->xbutton.button==2) mask=Button2Mask;
     else mask=Button3Mask;
     keys=mask;
     int bx=event_->xbutton.x;
     int by=event_->xbutton.y;
     if (selectPoint()<0) return;
     bx=nt(selectLine())->points(selectPoint())->x;
     by=nt(selectLine())->points(selectPoint())->y;
     drawMoveLineHandleSymbols(bx,by);
     y=by; x=bx;
     while (keys&mask)
      {
	XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
	ix=ix<(x_org()+offset)?x_org()+offset:ix;
	ix=ix>(x_end()-offset)?x_end()-offset:ix;
	if (keys&ControlMask)
	 {
	   if (abs(ix-bx)>abs(iy-by)) iy=by;
	   else ix=bx;
	 }
	if (ix!=x||iy!=y)
	 { 
	   drawMoveLineHandleSymbols(x,y);
	   drawMoveLineHandleSymbols(ix,iy);
	 }
	y=iy; x=ix;
      }
     drawLineHandles();
     nt(selectLine())->points(selectPoint())->x=x;
     nt(selectLine())->points(selectPoint())->y=y;
     drawLineHandles();
     drawMoveLineHandleSymbols(bx,by);
   }
}

MSBoolean MSGraph::moveLineSegment(const XEvent *event_,MSBoolean copy_)
{
  unsigned int 	keys,mask;
  int 		ix,iy;
  int 		rx,ry;
  int 		x,y;
  int		i,offset=1;
  Window 	root,child;

  if (_onLineSegment==MSTrue)
   {
     if (event_->xbutton.button==1) mask=Button1Mask;
     else if (event_->xbutton.button==2) mask=Button2Mask;
     else mask=Button3Mask;
     keys=mask;
     int bx=event_->xbutton.x;
     int by=event_->xbutton.y;
     
     nt(selectLine())->drawLineSegments();
     y=by; x=bx;
     while (keys&mask)
      {
	XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
	ix=ix<(x_org()+offset)?x_org()+offset:ix;
	ix=ix>(x_end()-offset)?x_end()-offset:ix;
	if (ix!=x||iy!=y)
	 { 
	   nt(selectLine())->drawLineSegments(x-bx,y-by);
	   nt(selectLine())->drawLineSegments(ix-bx,iy-by);
	 }
	y=iy; x=ix;
      }
     if (abs(y-by)<5&&abs(x-bx)<5) return MSFalse;
     drawLineHandles();
     if (copy_==MSTrue)
      {
	_focusLine++;
	_selectLine++;
	_newtraceCt++;
	for (i=0; i<nt(selectLine()-1)->pointCount(); i++)
	 {
	   nt(selectLine())->points(i)->x=nt(selectLine()-1)->points(i)->x+x-bx;
	   nt(selectLine())->points(i)->y=nt(selectLine()-1)->points(i)->y+y-by;
	   nt(selectLine())->pointCount(i+1);
	 }
      }
     else 
      {
	nt(selectLine())->drawLineSegments();
	for (i=0; i<nt(selectLine())->pointCount(); i++)
	 {
	   nt(selectLine())->points(i)->x+=x-bx;
	   nt(selectLine())->points(i)->y+=y-by;
	 }
	drawLineHandles();
      }
   }
  return MSTrue;
}


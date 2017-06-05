/////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSGraph.H>
#include <math.h>
#include <unistd.h>
 
extern const int  MSDegPerRadian;
void MSGraph::drawGraph(MSBoolean update_)
{
  switch(orientation())
   {
   case Horizontal:
     drawHtraces(update_);
     break;
     
   default:
     drawVtraces(update_);
     break;
   }
}

void MSGraph::drawHtraces(MSBoolean update_)
{
  int start,end,bufSize;
  int count=0,textCount=0;
  MSTrace  	   *trace;  
  MSTrace  	  **stackTrace=new MSTrace*[traceList().count()];
  MSTrace  	  **textTrace=new MSTrace*[traceList().count()];
 int               stackCount=0;
  unsigned i=0;
  
  for (i=0;i<traceList().count();i++)
   {
     if ((trace=graphTrace(i))!=0)
      {
        if (trace->style()==Stack)
         {
           stackTrace[stackCount++]=trace;
         }
        else if(trace->style()==MSG::Text)
         {
           if(trace->traceSet()->textLength()!=0&&trace->dataCount()==1)
            {
              textTrace[textCount++]=trace;
            }
         }
      }
   }

  for (i=0;i<traceList().count();i++)
   {
     if ((trace=graphTrace(i))!=0)
      {
	start=update_==MSTrue&&trace->traceSet()->overlap()!=MSTrue?trace->traceSet()->updateIndex():0;
	end=trace->dataCount();
	start=start>end?0:start;
	bufSize=end-start;
	bufSize=bufSize>MSGraph::_MaxBufSize?MSGraph::_MaxBufSize:
          bufSize<MSGraph::_MinBufSize?MSGraph::_MinBufSize:bufSize;
	if (trace->stipple()!=0)
	 {
	   XSetStipple(display(),traceGC(),trace->stipple());
	   XSetFillStyle(display(),traceGC(),FillOpaqueStippled);
	 }
	switch (trace->style())
	 {
         case Bar:
           plotBarTrace(trace,start,end,bufSize,count);
	   count++;
	   break;
         case Stack:
           if (stackCount>0) plotStackTrace(stackTrace,stackCount,count,update_);
	   count++;
	   break;
         }
      }
   }
  if (textCount>0) plotTextTrace(textTrace,textCount);
  delete [] textTrace;
  delete [] stackTrace;
}


void MSGraph::drawVtraces(MSBoolean update_)
{
  int               areaCount=0,stackCount=0,textCount=0,fillCount=0,pieCount=0,count=0;
  unsigned          i;
  XRectangle        clipRect;
  int 		    start,end,bufSize;
  MSTrace  	   *trace;
  MSTrace  	  **areaTrace=new MSTrace*[traceList().count()];
  MSTrace  	  **stackTrace=new MSTrace*[traceList().count()];
  MSTrace  	  **pieTrace=new MSTrace*[traceList().count()];
  MSTrace  	  **textTrace=new MSTrace*[traceList().count()];
  MSTrace  	  **fillTrace=new MSTrace*[traceList().count()];
  int 		    rule_width=axisRuleWidth()==0?1:axisRuleWidth();
  _marketProfileStatus=MSFalse;
  if (update_!=MSTrue)
   {
     clipRect.x=x_org(); //-rule_width;
     clipRect.y=y_end(); //-rule_width;
     clipRect.width=plotAreaRect()->width()+rule_width;
     clipRect.height=plotAreaRect()->height()+rule_width;
     XSetClipRectangles(display(),traceGC(),0,0,&clipRect,1,Unsorted);
   }
  
  for (i=0;i<traceList().count();i++)
   {
     if ((trace=graphTrace(i))!=0)
      {
	if (trace->style()==Pie)
	 {
	   pieTrace[pieCount++]=trace;
	 }
	else if (trace->style()==Area)
	 {
	   areaTrace[areaCount++]=trace;
	 }
	else if (trace->style()==Stack)
	 {
	   stackTrace[stackCount++]=trace;
	 }
	else if (trace->style()==MSG::Text&&trace->traceSet()->textLength()!=0&&trace->dataCount()==1)
	 {
	   textTrace[textCount++]=trace;
	 }
	else if (trace->style()==Fill)
	 {
	   fillTrace[fillCount++]=trace;
	 }
      }
   }
  if (areaCount>0) plotAreaTrace(areaTrace,areaCount,update_);
  for (i=0;i<fillCount;i++) plotFillTrace(fillTrace[i],update_);
  for (i=0;i<pieCount;i++) plotPieTrace(pieTrace[i]);
  for (i=0;i<traceList().count();i++)
   {
     if ((trace=graphTrace(i))!=0)
      {
	start=update_==MSTrue&&trace->traceSet()->overlap()!=MSTrue?trace->traceSet()->updateIndex():0;
	end=trace->dataCount();
	start=start>end?0:start;

        bufSize=end-start;
	bufSize=bufSize>MSGraph::_MaxBufSize?MSGraph::_MaxBufSize:
	          bufSize<MSGraph::_MinBufSize?MSGraph::_MinBufSize:bufSize;
	if (trace->stipple()!=0)
	 {
	   XSetStipple(display(),traceGC(),trace->stipple());
	   XSetFillStyle(display(),traceGC(),FillOpaqueStippled);
	 }
	switch (trace->style())
	 {
	 case MSG::Text:
	 case Area:
	 case Fill:
	   break;
	   
	 case Line:
	 case Line|Scatter:
	   plotLineTrace(trace,start,end,bufSize);
	   if (trace->style()==Line) break;
	 case Scatter:
	   plotScatterTrace(trace,start,end,bufSize);
	   if (update_!=MSTrue) XSetClipRectangles(display(),traceGC(),0,0,&clipRect,1,Unsorted);
	   break;

	 case Step:
	 case Step|Scatter:
	   plotStepTrace(trace,start,end,bufSize);
	   if (trace->style()==Step) break;
	   plotScatterTrace(trace,start,end,bufSize);
	   if (update_!=MSTrue) XSetClipRectangles(display(),traceGC(),0,0,&clipRect,1,Unsorted);
	   break;
	   
	 case MSG::Outline:
	   plotOutlineTrace(trace,start,end,bufSize);
	   break;
	   	   
	 case Bar:
	   plotBarTrace(trace,start,end,bufSize,count);
	   count++;
	   break;
	   
	 case Stack:
	   if (stackCount>0) plotStackTrace(stackTrace,stackCount,count,update_);
	   count++;
	   break;
	   
	 case Close:
	   plotLineTrace(trace,start,end,bufSize);
	   break;

	 case HL:
	   plotHighLowTrace(trace,start,end,bufSize);
	   break;
	   
	 case HLOC:
	   plotOpenTicks(trace,start,end,bufSize);
	   plotCloseTicks(trace,start,end,bufSize);
	   plotHighLowTrace(trace,start,end,bufSize);
	   break;
	   
	 case HLC:
	   plotCloseTicks(trace,start,end,bufSize);
	   plotHighLowTrace(trace,start,end,bufSize);
	   break;

	 case Candle:
	   plotCandleTrace(trace,start,end,bufSize);
	   break;
	   
	 case Osc:
	   plotLineTrace(trace,start,end,bufSize);
//	   plotBetweenTrace(trace,update_);
	   break;

	 case MarketProfile:
	 case ColorProfile:
	   if (update_!=MSTrue) plotMarketProfile(trace,bufSize);
	   _marketProfileStatus=MSTrue;
	   break;

	 case Segment:
	   plotSegmentTrace(trace,graphPixmap(),traceGC());
	   break;
	 }
      }
   }     
  if (textCount>0) plotTextTrace(textTrace,textCount);
  delete [] areaTrace;
  delete [] pieTrace;
  delete [] stackTrace;
  delete [] textTrace;
  delete [] fillTrace;
}

void MSGraph::plotLineTrace(MSTrace *trace_,int start_,int end_,int bufSize_)
{
  int 		k,xs,ys;
  int 		x,y;
  int 		pointCount=0;
  int	        xLast=0,yLast=0;
  XPoint       *points;

  if (trace_->dataCount()<2) return;
  start_=start_!=0?start_-1:start_;
  xs=trace_->xAxis(); ys=trace_->yAxis();
  XSetForeground(display(),traceGC(),trace_->lineColor());
  setLineAttributes(trace_->lineStyle(),trace_->lineWeight(),traceGC(),trace_->lineWidth(),CapButt,JoinRound);
  points=new XPoint[bufSize_+2];
  for (k=start_;k<end_;k++)
   {
     x=xValueToPixel(xValue(trace_,k),xs);
     y=yValueToPixel(trace_->y(k),ys);
     if (x==xLast&&y==yLast) continue;
     xLast=x; yLast=y;
     points[pointCount].x=x;
     points[pointCount++].y=y;
     if (pointCount>=bufSize_)
      {
	XDrawLines(display(),graphPixmap(),traceGC(),points,pointCount,CoordModeOrigin);
	pointCount=0;
      }
   }
  if (pointCount>0)
   {
     XDrawLines(display(),graphPixmap(),traceGC(),points,pointCount,CoordModeOrigin);
   }
  delete [] points;
}

void MSGraph::plotSegmentTrace(MSTrace *trace_,Window xwin_,GC gc_)
{
  int 		k,xs,ys;
  int 		x,y,xx,yy;
  int 		pointCount=0;
  int	        x1_last=0,y1_last=0;
  int	        x2_last=0,y2_last=0;
  XSegment     *segments;
  
  if (trace_->dataCount()<2) return;
  xs=trace_->xAxis(); ys=trace_->yAxis();
  int bufSize=trace_->dataCount()/2;;
  bufSize=bufSize>MSGraph::_MaxBufSize?MSGraph::_MaxBufSize:
	    bufSize<MSGraph::_MinBufSize?MSGraph::_MinBufSize:bufSize;
  segments=new XSegment[bufSize];
  if (gc_==traceGC())
   {
     XSetForeground(display(),traceGC(),trace_->lineColor());
     setLineAttributes(trace_->lineStyle(),trace_->lineWeight(),traceGC(),trace_->lineWidth(),CapButt,JoinRound);
   }
  for (k=0;k<trace_->dataCount()-1;k+=2)
   {
     x=xValueToPixel(xValue(trace_,k),xs);
     y=yValueToPixel(trace_->y(k),ys);
     xx=xValueToPixel(xValue(trace_,k+1),xs);
     yy=yValueToPixel(trace_->y(k+1),ys);
     if (x==x1_last&&y==y1_last&&xx==x2_last&&y==y2_last) continue;
     x1_last=x; y1_last=y; x2_last=xx; y2_last=yy;
     segments[pointCount].x1=x+trace_->xOffset();
     segments[pointCount].y1=y+trace_->yOffset();
     segments[pointCount].x2=xx+trace_->xOffset();
     segments[pointCount++].y2=yy+trace_->yOffset();
     if (pointCount>=bufSize)
      {
	XDrawSegments(display(),xwin_,gc_,segments,pointCount);
	pointCount=0;
      }
   }
  XDrawSegments(display(),xwin_,gc_,segments,pointCount);
  delete [] segments;
}

void MSGraph::plotFillTrace(MSTrace *trace_,MSBoolean update_)
{
  int 		k,xs,ys;
  int 		x,y;
  int 		pointCount=0;
  int	        xLast=0,yLast=0;
  XPoint       *points;

  if (trace_->dataCount()<2) return;
  int start=update_==MSTrue?trace_->traceSet()->updateIndex():0;
  int end=trace_->dataCount();
  int bufSize=end-start+2;
  bufSize=bufSize>MSGraph::_MaxBufSize?MSGraph::_MaxBufSize:
	    bufSize<MSGraph::_MinBufSize?MSGraph::_MinBufSize:bufSize;
  xs=trace_->xAxis(); ys=trace_->yAxis();
  XSetForeground(display(),traceGC(),trace_->lineColor());
  setLineAttributes(trace_->lineStyle(),trace_->lineWeight(),traceGC(),trace_->lineWidth(),CapButt,JoinRound);
  points=new XPoint[bufSize];
  if (update_==MSTrue)
   {
     x=xValueToPixel(xValue(trace_,0),xs);
     y=yValueToPixel(trace_->y(0),ys);
     points[pointCount].x=x;
     points[pointCount++].y=y;
   }
  for (k=start;k<end;k++)
   {
     x=xValueToPixel(xValue(trace_,k),xs);
     y=yValueToPixel(trace_->y(k),ys);
     if (x==xLast&&y==yLast) continue;
     xLast=x; yLast=y;
     points[pointCount].x=x;
     points[pointCount++].y=y;
     if (pointCount>=bufSize)
      {
	points[pointCount].x=points[0].x;
	points[pointCount++].y=points[0].y;
	XSetForeground(display(),traceGC(),trace_->fillColor());
	PFillPolygon(display(),graphPixmap(),traceGC(),points,pointCount,Convex,CoordModeOrigin);
	XSetForeground(display(),traceGC(),trace_->lineColor());
	XDrawLines(display(),graphPixmap(),traceGC(),points,pointCount,CoordModeOrigin);
	pointCount=0;
      }
   }
  if (pointCount>0)
   {
     points[pointCount].x=points[0].x;
     points[pointCount++].y=points[0].y;
     XSetForeground(display(),traceGC(),trace_->fillColor());
     PFillPolygon(display(),graphPixmap(),traceGC(),points,pointCount,Convex,CoordModeOrigin);
     XSetForeground(display(),traceGC(),trace_->lineColor());
     XDrawLines(display(),graphPixmap(),traceGC(),points,pointCount,CoordModeOrigin);
   }
  delete [] points;
}

void MSGraph::plotStepTrace(MSTrace *trace_,int start_,int end_,int bufSize_)
{
  int 		k,xs,ys;
  int 		x,y;
  int 		pointCount=0;
  int	        xLast=0,yLast=0;
  XPoint       *points;
  int 		bufsize=bufSize_*2;
  
  if (trace_->dataCount()<2) return;
  xs=trace_->xAxis();
  ys=trace_->yAxis();
  
  if (trace_->dataCount()<2) return;
  end_=start_==0&&end_==1?2:end_;
  start_=start_!=0?start_-2:start_;
  XSetForeground(display(),traceGC(),trace_->lineColor());
  setLineAttributes(trace_->lineStyle(),trace_->lineWeight(),traceGC(),trace_->lineWidth(),CapButt,JoinRound);
  points=new XPoint[bufsize];
  for (k=start_;k<end_;k++)
   {
     x=xValueToPixel(xValue(trace_,k)+trace_->xOffset(),xs);
     y=yValueToPixel(trace_->y(k)+trace_->yOffset(),ys);
     if (x==xLast&&y==yLast) continue;
     xLast=x; yLast=y;
     points[pointCount].x=x;
     points[pointCount++].y=y;
     if (pointCount>=bufsize)
      {
	XDrawLines(display(),graphPixmap(),traceGC(),points,pointCount,CoordModeOrigin);
	pointCount=0;
      }
     if (k<trace_->dataCount()-1)
      {
	x=xValueToPixel(xValue(trace_,k+1)+trace_->xOffset(),xs);
	points[pointCount].x=x;
	points[pointCount++].y=y;
      }
   }
  if (pointCount>0)
   {
     XDrawLines(display(),graphPixmap(),traceGC(),points,pointCount,CoordModeOrigin);
   }
  delete [] points;
}

void MSGraph::plotOutlineTrace(MSTrace *,int ,int ,int )
{
}

void MSGraph::plotAreaTrace(MSTrace **trace_,int count_,MSBoolean update_)
{
  int 		i,j,k,xs,ys;
  int 		pointCount=0;
  int 		x,y,xLast=0,yLast=0;
  double 	y_value;
  XPoint       *points;
  int 		bufSize;
  int 		startIncrement;
  int 		endIncrement;
  int 		startSign=0;
  MSBoolean     negative=MSFalse;
  
  for (i=0;i<count_;i++) if (trace_[i]->traceSet()->yMin()<0) { startSign=-1; break; }
  for (i=0;i<count_;i++)
   {
     startIncrement=update_==MSTrue?trace_[i]->traceSet()->updateIndex():0;
     endIncrement=trace_[i]->dataCount();
     bufSize=endIncrement-startIncrement;
     bufSize=bufSize>MSGraph::_MaxBufSize?MSGraph::_MaxBufSize:
               bufSize<MSGraph::_MinBufSize?MSGraph::_MinBufSize:bufSize;
     points=new XPoint[bufSize+2];
     xs=trace_[i]->xAxis();
     ys=trace_[i]->yAxis();
     setLineAttributes(trace_[i]->lineStyle(),trace_[i]->lineWeight(),traceGC(),
		       trace_[i]->lineWidth(),CapButt,JoinRound);
  
     for (int sign=startSign;sign<1;sign++)
      {
	for (k=startIncrement;k<endIncrement;k++)
	 {
	   if ((k>2&&xValue(trace_[i],k-2)<xMinData(xs))&&
	       (k<trace_[i]->dataCount()-2&&xValue(trace_[i],k+2)<xMaxData(xs))) continue;
	   for (j=i,y_value=0;j<count_;j++)
	    {
	      double yy=trace_[j]->y(k);
	      if(yy<0) negative=MSTrue;
	      y_value+=sign<0?yy<0?yy:0:yy<0?0:yy;
	    }
	   x=xValueToPixel(xValue(trace_[i],k),xs);
	   y=yValueToPixel(y_value,ys);
	   if (x==xLast&&y==yLast) continue;
	   xLast=x; yLast=y;
	   points[pointCount].x=x;
	   points[pointCount++].y=y;
	   if (pointCount>=bufSize)
	    {
	      if (sign==0||negative==MSTrue)
	       {
		 x=xValueToPixel(xValue(trace_[i],endIncrement-1),xs);
		 y=yValueToPixel(negative==MSTrue?0:yMinData(ys),ys);
		 points[pointCount].x=x;
		 points[pointCount++].y=y;
		 x=xValueToPixel(xValue(trace_[i],startIncrement),xs);
		 points[pointCount].x=x;
		 points[pointCount++].y=y;
		 XSetForeground(display(),traceGC(),trace_[i]->fillColor());
		 PFillPolygon(display(),graphPixmap(),traceGC(),points,pointCount,Complex,CoordModeOrigin);
		 XSetForeground(display(),traceGC(),trace_[i]->lineColor());
		 XDrawLines(display(),graphPixmap(),traceGC(),points,pointCount-2,CoordModeOrigin);
	       }
	      pointCount=0;
	    }
	 }
	if (sign==0||negative==MSTrue)
	 {
	   x=xValueToPixel(xValue(trace_[i],endIncrement-1),xs);
	   y=yValueToPixel(negative==MSTrue?0:yMinData(ys),ys);
	   points[pointCount].x=x;
	   points[pointCount++].y=y;
	   x=xValueToPixel(xValue(trace_[i],startIncrement),xs);
	   points[pointCount].x=x;
	   points[pointCount++].y=y;
	   XSetForeground(display(),traceGC(),trace_[i]->fillColor());
	   PFillPolygon(display(),graphPixmap(),traceGC(),points,pointCount,Complex,CoordModeOrigin);
	   XSetForeground(display(),traceGC(),trace_[i]->lineColor());
	   XDrawLines(display(),graphPixmap(),traceGC(),points,pointCount-2,CoordModeOrigin);
	 }
	pointCount=0;
      }
     delete [] points;
   }
}  

void MSGraph::plotScatterTrace(MSTrace *trace_,int start_,int end_,int bufSize_)
{
  int 	       		k,bs,col=trace_->virtualCol();
  int	       		x,y,xs,ys;
  double       		x_value,y_value;
  XRectangle        	clipRect;
  char*			array=0;
  int	       		pointCount=0;
  MSBoolean		updateColor=MSFalse;
  unsigned long 	lc,fc;
  static unsigned long 	lineColor=0,fillColor=0;
  int			xLast=0,yLast=0;
  PSF			func;
  XFontStruct  	       *fi=(XFontStruct *)server()->fontStruct(trace_->font());

  lineColor=trace_->lineColor(); 
  fillColor=trace_->fillColor(); 
  XSetForeground(display(),traceGC(),fillColor);
  XSetLineAttributes(display(),traceGC(),1,LineSolid,CapButt,JoinMiter);

  if (start_==0)
   {
     clipRect.x=x_org()-trace_->symbolSize();
     clipRect.y=y_end()-trace_->symbolSize();
     clipRect.width=plotAreaRect()->width()+trace_->symbolSize()<<1;
     clipRect.height=plotAreaRect()->height()+trace_->symbolSize()<<1;
     XSetClipRectangles(display(),traceGC(),0,0,&clipRect,1,Unsorted);
   }
  else start_--;
  
  switch (trace_->symbol())
   {
   case Square: 
   case Square|Fill: 
   case Square|Circle: 
     func=&MSGraph::buildSquareSymbol;
     array=new char[sizeof(XRectangle)*bufSize_];
     break;

   case Circle:
   case Circle|Fill:
     func=&MSGraph::buildCircleSymbol;
     array=new char[sizeof(XArc)*bufSize_];
     break;

   case X:
     func=&MSGraph::buildXSymbol;
     array=new char[sizeof(XSegment)*2*bufSize_];
     break;
   case Cross:
     func=&MSGraph::buildCrossSymbol;
     array=new char[sizeof(XSegment)*2*bufSize_];
     break;
   case X|Cross:
     func=&MSGraph::buildStarSymbol;
     array=new char[sizeof(XSegment)*4*bufSize_];
     break;
   case Triangle:
     if (outputMode()==Print||trace_->symbolSize()<3)
      {
	func=&MSGraph::buildTrianglePrintSymbol;
	array=new char[sizeof(XSegment)*3*bufSize_];
	break;
      }

   case Triangle|Fill:
     func=&MSGraph::buildTriangleSymbol;
     bs=trace_->symbolSize()*3*bufSize_;
     bs=(int)(bs<XMaxRequestSize(display())?bs:(XMaxRequestSize(display())/2));
     array=new char[sizeof(XPoint)*bs];
     break;
   case Diamond:
     if (outputMode()==Print||trace_->symbolSize()<3)
      {
	func=&MSGraph::buildDiamondPrintSymbol;
	array=new char[sizeof(XSegment)*4*bufSize_];
	break;
      }

   case Diamond|Fill:
     func=&MSGraph::buildDiamondSymbol;
     bs=trace_->symbolSize()*2*bufSize_;
     bs=(int)(bs<XMaxRequestSize(display())?bs:(XMaxRequestSize(display())/2));
     array=new char[sizeof(XPoint)*bs];
     break;
   case MSG::Text:
     func=&MSGraph::buildCharSymbol;
     array=new char[sizeof(XPoint)*bufSize_];
     XSetFont(display(),traceGC(),trace_->font());
     break;
   default:
     return;
   }
  xs=trace_->xAxis();
  ys=trace_->yAxis();
  int end=end_-1;
  for (k=start_;k<end_;k++)
   {
     if ((x_value=xValue(trace_,k))<xMinData(xs)||x_value>xMaxData(xs)) continue;
     if ((y_value=trace_->y(k))<yMinData(ys)||y_value>yMaxData(ys)) continue;
     x=xValueToPixel(x_value,xs);
     y=yValueToPixel(y_value,ys);
     if (x==xLast&&y==yLast) continue;
     xLast=x; yLast=y;
     (this->*func)((void*)array,pointCount,x,y,trace_->symbolSize());
     if (k<end)
      {
        if ((fc=trace_->fillColor(k+1,col))!=fillColor)
         {
           updateColor=MSTrue;
           fillColor=fc;
         }
        if ((lc=trace_->lineColor(k+1,col))!=lineColor)
         {
           updateColor=MSTrue;
           lineColor=lc;
         }
      }
     if (updateColor==MSTrue||pointCount>=bufSize_)
      {
	drawScatterSymbols(graphPixmap(),traceGC(),trace_,array,pointCount,lineColor,fillColor,fi);
	pointCount=0;
	updateColor=MSFalse;
	fillColor=trace_->fillColor(k+1);
	lineColor=trace_->lineColor(k+1);
      }   
   }
  if (pointCount>0)
   {
     drawScatterSymbols(graphPixmap(),traceGC(),trace_,array,pointCount,lineColor,fillColor,fi);
   }
  delete [] array;
}

void MSGraph::drawScatterSymbols(Window xwin_,GC gc_,MSTrace *trace_,void *array_,int count_,
			      unsigned long lineColor_,unsigned long fillColor_,
			      XFontStruct *fi_)
{
  int 	size;
  
  XSetForeground(display(),traceGC(),fillColor_);
  switch (trace_->symbol())
   {
   case Square|Fill: 
     XFillRectangles(display(),xwin_,gc_,(XRectangle*)array_,count_);
     if (fillColor_==lineColor_) break;
     XSetForeground(display(),gc_,lineColor_);
   case Square:
     XDrawRectangles(display(),xwin_,gc_,(XRectangle*)array_,count_);
     break;
   case Circle|Fill:
     XFillArcs(display(),xwin_,gc_,(XArc*)array_,count_);
     if (fillColor_==lineColor_) break;
     XSetForeground(display(),gc_,lineColor_);
   case Circle:
     XDrawArcs(display(),xwin_,gc_,(XArc*)array_,count_);
     break;
   case Cross: 
   case X: 
   case X|Cross: 
     XDrawSegments(display(),xwin_,gc_,(XSegment *)array_,count_);
     break;
   case Triangle: 
   case Diamond: 
     if (outputMode()==Print||trace_->symbolSize()<3)
      {
	XDrawSegments(display(),xwin_,gc_,(XSegment *)array_,count_);
      }
     else
      {
	XDrawPoints(display(),xwin_,gc_,(XPoint*)array_,count_,CoordModeOrigin);
      }
     break;
   case Triangle|Fill: 
     size=3*(trace_->symbolSize()-1);
     XDrawPoints(display(),xwin_,gc_,(XPoint*)array_,count_,CoordModeOrigin);
     fillPolygons(display(),xwin_,gc_,(XPoint*)array_,count_,
		  Convex,CoordModeOrigin,size);
     break;
   case Diamond|Fill: 
     size=4*(trace_->symbolSize()-1)/2;
     XDrawPoints(display(),xwin_,gc_,(XPoint*)array_,count_,CoordModeOrigin);
     fillPolygons(display(),xwin_,gc_,(XPoint*)array_,count_,
		  Convex,CoordModeOrigin,size);
     break;
   case MSG::Text: 
     {
       if (trace_->textSymbol().length()>0&&fi_!=0)
        {
          XPoint *pt=(XPoint*)array_;
          int h=(fi_->ascent+fi_->descent)/2;
          int len=trace_->textSymbol().length();
          for (int i=0;i<count_;i++)
           {
             const char *cp=trace_->textSymbol()(i%len);
             int strl=strlen(cp);
             int w=XTextWidth(fi_,cp,strl)/2;
             int x=pt[i].x-w;
             int y=pt[i].y+h;
             XDrawString(display(),xwin_,gc_,fi_,x,y,cp,strl);
           }
        }
       break;
     }
   }
}
void MSGraph::plotBarTrace(MSTrace *trace_,int start_,int end_,int bufSize_,int ct_)
{

  int    	xs,ys;
  int		x,y,y0,x0,xLast=0,yLast=0;
  int		pointCount=0;
  int	 	bar_offset;
  double 	x_value, y_value;
  XRectangle   *rects;
  MSBoolean	updateColor=MSFalse;
  unsigned long lineColor,fillColor,fc;
  int           col=trace_->virtualCol();
  
  lineColor=trace_->lineColor(start_,col);
  fillColor=trace_->fillColor(start_,col); 
  xs=trace_->xAxis();
  ys=trace_->yAxis();
  rects =new XRectangle[bufSize_];
  XSetLineAttributes(display(),traceGC(),1,LineSolid,CapProjecting,JoinMiter);
  if (orientation() == Horizontal)
   {
     x=(x_value=trace_->traceSet()->xDelta()*yScale(ys))<MSGraph::_SHRT_MAX?
       (int)x_value:MSGraph::_SHRT_MAX;
   }
  else
   {
     x=(x_value=trace_->traceSet()->xDelta()*xScale(xs))<MSGraph::_SHRT_MAX?
       (int)x_value:MSGraph::_SHRT_MAX;
   }
  barWidth(((barCount()+1)*maxBarWidth()>x-2)?(x-2)/(barCount()+1): maxBarWidth());
  xBar(barCount()*barWidth()/2);
  bar_offset=ct_*barWidth()-xBar();
  trace_->xShift(-bar_offset/xScale(xs));
  y0=yValueToPixel(0.0,ys);
  y0=(y0>y_org())?y_org():y0;
  y0=(y0<y_end())?y_end():y0;

  x0=xValueToPixel(0.0,xs);
  x0=(x0<x_org())?x_org():x0;
  x0=(x0>x_end())?x_end():x0;
  
  for (int k=start_;k<end_;k++)
   {
     x_value=xValue(trace_,k);
     y_value=yValue(trace_,k);
     
     if (k<end_-1&&((fc=trace_->fillColor(k+1,col))!=fillColor||
                 trace_->lineColor(k+1,col)!=lineColor)) updateColor=MSTrue;
     if (x_value<xMinData(xs))
      {
	if (updateColor==MSTrue)
	 {
	   fillColor=fc;
	   lineColor=trace_->lineColor(k+1);
	   updateColor=MSFalse;
	 }
	continue;
      }

     if (orientation()==Horizontal&&(y_value>yMaxData(ys)||y_value<yMinData(ys))) continue;
     else if (orientation()!=Horizontal&&x_value>xMaxData(xs)) continue;
     
     x=xValueToPixel(x_value,xs);
     y=yValueToPixel(y_value,ys);
     
     if (x==xLast&&y==yLast) continue;
     xLast=x; yLast=y;

     if (orientation() == Horizontal) 
      {
        rects[pointCount].y=y+bar_offset;
        rects[pointCount].x=x>x0?x0:x; 
        rects[pointCount].width=abs(x-x0);  
        rects[pointCount].height=(barWidth()>2)?barWidth()-1:1;
      }
     else
      {
        rects[pointCount].x=x+bar_offset;
        rects[pointCount].y=y>y0?y0:y;
        rects[pointCount].width=(barWidth()>2)?barWidth()-1:1;
        rects[pointCount].height=abs(y0-y);
      }

     pointCount++;
     
     if (pointCount>=bufSize_||updateColor==MSTrue)
      {
	XSetForeground(display(),traceGC(),fillColor);
	PFillRectangles(display(),graphPixmap(),traceGC(),rects,pointCount);
	if (barWidth()>=5)
	 {
	   XSetForeground(display(),traceGC(),lineColor);
	   XDrawRectangles(display(),graphPixmap(),traceGC(),rects,pointCount);
	 }
	pointCount=0;
	if (updateColor==MSTrue)
	 {
	  
	   fillColor=fc;
	   lineColor=trace_->lineColor(k+1);
	   updateColor=MSFalse;
	 }
      }
   }
  XSetForeground(display(),traceGC(),fillColor);
  PFillRectangles(display(),graphPixmap(),traceGC(),rects,pointCount);
  if (barWidth()>=5)
   {
     XSetForeground(display(),traceGC(),lineColor);
     XDrawRectangles(display(),graphPixmap(),traceGC(),rects,pointCount);
   }
  delete [] rects;
}

void MSGraph::plotHBarTrace(MSTrace *trace_)
{
  if (trace_==0)return;
}

int MSGraph::setLineWidth(MSTrace *trace_)
{
  int 	x,k,offset,line_width;
  
  x=(int)(trace_->traceSet()->xDelta()*xScale(0));
  if (trace_->style()==HL)
   {	
     line_width=((k=(trace_->lineWidth()>=x)?x-1:trace_->lineWidth())<1)?1:k;
   }
  else
   {
     if (trace_->style()==HLC)offset=2;
     else if (trace_->style()==HLOC)offset=3;
     else if (trace_->style()==Candle)offset=5;
     line_width=((k=(offset*trace_->lineWidth()>=x)?
		    ((x%offset)==0)?(x/offset)-1:x/offset:trace_->lineWidth())<1)?1:k;
   }
  return line_width;
}

void MSGraph::plotHighLowTrace(MSTrace *trace_,int start_,int end_,int bufSize_)
{
  int 		k,line_width;
  double 	x_value=0;
  int 	        x,y1,y2;
  int 		high=0,low=1;
  int 		pointCount=0;
  XSegment     *hlPoints;
  MSBoolean	updateColor=MSFalse;
  unsigned long line_color=trace_->lineColor(high+trace_->offset());

  int xs=trace_->xAxis();
  int ys=trace_->yAxis();
  line_width=setLineWidth(trace_);
  setLineAttributes(trace_->lineStyle(),trace_->lineWeight(),traceGC(),line_width,CapProjecting,JoinRound);

  hlPoints=new XSegment[bufSize_];

  for (k=start_;k<end_;k++)
   {
     x_value=xValue(trace_,k);
     if (x_value<xMinData(xs)||x_value>xMaxData(xs))continue;
     x=xValueToPixel(x_value,xs);
     y1=yValueToPixel(trace_->y(k,high+trace_->offset()),ys);
     y2=yValueToPixel(trace_->y(k,low+trace_->offset()),ys);
     hlPoints[pointCount].x1=(short)x;
     hlPoints[pointCount].x2=(short)x;
     hlPoints[pointCount].y1=(short)y1;
     hlPoints[pointCount++].y2=(short)y2;
     if (line_color!=trace_->lineColor(k+1,high+trace_->offset()))
      {
	updateColor=MSTrue;
      }
     if (pointCount>=bufSize_||updateColor==MSTrue)
      {
	XSetForeground(display(),traceGC(),line_color);
	XDrawSegments(display(),graphPixmap(),traceGC(),hlPoints,pointCount);
	pointCount=0;
	if (updateColor==MSTrue)
	 {
	   line_color=trace_->lineColor(k+1,high+trace_->offset());
	   updateColor=MSFalse;
	 }
      }
   }
  XSetForeground(display(),traceGC(),line_color);
  XDrawSegments(display(),graphPixmap(),traceGC(),hlPoints,pointCount);
  delete [] hlPoints;
}

void MSGraph::plotOpenTicks(MSTrace *trace_,int start_,int end_,int bufSize_)
{
  int 		k,xs,ys;
  double 	x_value=0;
  int 	        x,y;
  int 		open=0;
  int 		pointCount=0;
  XSegment     *openPoints;
  int		line_width;
  int		tick_width;
  MSBoolean	updateColor=MSFalse;
  unsigned long line_color=trace_->lineColor(open);
  
  xs=trace_->xAxis();
  ys=trace_->yAxis();
  line_width=setLineWidth(trace_);
  tick_width=(line_width>5)?5:line_width;
  x=(int)(trace_->traceSet()->xDelta()*xScale(xs));
  tick_width=x>3&&line_width==1?tick_width*2:tick_width;
  setLineAttributes(trace_->lineStyle(),trace_->lineWeight(),traceGC(),line_width,CapProjecting,JoinMiter);
  openPoints =new XSegment[bufSize_];

  for (k=start_;k<end_;k++)
   {
     x_value=xValue(trace_,k);
     if (x_value<xMinData(xs)||x_value>xMaxData(xs))continue;
     x=xValueToPixel(x_value,xs);
     y=yValueToPixel(trace_->y(k,open),ys);
     openPoints[pointCount].x1=(short)x-tick_width;
     openPoints[pointCount].x2=(short)x;
     openPoints[pointCount].y1=(short)y;
     openPoints[pointCount++].y2=(short)y;
     if (line_color!=trace_->lineColor(k+1,open)) updateColor=MSTrue;
     if (pointCount>=bufSize_||updateColor==MSTrue)
      {
	XSetForeground(display(),traceGC(),line_color);
	XDrawSegments(display(),graphPixmap(),traceGC(),openPoints,pointCount);
	pointCount=0;
	if (updateColor==MSTrue)
	 {
	   line_color=trace_->lineColor(k+1,open);
	   updateColor=MSFalse;
	 }
      }
   }
  XSetForeground(display(),traceGC(),line_color);
  XDrawSegments(display(),graphPixmap(),traceGC(),openPoints,pointCount);
  delete [] openPoints;
}

void MSGraph::plotCloseTicks(MSTrace *trace_,int start_,int end_,int bufSize_)
{
  int 		k,xs,ys;
  double 	x_value=0;
  int 	        x,y;
  int 		close=2+trace_->offset();
  int 		pointCount=0;
  XSegment     *closePoints;
  int		line_width;
  int		tick_width;
  MSBoolean	updateColor=MSFalse;
  unsigned long line_color=trace_->lineColor(close);
  
  xs=trace_->xAxis();
  ys=trace_->yAxis();
  line_width=setLineWidth(trace_);
  tick_width=(line_width>5)?5:line_width;
  x=(int)(trace_->traceSet()->xDelta()*xScale(xs));
  tick_width=x>3&&line_width==1?tick_width*2:tick_width;
  setLineAttributes(trace_->lineStyle(),trace_->lineWeight(),traceGC(),line_width,CapProjecting,JoinMiter);
  closePoints=new XSegment[bufSize_];
  for (k=start_;k<end_;k++)
   {
     x_value=xValue(trace_,k);
     if (x_value<xMinData(xs)||x_value>xMaxData(xs))continue;
     x=xValueToPixel(x_value,xs);
     y=yValueToPixel(trace_->y(k,close),ys);
     closePoints[pointCount].x1=(short)x+tick_width;
     closePoints[pointCount].x2=(short)x;
     closePoints[pointCount].y1=(short)y;
     closePoints[pointCount++].y2=(short)y;
     if (line_color!=trace_->lineColor(k+1,close)) updateColor=MSTrue;
     if (pointCount>=bufSize_||updateColor==MSTrue)
      {
	XSetForeground(display(),traceGC(),line_color);
	XDrawSegments(display(),graphPixmap(),traceGC(),closePoints,pointCount);
	pointCount=0;
	if (updateColor==MSTrue)
	 {
	   line_color=trace_->lineColor(k+1,close);
	   updateColor=MSFalse;
	 }
      }
   }
  XSetForeground(display(),traceGC(),line_color);
  XDrawSegments(display(),graphPixmap(),traceGC(),closePoints,pointCount);
  delete [] closePoints;
}

void MSGraph::plotCandleTrace(MSTrace *trace_,int start_,int end_,int bufSize_)
{
  int 		  h,k,xs,ys;
  double 	  x_value;
  int 		  open=0,high=1,low=2,close=3;
  int 		  pointCount=0,ofCount=0,cfCount=0;
  int 	          x,y_open,y_close,y_high,y_low;
  XRectangle     *ocRects;
  XRectangle     *cfRects;
  XRectangle     *ofRects;
  XSegment       *hPoints;
  XSegment       *lPoints;
  MSBoolean	  updateColor=MSFalse;
  int 		  line_width;
  unsigned long   closecolor=trace_->lineColor(close);
  unsigned long   opencolor=trace_->lineColor(open);
  unsigned long   highcolor=trace_->lineColor(high);
  unsigned long   lowcolor=trace_->lineColor(low);
  
  xs=trace_->xAxis();
  ys=trace_->yAxis();
  start_=start_!=0?start_-1:start_;
  line_width=setLineWidth(trace_);
  trace_->xShift(3*line_width/xScale(xs));
  setLineAttributes(trace_->lineStyle(),trace_->lineWeight(),traceGC(),line_width,CapProjecting,JoinMiter);
  ocRects =new XRectangle[bufSize_];
  cfRects =new XRectangle[bufSize_];
  ofRects =new XRectangle[bufSize_];
  hPoints =new XSegment[bufSize_];
  lPoints =new XSegment[bufSize_];
  for (k=start_;k<end_;k++)
   {
     x_value=xValue(trace_,k);
     if (x_value<xMinData(xs)||x_value>xMaxData(xs))continue;
     x=xValueToPixel(x_value,xs);
     y_high=yValueToPixel(trace_->y(k,high),ys);
     y_low=yValueToPixel(trace_->y(k,low),ys);
     y_open=yValueToPixel(trace_->y(k,open),ys);
     y_close=yValueToPixel(trace_->y(k,close),ys);
     if (y_open<=y_close)// fill rectangle
      {
	ocRects[pointCount].height=(unsigned short)(((h=(y_close-y_open))==0)?line_width:h);
	ocRects[pointCount].width=(unsigned short)(4*line_width);
	ocRects[pointCount].x=(short)(x-2*line_width);
	ocRects[pointCount].y=(short)y_open;
	if (h!=0)
	 {
	   cfRects[cfCount].height=(unsigned short)h;
	   cfRects[cfCount].width=(unsigned short)(4*line_width);
	   cfRects[cfCount].x=(short)(x-2*line_width);
	   cfRects[cfCount++].y=(short)y_open;
	 }
	hPoints[pointCount].x1=(short)x;
	hPoints[pointCount].x2=(short)x;
	hPoints[pointCount].y1=(short)y_open;
	hPoints[pointCount].y2=(short)y_high;
	
	lPoints[pointCount].x1=(short)x;
	lPoints[pointCount].x2=(short)x;
	lPoints[pointCount].y1=(short)y_close;
	lPoints[pointCount++].y2=(short)y_low;
      }
     else if (y_open>y_close) // draw Rectangle
      {
	ocRects[pointCount].x=(short)(x-2*line_width);
	ocRects[pointCount].y=(short)y_close;
	ocRects[pointCount].width=(unsigned short)(4*line_width);
	ocRects[pointCount].height=(unsigned short)(y_open-y_close);
	
	ofRects[ofCount].x=(short)(x-2*line_width);
	ofRects[ofCount].y=(short)y_close;
	ofRects[ofCount].width=(unsigned short)(4*line_width);
	ofRects[ofCount++].height=(unsigned short)(y_open-y_close);
	
	hPoints[pointCount].x1=(short)x;
	hPoints[pointCount].x2=(short)x;
	hPoints[pointCount].y1=(short)y_close;
	hPoints[pointCount].y2=(short)y_high;
	
	lPoints[pointCount].x1=(short)x;
	lPoints[pointCount].x2=(short)x;
	lPoints[pointCount].y1=(short)y_open;
	lPoints[pointCount++].y2=(short)y_low;
      }
     if (opencolor!=trace_->lineColor(k+1,open)&&
	 highcolor!=trace_->lineColor(k+1,high)&&
	 lowcolor!=trace_->lineColor(k+1,low)&&
	 closecolor!=trace_->lineColor(k+1,close))
      {
	updateColor=MSTrue;
      }
     if (pointCount>=bufSize_||updateColor==MSTrue)
      {
	XSetForeground(display(),traceGC(),highcolor);
	XDrawSegments(display(),graphPixmap(),traceGC(),hPoints,pointCount);
	XDrawSegments(display(),graphPixmap(),traceGC(),lPoints,pointCount);
	XSetForeground(display(),traceGC(),opencolor);
	PFillRectangles(display(),graphPixmap(),traceGC(),cfRects,cfCount);
	XSetForeground(display(),traceGC(),closecolor==opencolor?background():closecolor);
	XFillRectangles(display(),graphPixmap(),traceGC(),ofRects,ofCount);
	XSetForeground(display(),traceGC(),lowcolor);
	XDrawRectangles(display(),graphPixmap(),traceGC(),ocRects,pointCount);
	pointCount=cfCount=ofCount=0;
	if (updateColor==MSTrue)
	 {
	   closecolor=trace_->lineColor(k+1,close);
	   opencolor=trace_->lineColor(k+1,open);
	   highcolor=trace_->lineColor(k+1,high);
	   lowcolor=trace_->lineColor(k+1,low);
	   updateColor=MSFalse;
	 }
      }
   }
  if (pointCount>0)
   {
     XSetForeground(display(),traceGC(),highcolor);
     XDrawSegments(display(),graphPixmap(),traceGC(),hPoints,pointCount);
     XDrawSegments(display(),graphPixmap(),traceGC(),lPoints,pointCount);
     XSetForeground(display(),traceGC(),opencolor);
     PFillRectangles(display(),graphPixmap(),traceGC(),cfRects,cfCount);
     XSetForeground(display(),traceGC(),closecolor==opencolor?background():closecolor);
     XFillRectangles(display(),graphPixmap(),traceGC(),ofRects,ofCount);
     XSetForeground(display(),traceGC(),lowcolor);
     XDrawRectangles(display(),graphPixmap(),traceGC(),ocRects,pointCount);
   }
  delete [] ocRects;
  delete [] cfRects;
  delete [] ofRects;
  delete [] hPoints;
  delete [] lPoints;
}

void MSGraph::plotStackTrace(MSTrace **trace_,int& ct_,int count_,MSBoolean update_)
{
  int 		i,j,k,xs,ys;
  int 		pointCount=0;
  int 		x,y,y0,x0,yi,xLast=0,yLast=0; // xi
  double 	yy,xx,x_value,y_value;
  XRectangle   *rects;
  int 		bufSize;
  int	 	bar_offset;
  int 		startIncrement;
  int 		endIncrement;
  unsigned long lineColor;
  unsigned long fillColor;
  int 		sign=0;
  int 		ct=ct_;

  for (i=0;i<ct;i++)
   {
     startIncrement=update_==MSTrue?trace_[i]->traceSet()->updateIndex():0;
     endIncrement=trace_[i]->dataCount();
     bufSize=endIncrement-startIncrement;
     bufSize=bufSize>MSGraph::_MaxBufSize?MSGraph::_MaxBufSize:
             bufSize<MSGraph::_MinBufSize?MSGraph::_MinBufSize:bufSize;
     rects=new XRectangle[bufSize];
     xs=trace_[i]->xAxis();
     ys=trace_[i]->yAxis();
     if (i==0)
      {
        if (orientation()==Horizontal)
         {
           x=(x_value=trace_[i]->traceSet()->xDelta()*yScale(ys))<MSGraph::_SHRT_MAX?
             (int)x_value:MSGraph::_SHRT_MAX;
         }
        else
         {
           x=(x_value=trace_[i]->traceSet()->xDelta()*xScale(xs))<MSGraph::_SHRT_MAX?
             (int)x_value:MSGraph::_SHRT_MAX;
         }
        barWidth(((barCount()+1)*maxBarWidth()>x-2)?(x-2)/(barCount()+1):maxBarWidth());
	xBar(barCount()*barWidth()/2);
	bar_offset=count_*barWidth()-xBar();
	trace_[i]->xShift(-bar_offset/xScale(xs));
	XSetLineAttributes(display(),traceGC(),1,LineSolid,CapProjecting,JoinMiter);
      }
     y0=yValueToPixel(0.0,ys);
     y0=(y0>y_org())?y_org():y0;
     y0=(y0<y_end())?y_end():y0;

     x0=xValueToPixel(0.0,xs);
     x0=(x0<x_org())?x_org():x0;
     x0=(x0>x_end())?x_end():x0;

     lineColor=trace_[i]->lineColor(); 
     fillColor=trace_[i]->fillColor(); 
     for (k=startIncrement;k<endIncrement;k++)
      {
        if (orientation()==Horizontal)
         {
           y_value=yValue(trace_[i],k);
           if (y_value<yMinData(ys)||y_value>yMaxData(ys)) continue;
           for (j=i,x_value=0.,sign=0;j<ct;j++)
            {
              xx=xValue(trace_[j],k);
              if(j==i&&xx<0) sign=-1;
              x_value+=sign<0?xx<0?xx:0:xx<0?0:xx;
            }
           if (x_value<xMinData(xs)) continue;
           x=xValueToPixel(x_value,xs);
           y=yValueToPixel(y_value,ys);
           if (x==xLast&&y==yLast) continue;
           xLast=x; yLast=y;

//           for (j=ct-1,x_value=0.;j>i;j--)
//             {
//               xx=trace_[j]->y(k);
//               x_value+=sign<0?xx<0?xx:0:xx<0?0:xx;
//             }
//            cout << "x_value again" << x_value << endl;
//            xi=yValueToPixel(x_value,ys);
           
           rects[pointCount].y=y+bar_offset;
           rects[pointCount].x=x>x0?x0:x; //sign<0?xi:x;
           rects[pointCount].width=abs(x0-x);//(i<ct-1)?abs(xi-x):abs(x0-x);
           rects[pointCount++].height=(barWidth()>2)?barWidth()-1:1;
         }
        else
         {
           if (xValue(trace_[i],k)<xMinData(xs)&&xValue(trace_[i],k)<xMaxData(xs)) continue;
           for (j=i,y_value=0.,sign=0;j<ct;j++)
            {
              yy=trace_[j]->y(k);
              if(j==i&&yy<0) sign=-1;
              y_value+=sign<0?yy<0?yy:0:yy<0?0:yy;
            }
           x=xValueToPixel(xValue(trace_[i],k),xs);
           y=yValueToPixel(y_value,ys);
           if (x==xLast&&y==yLast) continue;
           xLast=x; yLast=y;
           for (j=ct-1,y_value=0.;j>i;j--)
            {
              yy=trace_[j]->y(k);
              y_value+=sign<0?yy<0?yy:0:yy<0?0:yy;
            }
           yi=yValueToPixel(y_value,ys);
           rects[pointCount].x=x+bar_offset;
           rects[pointCount].y=sign<0?yi:y;
           rects[pointCount].width=(barWidth()>2)?barWidth()-1:1;
           rects[pointCount++].height=(i<ct-1)?abs(yi-y):abs(y0-y);
         }

        if (pointCount>=bufSize)
	 {
	   {
	     XSetForeground(display(),traceGC(),fillColor);
	     PFillRectangles(display(),graphPixmap(),traceGC(),rects,pointCount);
	     if (barWidth()>=5)
	      {
		XSetForeground(display(),traceGC(),lineColor);
		XDrawRectangles(display(),graphPixmap(),traceGC(),rects,pointCount);
	      }
	   }
	   pointCount=0;
	 }
      }
     XSetForeground(display(),traceGC(),fillColor);
     PFillRectangles(display(),graphPixmap(),traceGC(),rects,pointCount);
     if (barWidth()>=5)
      {
	XSetForeground(display(),traceGC(),lineColor);
	XDrawRectangles(display(),graphPixmap(),traceGC(),rects,pointCount);
      }
     pointCount=0;
     delete [] rects;
   }
  ct_=0;
}  

void MSGraph::plotTextTrace(MSTrace **trace_,int count_)
{
  for (int i=0;i<count_;i++)
   {
     XFontStruct *fontInfo=(XFontStruct*)server()->fontStruct(trace_[i]->traceSet()->textFont());
     XSetFont(display(),traceGC(),trace_[i]->traceSet()->textFont());
     int x=xValueToPixel(xValue(trace_[i],0),trace_[i]->xAxis());
     int y=yValueToPixel(yValue(trace_[i],0),trace_[i]->yAxis());
     MSStringVector aStringVector;
     MSString formatBuffer;
     int lines=trace_[i]->traceSet()->textLength();
     for (unsigned k=0;k<lines;k++) aStringVector<<trace_[i]->traceSet()->formatText(formatBuffer.removeAll(),k);
     if (trace_[i]==selectTrace())
      {
	int h=aStringVector.length()*(fontInfo->ascent+fontInfo->descent);
	int w=maxStringWidth(fontInfo,aStringVector);
	XSetForeground(display(),traceGC(),trace_[i]->traceSet()->textForeground());
	XFillRectangle(display(),graphPixmap(),traceGC(),x,y,w,h);
	XSetForeground(display(),traceGC(),MSWidget::background());
      }
     else
      {
	XSetForeground(display(),traceGC(),trace_[i]->traceSet()->textForeground());
      }
     y+=fontInfo->ascent;
     for (unsigned j=0;j<lines;j++)
      {
	XDrawString(display(),graphPixmap(),traceGC(),fontInfo,x,y,aStringVector(j),aStringVector(j).length());
	y+=fontInfo->ascent+fontInfo->descent;
      }
   }
}

void MSGraph::plotHStackTrace(MSTrace *trace_)
{
  if (trace_==0) return;
}

void MSGraph::plotBetweenTrace(MSTrace *trace_)
{
  if (trace_==0) return;
}



void MSGraph::buildSquareSymbol(void* array_,int& count_,int x_,int y_,int size_)
{ 
  XRectangle *rects=(XRectangle *)array_;
  int count=count_;
  rects[count].x=x_-(size_>>1);
  rects[count].y=y_-(size_>>1);
  rects[count].width=size_-1;
  rects[count++].height=size_-1;
  count_=count; 
}

void MSGraph::buildCircleSymbol(void* array_,int& count_,int x_,int y_,int size_)
{ 
  short angle1=0,angle2=23040;  // 360*64;
  XArc *arcs=(XArc *)array_;
  int count=count_;
  arcs[count].x=x_-(size_>>1);
  arcs[count].y=y_-(size_>>1);
  arcs[count].width=size_;
  arcs[count].height=size_;
  arcs[count].angle1=angle1;
  arcs[count++].angle2=angle2;
  count_=count; 
}

void MSGraph::buildStarSymbol(void* array_,int& count_,int x_,int y_,int size_)
{ 
  buildCrossSymbol(array_,count_,x_,y_,size_);
  buildXSymbol(array_,count_,x_,y_,size_); 
}

void MSGraph::buildCrossSymbol(void* array_,int& count_,int x_,int y_,int size_)
{ 
  XSegment *segments=(XSegment *)array_;
  int r=size_>>1;
  int count=count_;
  for (int j=0;j<2;j++) 
   {
     segments[count].x1=x_-(j==1?0:r);
     segments[count].y1=y_-(j==0?0:r);
     segments[count].x2=x_+(j==1?0:r);
     segments[count++].y2=y_+(j==0?0:r); 
   }
  count_=count; 
}

void MSGraph::buildXSymbol(void* array_,int& count_,int x_,int y_,int size_)
{ 
  XSegment *segments=(XSegment *)array_;
  int rc=(int)((size_>>1)*0.707)+1;
  int count=count_;
  for (int j=0;j<2;j++) 
   {
     segments[count].x1=x_-(j==0?rc:-rc);
     segments[count].y1=y_-rc;
     segments[count].x2=x_+(j==0?rc:-rc);
     segments[count++].y2=y_+rc; 
   }
  count_=count; 
}

void MSGraph::buildTriangleSymbol(void* array_,int& count_,int x_,int y_,int size_)
{ 
  XPoint *points=(XPoint *)array_;
  int size=size_-1;
  int r=size>>1;
  int s1=count_;
  int s2=s1+size*2-1;
  int s3=s1+size*3-1;
  for (int i=0;i<size_-1;i++)
   {	
     points[s1].x=x_-r+(int)((i+1)>>1);
     points[s1++].y=y_+r-i-1; 
     points[s2].x=x_+r-(int)(i>>1);
     points[s2--].y=y_+r-i;
     points[s3].x=x_-r+i;
     points[s3--].y=y_+r; 
   }
  count_+=size*3; 
}

void MSGraph::buildTrianglePrintSymbol(void* array_,int& count_,int x_,int y_,int size_)
{ 
  XSegment *segments=(XSegment *)array_;
  int r=(int)(size_>>1);
  int count=count_;
  for (int j=0;j<3;j++) 
   {
     segments[count].x1=x_-(j==2?-r:r);
     segments[count].y1=y_+r;
     segments[count].x2=x_+(j==1?r:0);
     segments[count++].y2=y_-(j==1?-r:r); 
   } 
  count_=count; 
}

void MSGraph::buildDiamondSymbol(void* array_,int& count_,int x_,int y_,int size_)
{ 
  XPoint *points=(XPoint *)array_;
  int size=(size_-1)>>1;
  int r=size>>1;
  int ct=count_;
  int s2=ct+(size<<1)-1;
  int s3=ct+(size<<1);
  int s4=ct+(size<<2)-1;
  for (int i=0;i<size;i++) 
{
    points[ct].x=x_-r+(int)((i+1)>>1);
    points[ct++].y=y_-i-1; 
    points[s2].x=x_+r-(int)(i>>1);
    points[s2--].y=y_-i; 
    points[s3].x=x_+r-(int)((i+1)>>1);
    points[s3++].y=y_+i+1;
    points[s4].x=x_-r+(int)(i>>1);
    points[s4--].y=y_+i; 
  } 
  count_+=size<<2; 
}

void MSGraph::buildDiamondPrintSymbol(void* array_,int& count_,int x_,int y_,int size_)
{ 
  XSegment *segments=(XSegment *)array_;
  int size=(size_-1)>>1;
  int r=size>>1;
  int count=count_;
  for (int j=0;j<4;j++) 
   {
    segments[count].x1=x_;
    segments[count].y1=y_-(j<2?size:-size);;
    segments[count].x2=x_-r;
    segments[count++].y2=y_; 
    r=-r;
  }
  count_=count; 
}

void MSGraph::buildCharSymbol(void* array_,int& count_,int x_,int y_,int size_)
{ 
  XPoint *points=(XPoint *)array_;
  int ct=count_;
  if (size_>0) { points[ct].x=x_; points[ct++].y=y_; }
  count_=ct; 
}

void MSGraph::setLineAttributes(unsigned long style_,int weight_,GC gc_,int lineWidth_,
			       int capStyle_,int joinStyle_)
{
  const static char  dotDash0[]={5,3,2,3};
  const static char  dotDash1[]={6,3,2,3};
  const static char  dotDash2[]={7,3,2,3};
  const static char  dotDash3[]={8,3,2,3};
  const static char  dotDash4[]={9,3,2,3};
  const static char  dash0[]   ={3,2};
  const static char  dash1[]   ={4,2};
  const static char  dash2[]   ={5,2};
  const static char  dash3[]   ={6,2};
  const static char  dash4[]   ={7,2};
  const static char  dot0[]    ={1,1};
  const static char  dot1[]    ={1,2};
  const static char  dot2[]    ={1,3};
  const static char  dot3[]    ={1,4};
  const static char  dot4[]    ={1,5};
  const static int   ct[]      ={2,2,2,2,2,2,2,2,2,2,4,4,4,4,4};
  static const char *styles[]  ={dash0,dash1,dash2,dash3,dash4,
				 dot0,dot1,dot2,dot3,dot4,
				 dotDash0,dotDash1,dotDash2,dotDash3,dotDash4};
  
  if (style_!=MSSolid)
   {
     int index=(style_&MSDot?style_&MSDash?10:5:0)+weight_;
     XSetLineAttributes(display(),gc_,lineWidth_,LineOnOffDash,capStyle_,joinStyle_);
     XSetDashes(display(),gc_,0,styles[index],ct[index]);
   }
  else XSetLineAttributes(display(),gc_,lineWidth_,LineSolid,capStyle_,joinStyle_);
}

void MSGraph::plotMarketProfile(MSTrace *trace_,int bufSize_)
{
  int 		i,k,xs,ys;
  int 		x,y,xOffset=0;
  int 		sessionCt=0,priceCt=0;
  double 	yInc,time,price,lastPrice=0;
  int		r,row,col,color,offset;
  int		period=0;
  int		length=0,maxWidth=0,height=0;
  XColor 	fgRGB;
  MSBoolean	split=MSFalse;
  unsigned long red,green,blue,priceColor;
  XPoint       *points=new XPoint[bufSize_];
  MSString     *priceString=new MSString[bufSize_];
  double       *priceIndex=new double[bufSize_];
  int	       *priceCount=new int[bufSize_];
  int          *width=new int[bufSize_];
  const char   *tempStr;
  static char 	buf[14];
  static const char *symbols[]={"A","B","C","D","E","F","G","H","I","J","K","L","M","N",
				"P","Q","R","S","T","V","W","X","Y","Z","a","b","c","d",
				"e","f","g","h","i","j","k","l","m","n","p","q","r","s",
				"t","v","w","x","y","z"};
  
  XFontStruct *fontInfo=(XFontStruct *)server()->fontStruct(trace_->traceSet()->textFont());
  height=fontInfo->ascent+fontInfo->descent;
  
  xs=trace_->xAxis(); ys=trace_->yAxis();
  int start=trace_->dataCount()-bufSize_-1;
  double startTime=xValue(trace_,start>0?start:0);
  if (yLabelOut(ys).formatType()==MSFormat::Money)
   {
     switch(yLabelOut(ys).format().moneyFormat())
      {
      case MSMoney::ThirtySeconds:
	yInc=3.125e-2;
	break;
      case MSMoney::SixtyForths:
	yInc=1.5625e-2;
	break;
      case MSMoney::OneTwentyEights:
	yInc=7.8125e-3;
	break;
//   case MSFormat::Price320:
//     yInc=3.125e-3;
//     break;
//   case MSFormat::Price328:
//     yInc=3.90625e-3;
//     break;
      }
   }
  else yInc=1.e-1;
    
  fgRGB.pixel=trace_->lineColor();
  XQueryColor(display(),server()->colormap(),&fgRGB);
  int redOffset=65535-fgRGB.red;
  int greenOffset=65535-fgRGB.green;
  int blueOffset=65535-fgRGB.blue;
  redOffset=redOffset>5000?redOffset:-(int)fgRGB.red;
  greenOffset=greenOffset>5000?greenOffset:-(int)fgRGB.green;
  blueOffset=blueOffset>5000?blueOffset:-(int)fgRGB.blue;
  setLineAttributes(trace_->lineStyle(),trace_->lineWeight(),traceGC(),trace_->lineWidth(),CapButt,JoinRound);
  height=(int)(yInc*yScale(ys));
//  if ((t=t0+xValue(trace_,0))>sessionCt*sessionPeriod())
//  for (k=0;k<trace_->dataCount();k++)
//   {
//     if ((time=trace_->x(k)-startTime-tpoOpen()-(sessionCt*sessionPeriod()))>=0)
//      {
//	cout<<"period\t"<<sessionCt++<<"\tindex\t";
//	cout<<k<<"\tvalue\t"<<trace_->x(k)<<"\ttime\t"<<time<<endl; 
//      }
//   } 
//  sessionCt=0;
  for (k=0;k<trace_->dataCount();k++)
   {
     if ((time=trace_->x(k)-startTime-tpoOpen()-(sessionCt*sessionPeriod()))>=0)
      {
	period=(int)(time/tpoPeriod());
	if (time>(tpoClose()-tpoOpen())||
	    (period>0&&period==tpoBreakChar()&&split==MSFalse)||period>=48)
	 {
	   for (row=0;row<priceCt;row++)
	    {
	      if (trace_->style()==ColorProfile)
	       {
		 for (col=0;col<priceString[row].length();col++)
		  {
		    if ((tempStr=priceString[row].string())!=0)
		     {
		       for (color=0;color<period;color++) 
			{
			  if (strncmp(&tempStr[col],symbols[color],1)==0) break;
			} 
		     }
		    red=fgRGB.red+color*redOffset/period;
		    green=fgRGB.green+color*greenOffset/period;
		    blue=fgRGB.blue+color*blueOffset/period;
		    sprintf(buf,"#%04X%04X%04X",red,green,blue);
		    priceColor=server()->pixel(buf);
		    XSetForeground(display(),traceGC(),priceColor);
		    offset=col*maxWidth/length-maxWidth/length/2;
		    PFillRectangle(display(),graphPixmap(),traceGC(),points[row].x+offset,
				   points[row].y-height/2,maxWidth/length,height);
		  }
	       }
	      else
	       {
		 XSetForeground(display(),traceGC(),trace_->lineColor());
		 sprintf(buf,"%3d ",priceCount[row]);
		 priceString[row].insert(0,buf);
		 XDrawString(display(),graphPixmap(),traceGC(),fontInfo,points[row].x,points[row].y,
			     priceString[row].string(),priceString[row].length());
	       }
	      priceString[row]="";
	    }
	   xOffset=(int)(1.1*maxWidth);
	   priceCt=maxWidth=0;
	   if (time>(tpoClose()-tpoOpen()))
	    {
	      width[++sessionCt]=0;
	      split=MSFalse;
	      xOffset=0;
	      continue;
	    }
	   else split=MSTrue;
	 }
	price=trace_->y(k);
	lastPrice=priceCt==0?price-yInc:lastPrice;
	int priceOffset=(int)((price-lastPrice)/yInc);
	int sign=priceOffset>0?1:-1;
	for (int j=0;j<priceOffset*sign;j++)
	 {
	   for (r=0;r<priceCt;r++) 
	    {
	      if (priceIndex[r]>(price-yInc)&&priceIndex[r]<(price+yInc)) break;
	    }
	   if (r==priceCt)
	    {
	      x=xValueToPixel(startTime+tpoOpen()+sessionCt*sessionPeriod(),xs);
	      y=yValueToPixel(price,ys);
	      priceCount[priceCt]=0;
	      priceIndex[priceCt]=price;
	      points[priceCt].x=x+xOffset;
	      points[priceCt++].y=y;
	    }
	   if (j==0) priceCount[r]++;
	   tempStr=priceString[r].string();
	   if (priceString[r].length()==0||
	       strcmp(&tempStr[priceString[r].length()-1],symbols[period])!=0)
	    {
	      priceString[r]<<symbols[period];
	      length=priceString[r].length()>length?priceString[r].length():length;
	      int w=XTextWidth(fontInfo,priceString[r].string(),priceString[r].length());
	      width[sessionCt]=width[sessionCt]>w?width[sessionCt]:w;
	      maxWidth=maxWidth>w?maxWidth:w;
	    }
	   price -= yInc*sign;
	 }
	lastPrice=trace_->y(k);
      }
   }
  for (i=0;i<priceCt;i++)
   {
     XDrawString(display(),graphPixmap(),traceGC(),fontInfo,points[i].x,points[i].y,
		 priceString[i].string(),priceString[i].length());
   }
  delete [] points;
  delete [] priceIndex;
  delete [] priceCount;
  delete [] priceString;
  delete [] width;
}

void MSGraph::tpoBreakChar(int key_) 
{ 
  if (key_>=65&&key_!=79&&key_!=85&&key_!=111&&key_!=117&&key_<122)
   { 
     int keyOffset=key_>117?4:key_>111?3:key_>85?2:key_>79?1:0;
     key_ -= 65+keyOffset;
     if (_tpoBreakChar!=key_) 
      { 
	_tpoBreakChar=key_; 
	redrawImmediately(); 
     } 
   } 
}

//////////////////////////////////////////////////////////////////////////////////////////
//                                Pie drawing functions
//////////////////////////////////////////////////////////////////////////////////////////


#if !defined (MS_HAS_M_2PI)                       
static const double M_2PI=2*M_PI;
#endif
static const double M_3PI = 3*M_PI,M_4PI = 4*M_PI;
static const double DegPerRadian = M_2PI/(64.0*360.0);
static const double circle = 23040;           // 360*64;
static const int    semi = int(circle/2);    // 180
static const int    semi_1 = int(semi*0.9);  // 162
static const int    semi_2 = int(semi/2);    //  90
static const int    semi_4 = int(semi/4);    //  45
static const int    semi_5 = int(semi/5);    //  36
static const int    lineThreshold = 100;


/**********************************************************************************************/
// MSGraphPieData is a fully public structure that contain data needed to draw pie chart
// angle1 - pie slices starting angles
// angle2 - angle of the pie slice
// radian1, radian2 - radian equivalent of angle1 and angle2
// xo, yo   - 
// cos1, sin1 - precomputed cos and sin for angle1 
// cos2, sin2
// cosB, sinB - precomputed cos and sin for bisectors
// sequence   - sequence in which pie slices will be drawn.   
/**********************************************************************************************/

class MSGraphPieData
{
public:
  MSGraphPieData(unsigned);
  ~MSGraphPieData();
  void clear(unsigned);
  
// public data
  MSIntVector angle1,angle2;
  MSStringVector percents;
  MSFloatVector xo,yo;
  MSFloatVector radian1,radian2;
  MSFloatVector offsets,profiles;
  MSUnsignedLongVector lineColors,fillColors;
  MSFloatVector bisectors;
  MSUnsignedVector sequence;
  double *cosB, *sinB;
  double *cos1,*cos2,*sin1,*sin2;
  int bufSize;

private:
  void allocate(void); 
  void deallocate(void);
};

/**********************************************************************************************/
// MSGraphPieLabelData is a fully public structure that contain data needed to draw pie labels
// 
// iRects -
// oRects - 
// iText  - 
// oText  -
/**********************************************************************************************/

class MSGraphPieLabelData
{
public:
  MSGraphPieLabelData(unsigned);
  ~MSGraphPieLabelData();
  void clear(unsigned);

  XRectangle *iRects;
  XRectangle *oRects;
  MSStringVector *iText;
  MSStringVector *oText;
  int bufSize;

private:
  void allocate();
  void deallocate();
};

/**********************************************************************************************/
// Pie plot main routine
/**********************************************************************************************/

void MSGraph::plotPieTrace(MSTrace *trace_)
{
  static int    labelThreshold=100;

  MSTraceSet* ts=trace_->traceSet();
  int bufSize=trace_->dataCount();
  bufSize=bufSize>MSGraph::_MaxBufSize?MSGraph::_MaxBufSize:bufSize;
  int d=plotAreaRect()->width()<plotAreaRect()->height()?
    plotAreaRect()->width():plotAreaRect()->height();
  int w=(int)(d*(1.0-pieOffsetMargin()));
  int h=(int)ts->pieAspectRatio()*w;
  w=w%2>0?w+1:w;
  h=h%2>0?h+1:h;
  ts->resize(w,h);

  if (w>0)
   {
     XSetFont(display(),traceGC(),ts->textFont());
     int lineWidth=trace_->lineWidth()>0&&w>lineThreshold*2?trace_->lineWidth():0;
     setLineAttributes(trace_->lineStyle(),trace_->lineWeight(),traceGC(),lineWidth,CapRound,JoinRound);
     XSetArcMode(display(),traceGC(),ArcPieSlice);
     _pieDepth=(int)((w-h)/(5.0/ts->pieDepthFactor()));
     _pieRx=w/2;
     _pieRy=h/2;
     _pieX=x_org()+(plotAreaRect()->width()-w)/2;
     _pieY=y_end()+(plotAreaRect()->height()-h-_pieDepth)/2;
     
     // create pie data
     if (_pieData==0)  _pieData=new MSGraphPieData(bufSize);
     else _pieData->clear(bufSize);
     computePieData(trace_,_pieData);

     // create pie label data
     if (_pieLabelData==0)  _pieLabelData=new MSGraphPieLabelData(bufSize);
     else _pieLabelData->clear(bufSize);
     if (w>labelThreshold) computePieLabels(trace_,_pieData,_pieLabelData); 
     // calculate pie drawing sequence
     computeDrawingSequence(_pieData);

     // draw pie and slices
     drawPieSlices(trace_,_pieData,w,h);
     if (w>labelThreshold) drawLabels(trace_,_pieLabelData);
   }
}

/**********************************************************************************************/
// computePieData - precomputes data needed to draw pie slices
/**********************************************************************************************/

void MSGraph::computePieData(MSTrace* trace_,MSGraphPieData* pieData_)
{
  MSFloatVector& radian1=pieData_->radian1;
  MSFloatVector& radian2=pieData_->radian2;
  MSFloatVector& xo=pieData_->xo;
  MSFloatVector& yo=pieData_->yo;
  MSIntVector& angle2=pieData_->angle2;
  MSStringVector& percents=pieData_->percents;
  MSFloatVector& offsets=pieData_->offsets;
  MSFloatVector& profiles=pieData_->profiles;
  MSFloatVector& bisectors=pieData_->bisectors;
  MSUnsignedLongVector& lineColors=pieData_->lineColors;
  MSUnsignedLongVector& fillColors=pieData_->fillColors;
  double* cosB=pieData_->cosB;
  double* sinB=pieData_->sinB;
  double totalSlice=0,total=0;
  int angle=0,i;
  int bufSize=pieData_->bufSize;
  
  MSTraceSet   *ts=trace_->traceSet();

  for (i=0;i<bufSize;i++) total+=trace_->y(i);
  for (i=0;i<bufSize;i++)
   {
     double slice;
     if (total) slice=trace_->y(i)/total;
     totalSlice+=slice;
     radian1 << (totalSlice*M_2PI);
     angle2 << int(slice*circle);
     radian2 << angle2(i)*DegPerRadian;
     angle = angle>angle2(i)?angle:angle2(i);
     lineColors<<trace_->lineColor(i,trace_->virtualCol());
     fillColors<<trace_->fillColor(i,trace_->virtualCol());
     profiles<<ts->pieProfile(i);
     offsets<<ts->pieOffset(i);
   }

  if (ts->piePercentAlignment()>MSNone)
   {
     MSFormat format(ts->piePercentFormat());
     MSString formatBuffer;
     double sum=0;
     for (i=0;i<bufSize;i++)
      {
        double slice;
        if (total) slice=100.0*trace_->y(i)/total;
        MSFloat aFloat(slice);
        percents<<aFloat.format(formatBuffer.removeAll(),format);
        sum+=percents(i).asDouble();
      }
     double diff=100.0-sum;
     if (diff>0.01)
      {
        unsigned index=angle2.indexOf(angle);
        double slice;
        if (total) slice=100.0*trace_->y(index)/total;
        MSFloat aFloat(slice+diff);
        aFloat.format(formatBuffer.removeAll(),format);
        percents[index]=formatBuffer;
      }
   }

  // determine if drawing order is relevent
  if (ts->pieAspectRatio()<1.0)
   {
     for (i=0;i<bufSize;i++) if (profiles(i)<1.0||offsets(i)>0.0) _orderDependent=MSTrue;
   }

  // find index of largest angle
  if (_orderDependent==MSTrue)
   {
     for (i=0;i<bufSize;i++) if (angle2(i)>=semi) _indexOfMaxAngle=i;
   }
  
  // orient primary slice if specified, otherwise orient largest angle
  // to primarySliceAlignment
  if (ts->primarySliceAlignment()!=MSNone)
   {
     unsigned index=ts->primarySlice()>=0&&ts->primarySlice()<bufSize?
       ts->primarySlice():angle2.indexOf(angle);
     unsigned long align=ts->primarySliceAlignment();
     for (i=0,angle=0;i<index;i++) angle+=angle2(i);
     _startAngle=(int)(circle-angle2(index))/2-angle;
     _startAngle-=align&MSTop?semi_2:align&MSBottom?-semi_2:align&MSRight?semi:0;
   }
  else _startAngle=ts->pieAngle()*64;
  _startAngle=(int)(_startAngle<0?circle+_startAngle:_startAngle);

  double startRadian=(double)_startAngle*DegPerRadian;
  radian1+=startRadian;

  
  // compute pie offsets
  for (i=0;i<bufSize;i++)
   {
     double bisector=radian1(i)-radian2(i)/2.0;
     bisectors<<bisector;
     cosB[i]=cos(bisector);
     sinB[i]=sin(bisector);
     xo<<(cosB[i]*_pieRx*offsets(i));
     yo<<(sinB[i]*_pieRy*offsets(i));
   }

  // pre-compute sines and cosines for all angles
  for (i=0;i<bufSize;i++)
   {
     pieData_->sin1[i]=sin(radian1(i));
     pieData_->sin2[i]=sin(radian1(i)-radian2(i));
     pieData_->cos1[i]=cos(radian1(i));
     pieData_->cos2[i]=cos(radian1(i)-radian2(i));
   }

}

/**********************************************************************************************/
// computePiLabels - precomputes data needed to draw pie labels
/**********************************************************************************************/
void MSGraph::computePieLabels(MSTrace* trace_,const MSGraphPieData* pieData_,
                               MSGraphPieLabelData* pieLabelData_)
{ 
  XFontStruct *fontInfo=(XFontStruct*)server()->fontStruct(trace_->traceSet()->textFont());

  double* cosB=pieData_->cosB;
  double* sinB=pieData_->sinB;
  double *cos1=pieData_->cos1;
  double *cos2=pieData_->cos2;
  double *sin1=pieData_->sin1;
  double *sin2=pieData_->sin2;
  const MSFloatVector& xo=pieData_->xo;
  const MSFloatVector& yo=pieData_->yo;
  const MSIntVector& angle2=pieData_->angle2;
  const MSFloatVector& bisectors=pieData_->bisectors;
  const MSStringVector& percents=pieData_->percents;
  const MSFloatVector& profiles=pieData_->profiles;
  int bufSize=trace_->dataCount();  
  int x=_pieX;
  int y=_pieY;
  int w=(int)_pieRx*2;

  
  // pointers to needed data from MSGraphPieLabelData
  XRectangle *    iRects= pieLabelData_->iRects;
  XRectangle*     oRects= pieLabelData_->oRects;
  MSStringVector* iText= pieLabelData_->iText;
  MSStringVector* oText= pieLabelData_->oText;

  XPoint *opoints=new XPoint[bufSize];
  int yOffset;
  unsigned i=0;
  
// compute label positions
  unsigned labelOffset=5;

  static const double sin_30=0.5;
  static const double sin_60=0.8660254;
  MSIntVector sectors;

  // sectors for distributing labels if default postions overlap
  //
  //     3     2      1
  //      \    |    /
  //       \   |   /
  //        \  |  /
  //         \ | /
  // 4 _______\|/________ 0
  //          /|\
  //         / | \
  //        /  |  \
  //       /   |   \
  //      /    |    \
  //     5     6     7
  //

  // compute label positions
  for (i=0;i<bufSize;i++)
   { // lab
     // assign angles to a sector for label placement
     if (sinB[i]>-sin_30&&sinB[i]<sin_30&cosB[i]>0)       sectors<<0;
     else if (sinB[i]>sin_30&&sinB[i]<sin_60&cosB[i]>0)   sectors<<1;
     else if (sinB[i]>sin_60)                             sectors<<2;
     else if (sinB[i]>sin_30&&sinB[i]<sin_60&cosB[i]<0)   sectors<<3;
     else if (sinB[i]>-sin_30&&sinB[i]<sin_30&cosB[i]<0)  sectors<<4;
     else if (sinB[i]<-sin_30&&sinB[i]>-sin_60&cosB[i]<0) sectors<<5;
     else if (sinB[i]<-sin_60)                            sectors<<6;
     else if (sinB[i]>-sin_60&&sinB[i]<-sin_30&cosB[i]>0) sectors<<7;

     yOffset=(int)(_pieDepth-_pieDepth*profiles(i));
     double dia=0;
     double x1=x+xo(i)+_pieRx;
     double y1=y-yo(i)+_pieRy+yOffset;
     double xin,yin,x2,y2,x3,y3,a,b,c,perimeter,xcg,ycg,sp,radius;
     iRects[i].x=oRects[i].x=(int)(x1+xo(i)+cosB[i]*_pieRx);
     iRects[i].y=oRects[i].y=(int)(y1-yo(i)-sinB[i]*_pieRy);
     double *xcc=new double[2];
     double *ycc=new double[2];
     for (int p=0;p<2;p++)
      {
        double factor=p==0?angle2(i)>semi_4?2.0:1.5:1;
        x2=x1+cos1[i]*_pieRx*factor;
        y2=y1-sin1[i]*_pieRy*factor;
        x3=x1+cos2[i]*_pieRx*factor;
        y3=y1-sin2[i]*_pieRy*factor;
        if (p==1&&angle2(i)>semi_1)  // >162
         {
           x1=x1+cosB[i]*_pieRx;
           y1=y1-sinB[i]*_pieRy;
         }
        a=sqrt((x3-x2)*(x3-x2)+(y3-y2)*(y3-y2));
        b=sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1));
        c=sqrt((x3-x1)*(x3-x1)+(y3-y1)*(y3-y1));
        perimeter=a+b+c;
        sp=perimeter/2.0;
        radius=sqrt(((sp-a)*(sp-b)*(sp-c))/sp)*0.95;
        xcg=(x1+x2+x3)/3.0;
        ycg=(y1+y2+y3)/3.0;
        xcc[p]=(a*x1+c*x2+b*x3)/perimeter;
        ycc[p]=(a*y1+c*y2+b*y3)/perimeter;
      }
     xin=xcc[1];
     yin=ycc[1];
     opoints[i].x=(int)xcc[0];
     opoints[i].y=(int)(ycc[0]+(sinB[i]<0?_pieDepth+radius:0));
     delete [] xcc;
     delete [] ycc;
     
     if (angle2(i)>=semi_2&&angle2(i)<=semi_1)  // >90 and <150
      {
        x1=x1+cos(bisectors(i))*_pieRx;
        y1=y1-sin(bisectors(i))*_pieRy;
        b=sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1));
        c=sqrt((x3-x1)*(x3-x1)+(y3-y1)*(y3-y1));
        perimeter=a+b+c;
        sp=perimeter/2.0;
        double radius1=sqrt(((sp-a)*(sp-b)*(sp-c))/sp)*0.95;
        double xin1=(a*x1+c*x2+b*x3)/perimeter;
        double yin1=(a*y1+c*y2+b*y3)/perimeter;
        double radius2=radius+radius1*0.85;
//      XSetForeground(display(),traceGC(),server()->pixel("black"));
//      XDrawArc(display(),graphPixmap(),traceGC(),xin1-radius1,yin1-radius1,2*radius1,2*radius1,0,circle);
        double xin2=(xin+xin1)/2;
        double yin2=(yin+yin1)/2;
//      XSetForeground(display(),traceGC(),server()->pixel("blue"));
//      XDrawArc(display(),graphPixmap(),traceGC(),xin2-radius2,yin2-radius2,2*radius2,2*radius2,0,circle);
        xin=xin2;
        yin=yin2;
        radius=radius2;
//      XSetForeground(display(),traceGC(),server()->pixel("red"));
//      XDrawLine(display(),graphPixmap(),traceGC(),x1,y1,x2,y2);
//      XDrawLine(display(),graphPixmap(),traceGC(),x1,y1,x3,y3);
//      XDrawLine(display(),graphPixmap(),traceGC(),x2,y2,x3,y3);
      }
     else if (angle2(i)>semi_1&&angle2(i)<semi) // >162 and <180
      {
//      XSetForeground(display(),traceGC(),server()->pixel("black"));
//      XDrawArc(display(),graphPixmap(),traceGC(),xin-radius,yin-radius,2*radius,2*radius,0,circle);
      }
     else if (angle2(i)>semi)  // >180
      {
        x1=x+xo(i)+_pieRx;
        y1=y-yo(i)+_pieRy+yOffset;
        xin=x1+cos(bisectors(i))*_pieRx*0.5;
        yin=y1-sin(bisectors(i))*_pieRy*0.5;
/*        double xx1=abs(x1-x2);
        double yy1=abs(y1-y2);
        double xx2=abs(x1-x3);
        double yy2=abs(y1-y3);
*/  
    }
     else if (angle2(i)>semi_5) // >36 
      {
        xin=(xin+xcg)/2.0;
        yin=(yin+ycg)/2.0;
      }
//      XSetForeground(display(),traceGC(),server()->pixel("black"));
//      XDrawLine(display(),graphPixmap(),traceGC(),x1,y1,x2,y2);
//      XDrawLine(display(),graphPixmap(),traceGC(),x1,y1,x3,y3);
//      XDrawLine(display(),graphPixmap(),traceGC(),x2,y2,x3,y3);
//      XSetForeground(display(),traceGC(),server()->pixel("red"));
//      XDrawArc(display(),graphPixmap(),traceGC(),xin-radius,yin-radius,2*radius,2*radius,0,circle);
     dia=2*radius;
     alignPieLabels(trace_,iText[i],oText[i],percents,i);
     int len=iText[i].length();
     if (len>0)
      {
        iRects[i].height=len*(fontInfo->ascent+fontInfo->descent);
        iRects[i].width=maxStringWidth(fontInfo,iText[i]);
        if (iRects[i].height<dia&&iRects[i].width<dia)
         {
           iRects[i].x=(int)xin-iRects[i].width/2;
           iRects[i].y=(int)yin-iRects[i].height/2;
         }
        else
         {
           iRects[i].width=0;
           iRects[i].height=0;
           iText[i].removeAll();
           oText[i].removeAll();
           alignPieLabels(trace_,oText[i],oText[i],percents,i);
         }
      }
     len=oText[i].length();
     if (len>0)
      {
        oRects[i].height=len*(fontInfo->ascent+fontInfo->descent);
        oRects[i].width=maxStringWidth(fontInfo,oText[i]);
        // adjust y position
        if (sectors(i)>0&&sectors(i)<4) oRects[i].y-=oRects[i].height+labelOffset;
        else if (sectors(i)==0||sectors(i)==4) oRects[i].y-=oRects[i].height/2;
        else if (sectors(i)>4) oRects[i].y+=_pieDepth+labelOffset;
        // adjust x position
        if (sectors(i)==0) oRects[i].x=(int)(x+w+xo(i)+labelOffset);
        else if (sectors(i)==4) oRects[i].x=(int)(x+xo(i)-oRects[i].width-labelOffset);
        else if (sectors(i)==2||sectors(i)==6) oRects[i].x-=oRects[i].width/2;
        else if (cosB[i]<0) oRects[i].x-=oRects[i].width+labelOffset;
      }
     
   }
//          cout<<sectors<<endl;
//          for (i=0;i<bufSize;i++)
//           {
//             cout<<i<<"\t"<<oRects[i].x<<"\t"<<oRects[i].y<<"\t";
//             cout<<oRects[i].x+oRects[i].width<<"\t"<<oRects[i].y+oRects[i].height<<"\t"<<endl;
//           }
//          cout<<endl;
  // check for label overlap
  int passes;
  for (i=0, passes=0;i<bufSize&&passes<5;i++)
   {
     unsigned j=(i+1)%bufSize;
     int xxi=oRects[i].x+oRects[i].width;
     int yyi=oRects[i].y+oRects[i].height;
     int xxj=oRects[j].x+oRects[j].width;
     int yyj=oRects[j].y+oRects[j].height;
     int xi=oRects[i].x;
     int yi=oRects[i].y;
     int xj=oRects[j].x;
     int yj=oRects[j].y;
           
     if (((xj>=xi&&xj<xxi)&&((yj>yi&&yj<yyi)||(yi>yj&&yi<yyj)))||
         ((xi>=xj&&xi<xxj)&&((yi>yj&&yi<yyj)||(yj>yi&&yj<yyi))))
      {
        if (sectors(i)==sectors(j))
         {
           unsigned k,m,n;
           MSBinaryVector v1=sectors.binaryCompare(sectors(i),MSEqualTo);
           MSIndexVector iv;
           int heights=0,widths=0;
           for (k=0;k<bufSize;k++) if (v1(k)>0)
            {
              widths+=oRects[i].width+labelOffset;
              heights+=oRects[i].height+labelOffset;
              iv<<k;
            }
           int l=iv.length();
           if (sectors(i)==2||sectors(i)==6)
            {
              // sort by x-position
              for (m=0;m<l;m++)
               {
                 int min=m;
                 for (n=m+1;n<l;n++) if (oRects[iv(n)].x<oRects[iv(min)].x) min=n;
                 iv.exchange(min,m);
               }
              int xlast=0,xstart=x+(w-widths)/2;
              for (m=0;m<l;m++)
               {
                 oRects[iv(m)].x=xstart+xlast;
                 xlast+=oRects[iv(m)].width+labelOffset;
               }
            }
           else
            {
              // sort by y-position
              for (m=0;m<l;m++)
               {
                 int min=m;
                 for (n=m+1;n<l;n++) if (oRects[iv(n)].y>oRects[iv(min)].y) min=n;
                 iv.exchange(min,m);
               }
            }
           if (passes==0) for (m=0;m<l;m++)
            {
              int i=iv(m);
              if (sinB[i]>0||sectors(i)==1||sectors(i)==3)
               {
                 oRects[i].y=opoints[i].y-oRects[i].height;
               }
              else if (sinB[i]<0||sectors(i)==5||sectors(i)==7) oRects[i].y=opoints[i].y;
              else if (sectors(i)==0) oRects[i].x=opoints[i].x;
              else if (sectors(i)==4) oRects[i].x=opoints[i].x-oRects[i].width;
            }
           passes++;
           i=0;
         }
        else
         {
         }
      }
   }
  delete [] opoints;
}

/**********************************************************************************************/
// computeDrawingSequence - calculate drawging sequence
/**********************************************************************************************/
void MSGraph::computeDrawingSequence(MSGraphPieData* pieData_)
{
  int& bufSize=pieData_->bufSize;
  MSFloatVector& radian1=pieData_->radian1;
  MSFloatVector& radian2=pieData_->radian2;
  MSFloatVector& xo=pieData_->xo;
  MSFloatVector& yo=pieData_->yo;
  MSIntVector& angle2=pieData_->angle2;
  MSIntVector& angle1=pieData_->angle1;
  MSFloatVector& offsets=pieData_->offsets;
  MSFloatVector& profiles=pieData_->profiles;
  MSUnsignedLongVector& lineColors=pieData_->lineColors;
  MSUnsignedLongVector& fillColors=pieData_->fillColors;
  MSUnsignedVector& sequence=pieData_->sequence;
  
  int i=0,j=0,angle=0;
  
// split pieces with an angle greater than pi into 2
  if (_orderDependent==MSTrue&&_indexOfMaxAngle<bufSize)
   {
     bufSize++;
     unsigned index=_indexOfMaxAngle;
     radian2.insertAt(index,radian2(index)/2);
     radian2[index+1]=radian2(index);
     radian1.insertAt(index,radian1(index)-radian2(index));
     angle1.insertAt(index+1,angle2(index)/2);
     angle2.insertAt(index,angle2(index)/2);
     angle2[index+1]=angle2(index);
     xo.insertAt(index,xo(index));
     yo.insertAt(index,yo(index));
     lineColors.insertAt(index,lineColors(index));
     fillColors.insertAt(index,fillColors(index));
     profiles.insertAt(index,profiles(index));
     offsets.insertAt(index,offsets(index));

     // recompute cos and sin for angles
     delete [] pieData_->cos1;
     delete [] pieData_->cos2;
     delete [] pieData_->sin1;
     delete [] pieData_->sin2;
     pieData_->cos1=new double[bufSize];
     pieData_->cos2=new double[bufSize];
     pieData_->sin1=new double[bufSize];
     pieData_->sin2=new double[bufSize];
     for (i=0;i<bufSize;i++)
      {
        pieData_->sin1[i]=sin(radian1(i));
        pieData_->sin2[i]=sin(radian1(i)-radian2(i));
        pieData_->cos1[i]=cos(radian1(i));
        pieData_->cos2[i]=cos(radian1(i)-radian2(i));
      }
   }

  for (i=0,angle=_startAngle;i<bufSize;i++)
   {
     angle1<<angle;
     angle+=angle2(i);
     angle%=(int)circle;
   }

     sequence.series(bufSize);
     MSFloatVector exposed1,exposed2;
     if (_orderDependent==MSTrue)
      {
        for (i=0;i<bufSize;i++)
         {
           if (profiles(i)<1.0||offsets(i)>0.0||
               profiles((i+1)%bufSize)!=profiles(i)||
               profiles((i!=0?i:bufSize)-1)!=profiles(i)||
               offsets((i+1)%bufSize)!=offsets(i)||
               offsets((i!=0?i:bufSize)-1)!=offsets(i)||
               (i==_indexOfMaxAngle&&(profiles((i+2)%bufSize)!=profiles(i)||
                                     offsets((i+2)%bufSize)!=offsets(i))))
            {
              if ((pieData_->cos1[i]<0&&pieData_->cos2[i]>0)||(pieData_->cos1[i]>0&&pieData_->cos2[i]<0))
               {
                 exposed1<<(((pieData_->sin1[i]+pieData_->sin2[i])/2)<0?-1.0:1.0);
                 exposed2<<(((pieData_->sin1[i]+pieData_->sin2[i])/2)<0?-1.0:1.0);
               }
              else
               {
                 exposed1<<pieData_->sin1[i];
                 exposed2<<pieData_->sin2[i];
               }
            }
           else
            {
              exposed1<<-10.0;
              exposed2<<-10.0;
            }
         }
        for (i=0;i<bufSize;i++)
         {
           unsigned m=i;
           for (j=i+1;j<bufSize;j++)
            {
              if (exposed2(sequence(j))>exposed2(sequence(m))&&
                  exposed1(sequence(j))>exposed1(sequence(m))) m=j;
            }
           sequence.exchange(m,i);
         }
      }
}

/**********************************************************************************************/
// drawPieSlices 
/**********************************************************************************************/
void MSGraph::drawPieSlices(MSTrace * trace_,const MSGraphPieData* pieData_,int w_, int h_)

{
  // take references to PieData, no copying is performed
  const MSFloatVector& radian1=pieData_->radian1;
  const MSFloatVector& radian2=pieData_->radian2;
  const MSFloatVector& xo=pieData_->xo;
  const MSFloatVector& yo=pieData_->yo;
  const MSIntVector& angle2=pieData_->angle2;
  const MSIntVector& angle1=pieData_->angle1;
  const MSFloatVector& offsets=pieData_->offsets;
  const MSFloatVector& profiles=pieData_->profiles;
  const MSUnsignedLongVector& lineColors=pieData_->lineColors;
  const MSUnsignedLongVector& fillColors=pieData_->fillColors;
  const MSUnsignedVector& sequence=pieData_->sequence;

  double *cos1=pieData_->cos1;
  double *cos2=pieData_->cos2;
  double *sin1=pieData_->sin1;
  double *sin2=pieData_->sin2;

  MSTraceSet   *ts=trace_->traceSet();  
  unsigned bufSize=sequence.length();
  unsigned i,j,k;
  
// draw slices one at a time
  for (unsigned s=0;s<bufSize;s++)
   {
     i=sequence(s);
     // draw side panels of the pie only if angle is > PI and < 2PI
     if (ts->pieAspectRatio()<1.0&&profiles(i)>0.0)
      {
        int maxPixels=2;
        double radian=radian1(i)-radian2(i);
        double angleInc=_pieRy>0?(2*asin(maxPixels/_pieRy)):1; 
        if ((angle1(i)<=semi&&angle1(i)+angle2(i)>semi)||(angle1(i)>=semi&&angle1(i)<=circle))
         {
           unsigned passes=1;
           double rad=radian1(i)>M_4PI?M_4PI:radian1(i)<M_3PI&&radian1(i)>M_2PI?M_2PI:radian1(i);
           if (angle2(i)>semi&&angle1(i)+angle2(i)-circle>semi) 
            {
              rad=radian<M_2PI?M_2PI:M_4PI;
              passes=2;
            }
           else radian=radian>M_2PI&&radian<M_3PI?M_3PI:radian<M_PI?M_PI:radian;
           XSetForeground(display(),traceGC(),fillColors(i));
           for (unsigned pass=0;pass<passes;pass++)
            {
              if (pass>0)
               {
                 radian=radian1(i)>M_3PI?M_3PI:M_PI;
                 rad=radian1(i);
               }
              int pointCount=(int)rint(fabs(rad-radian)/angleInc)+2;
              XPoint *points=new XPoint[pointCount*2];
              int yOffset=(int)(_pieDepth-_pieDepth*profiles(i));
              for (j=0;j<pointCount;j++,radian+=angleInc)
               {
                 if (radian>rad) radian=rad;
                 points[j].x=(int)(_pieX+xo(i)+_pieRx+rint(cos(radian)*_pieRx));
                 points[j].y=(int)(_pieY-yo(i)+_pieRy-rint(sin(radian)*_pieRy)-1+yOffset);
               }
              for (k=j,j--;j<pointCount;j--,k++)
               {
                 points[k].x=points[j].x;
                 points[k].y=points[j].y+_pieDepth+1-yOffset;
               }
              PFillPolygon(display(),graphPixmap(),traceGC(),points,
                           pointCount*2,Complex,CoordModeOrigin);
              delete [] points;
            }
         }
        // draw arcs for shadow only between PI and 2PI
        if (profiles(i)>0.0&&(angle1(i)<=semi&&angle1(i)+angle2(i)>semi)||
            (angle1(i)>=semi&&angle1(i)<circle))
         {
           int angle_1=(angle1(i)<semi)?semi:angle1(i);
           int angle_2=(int)((angle1(i)+angle2(i)>circle)?circle-angle1(i):angle2(i));
           int offset=angle_1-angle1(i);
           XSetForeground(display(),traceGC(),lineColors(i));
           XDrawArc(display(),graphPixmap(),traceGC(),(int)(_pieX+xo(i)),
		    (int)(_pieY-yo(i)+_pieDepth),w_,h_,angle_1,angle_2-offset);
           if (angle2(i)>semi&&angle1(i)+angle2(i)-circle>semi)
            {
              angle_1=semi;
              angle_2=(int)(angle1(i)+angle2(i)-circle-semi);
              XDrawArc(display(),graphPixmap(),traceGC(),(int)(_pieX+xo(i)),
		       (int)(_pieY-yo(i)+_pieDepth),w_,h_,angle_1,angle_2);
            }
         }
      }
     // draw pie slices
     double yOffset=_pieDepth-_pieDepth*profiles(i);
     XSetForeground(display(),traceGC(),fillColors(i));
     XFillArc(display(),graphPixmap(),traceGC(),(int)(_pieX+xo(i)),
	      (int)(_pieY-yo(i)+yOffset),w_,h_,angle1(i),angle2(i));
     XSetForeground(display(),traceGC(),lineColors(i));
     XDrawArc(display(),graphPixmap(),traceGC(),(int)(_pieX+xo(i)),
	      (int)(_pieY-yo(i)+yOffset),w_,h_,angle1(i),angle2(i));
     for (unsigned p=0;p<2;p++)
      {
        double cose=p==0?cos1[i]:cos2[i];
        double sine=p==0?sin1[i]:sin2[i];
        double yy=sine*_pieRy;
        double xx=cose*_pieRx;
        int x1=(int)(_pieX+xo(i)+_pieRx),y1=(int)(_pieY-yo(i)+_pieRy+yOffset);
        int x2=(int)(x1+xx),y2=(int)(y1-yy);
        XSetForeground(display(),traceGC(),lineColors(i));
        if (ts->pieAspectRatio()<1.0)
         {
           // fill in sides of pie if exposed
           if ((i==_indexOfMaxAngle&&p==0)||(i==_indexOfMaxAngle+1&&p==1));
           else if (profiles(i)>0.0&&
                    (p==0&&cose<0&&(profiles((i+1)%bufSize)<profiles(i)||offsets((i+1)%bufSize)>0)||
                     p==1&&cose>0&&(profiles(i>0?i-1:bufSize-1)<profiles(i)||offsets(i>0?i-1:bufSize-1)>0))||
                    (offsets(i)>0.0&&(p==0&&cose<0||p==1&&cose>0)))
            {
              XPoint *points=new XPoint[4];
              points[0].x=x1;
              points[0].y=y1;
              points[1].x=x2;
              points[1].y=y2;
              points[2].x=x2;
              points[2].y=(int)(y2+_pieDepth-yOffset);
              points[3].x=x1;
              points[3].y=(int)(y1+_pieDepth-yOffset);
              XSetForeground(display(),traceGC(),fillColors(i));
              PFillPolygon(display(),graphPixmap(),traceGC(),points,4,Complex,CoordModeOrigin);
              XSetForeground(display(),traceGC(),lineColors(i));
              XDrawLine(display(),graphPixmap(),traceGC(),x1,points[3].y,x2,points[2].y);
              XDrawLine(display(),graphPixmap(),traceGC(),x2,y2,x2,points[2].y);
              XDrawLine(display(),graphPixmap(),traceGC(),x1,y1,x1,points[3].y);
              delete [] points;
            }
           else if (profiles(i)>0.0&&sine<0)
            {
              XDrawLine(display(),graphPixmap(),traceGC(),x2,y2,x2,
			(int)(y2+_pieDepth-yOffset));
            }
           // draw outer vertical lines
           if (p==0&&profiles(i)>0.0)
            {
              // draw left side of pie
              if ((angle1(i)<=semi&&angle1(i)+angle2(i)>=semi)||
                  (angle1(i)>semi&&angle1(i)+angle2(i)>=circle+semi)||
                  (angle1(i)>semi&&angle1(i)<semi+64))
               {
                 XDrawLine(display(),graphPixmap(),traceGC(),(int)(_pieX+xo(i)),
			   (int)(_pieY-yo(i)+_pieRy+yOffset),
                           (int)(_pieX+xo(i)),(int)(_pieY-yo(i)+_pieRy+_pieDepth));
               }
              // draw right side of pie
              if (angle1(i)<circle&&angle1(i)+angle2(i)>=circle)
               {
                 XDrawLine(display(),graphPixmap(),traceGC(),(int)(_pieX+w_+xo(i)),
			   (int)(_pieY-yo(i)+_pieRy+yOffset),
                           (int)(_pieX+w_+xo(i)),(int)(_pieY-yo(i)+_pieRy+_pieDepth));
               }
            }
         }
        //draw radial lines
        if (w_>lineThreshold)
         {
           if ((i==_indexOfMaxAngle&&p==0)||(i==_indexOfMaxAngle+1&&p==1));
           else XDrawLine(display(),graphPixmap(),traceGC(),x1,y1,x2,y2);
         }
      }
   }
}

/**********************************************************************************************/
// drawLabels
/**********************************************************************************************/
void MSGraph::drawLabels(MSTrace * trace_,const MSGraphPieLabelData* pieLabelData_)
{
// draw labels
  MSTraceSet   *ts=trace_->traceSet();  
  XFontStruct *fontInfo=(XFontStruct*)server()->fontStruct(ts->textFont());
  int bufSize=trace_->dataCount();
  unsigned i=0;
  // pointers to needed data from MSGraphPieLabelData
  const XRectangle *iRects= pieLabelData_->iRects;
  const XRectangle *oRects= pieLabelData_->oRects;
  const MSStringVector* iText= pieLabelData_->iText;
  const MSStringVector* oText= pieLabelData_->oText;

//        XDrawRectangles(display(),graphPixmap(),traceGC(),iRects,bufSize);
//        XDrawRectangles(display(),graphPixmap(),traceGC(),oRects,bufSize);

  XSetForeground(display(),traceGC(),ts->textForeground());
  for (i=0;i<bufSize;i++)
   {
     unsigned t;
     int yt=iRects[i].y+fontInfo->ascent;
     int len=iText[i].length();
     for (t=0;t<len;t++)
      {
        int xt=iRects[i].x+
          (iRects[i].width-XTextWidth(fontInfo,iText[i](t),iText[i](t).length()))/2;
        XDrawString(display(),graphPixmap(),traceGC(),fontInfo,xt,yt,
                    iText[i](t),iText[i](t).length());
        yt+=fontInfo->ascent+fontInfo->descent;
      }
     yt=oRects[i].y+fontInfo->ascent;
     len=oText[i].length();
     for (t=0;t<len;t++)
      {
        int xt=oRects[i].x+
          (oRects[i].width-XTextWidth(fontInfo,oText[i](t),oText[i](t).length()))/2;
        XDrawString(display(),graphPixmap(),traceGC(),fontInfo,xt,yt,
                    oText[i](t),oText[i](t).length());
        yt+=fontInfo->ascent+fontInfo->descent;
      }
   }
}


void MSGraph::alignPieLabels(MSTrace *trace_,MSStringVector& iLabel_,MSStringVector& oLabel_,
                             const MSStringVector& percents_,unsigned i_)
{
  MSTraceSet *ts=trace_->traceSet();
  unsigned long legend=ts->pieLegendAlignment();
  unsigned long value=ts->pieValueAlignment();
  unsigned long percent=ts->piePercentAlignment();
  iLabel_.reshape(3);
  oLabel_.reshape(3);
  if (legend>MSNone)
   {
     unsigned index=legend&MSTop?0:legend&MSBottom?2:1;
     if (legend&Outside) oLabel_[index]=trace_->legend(i_);
     else iLabel_[index]=trace_->legend(i_);
   }
  if (value>MSNone)
   {
     MSString formatBuffer;
     ts->formatOutput(formatBuffer,i_,trace_->virtualCol());
     if (formatBuffer.length()>0)
      {
        unsigned index=value&MSTop?0:value&MSBottom?2:1;
        MSString aString=value&Outside?oLabel_[index]:iLabel_[index];
        if (aString.length()>0)
         {
           if (!(value&MSLeft)||legend&MSLeft) aString<<" "<<formatBuffer;
           else aString.insert(" ",0).insert(formatBuffer,0,' ');
         }
        else aString<<formatBuffer;
        if (value&Outside) oLabel_[index]=aString;
        else iLabel_[index]=aString;
      }
   }
  if (percent>MSNone)
   {
     int bufSize=trace_->dataCount();
     double total=0;
     for (unsigned i=0;i<bufSize;i++) total+=trace_->y(i_);
     unsigned index=percent&MSTop?0:percent&MSBottom?2:1;
     MSString aString=percent&Outside?oLabel_[index]:iLabel_[index];
     MSString pc=percents_(i_);
     pc<<"%";
     if (aString.length()>0)
      {
        if (percent&MSLeft&&!(legend&MSLeft)&&!(value&MSLeft))
         {
           aString.insert((pc<<" "));
         }
        else if (aString.numWords()>1)
         {
           if (percent&MSRight&&!(value&MSRight)&&!(legend&MSRight)) aString<<" "<<pc;
           else aString.insert((pc<<" "),aString.indexOf(" ")+1);
         }
        else aString<<" "<<pc;
      }
     else aString<<pc;
     if (percent&Outside) oLabel_[index]=aString;
     else iLabel_[index]=aString;
   }
  for (int i=2;i>=0;i--)
   {
     if (iLabel_(i).length()==0) iLabel_.removeAt(i);
     if (i<oLabel_.length()&&oLabel_(i).length()==0) oLabel_.removeAt(i);
   }
}

int MSGraph::findSelectedSlice(MSTrace *trace_, int x_, int y_)
{
  MSTraceSet *ts=trace_->traceSet();
  double x0, y0;                   // x, y coordinates of slice center
  double dX,dY;                    // distance from point to slice center by x and y 
  double distance;                 // distance from point to slice center
  double angle;
  int selectedSlice=-1;
  MSBoolean found=MSFalse;

  if (_pieData!=0) 
   {
     int bufSize=_pieData->bufSize;
     // for every slice find if point is within it's arc
     // if it is, compare angles.
     for(unsigned i=0;i<bufSize;i++)
      {
        x0=_pieX+_pieData->xo(i)+_pieRx;
        y0=_pieY-_pieData->yo(i)+(_pieDepth-_pieDepth*_pieData->profiles(i))+_pieRy;
        dX=x_-x0;
        dY=y_-y0;
        if ( (dX*dX)/(_pieRx*_pieRx) + (dY*dY)/(_pieRy*_pieRy) < 1)
         {
           // calculate angle from 3 o'clock position, counterclockwise
           dY=dY/ts->pieAspectRatio();        // adjust dY for pieAspectRatio
           distance=sqrt(dX*dX+dY*dY);
           if (dX>=0&&dY<=0) angle=asin(fabs(dY)/distance);
           else if(dX<0&&dY<=0) angle=asin(fabs(dX)/distance) + M_PI/2;
           else if(dX<0&&dY>=0) angle=asin(fabs(dY)/distance) + M_PI;
           else angle=asin(dX/distance) + M_PI*1.5;
           angle=angle/DegPerRadian;          // convert from radian 
           unsigned next=i==bufSize-1?0:i+1;
           double angle1=_pieData->angle1(i);
           double angle2=_pieData->angle1(next);
           if (angle2>angle1&&angle>angle1&&angle<=angle2) found=MSTrue;
           else if(angle2<angle1&&(angle>angle1||angle<=angle2)) found=MSTrue;
           if (found==MSTrue)
            {
              // slice found, make adjustments for indexOfMaxAngle shift
              if (_orderDependent)
               {
                 if(i<_indexOfMaxAngle) selectedSlice=i;
                 else if(i==_indexOfMaxAngle || i==_indexOfMaxAngle+1) selectedSlice=_indexOfMaxAngle;
                 else selectedSlice=i-1;
               }
              else selectedSlice=i;
              break;
            }
         }
      }
   }
  return selectedSlice;
}

void MSGraph::clearPieData(void)
{
  if(_pieData!=0)      delete _pieData;
  if(_pieLabelData!=0) delete _pieLabelData;
}

/**********************************************************************************************/
// MSGraphPieData
/**********************************************************************************************/

MSGraphPieData::MSGraphPieData(unsigned bufSize_)
{
  bufSize=bufSize_;
  allocate();
}

MSGraphPieData::~MSGraphPieData()
{
  deallocate();
}

void MSGraphPieData::clear(unsigned bufSize_)
{
  bufSize=bufSize_;
  angle1.removeAll();
  angle2.removeAll();
  percents.removeAll();
  xo.removeAll();
  yo.removeAll();
  radian1.removeAll();
  radian2.removeAll();
  offsets.removeAll();
  profiles.removeAll();
  lineColors.removeAll();
  fillColors.removeAll();
  bisectors.removeAll();
  sequence.removeAll();
  deallocate();
  allocate();
}

void MSGraphPieData::allocate(void)
{
  cosB=new double[bufSize];
  sinB=new double[bufSize];
  cos1=new double[bufSize];
  cos2=new double[bufSize];
  sin1=new double[bufSize];
  sin2=new double[bufSize];
}

void MSGraphPieData::deallocate(void)
{
  delete [] cosB;
  delete [] sinB;
  delete [] cos1;
  delete [] cos2;
  delete [] sin1;
  delete [] sin2;
}

/**********************************************************************************************/
// MSGraphPieLabelData
/**********************************************************************************************/

MSGraphPieLabelData::MSGraphPieLabelData(unsigned bufSize_)
{
  bufSize=bufSize_;
  allocate();
}

MSGraphPieLabelData::~MSGraphPieLabelData()
{
  deallocate();
}

void MSGraphPieLabelData::clear(unsigned bufSize_)
{
  bufSize=bufSize_;
  deallocate();
  allocate();
}

void MSGraphPieLabelData::allocate(void)
{
  iRects=new XRectangle[bufSize];
  oRects=new XRectangle[bufSize];
  iText=new MSStringVector[bufSize];
  oText=new MSStringVector[bufSize];
}

void MSGraphPieLabelData::deallocate(void)
{
  delete [] iText;
  delete [] oText;
  delete [] iRects;
  delete [] oRects;
}


 

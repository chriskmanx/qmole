///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSWidgetOutput.H>
#include <MSGUI/MSPixmap.H>
#include <MSGUI/MSDisplayPrint.H>

#ifdef MS_NO_INLINES
#include <MSGUI/MSWidgetOutputInlines.C>
#endif

MSWidgetOutput::DisplayPrint MSWidgetOutput::_displayPrint;
MSWidgetOutput::OutputMode MSWidgetOutput::_outputMode=Draw;

MSWidgetOutput::MSWidgetOutput(void) :
_shadow(this)
{
  init();
  initColors();
}

MSWidgetOutput::MSWidgetOutput(MSDisplayServer *server_) :
MSWidget(server_),
_shadow(this)
{
  init();
  // do not call initColors() if server_==0 because we may not
  // be connected to a Display Server - i.e. a ReportTable
  // running in batch mode.
  if (server_!=0) initColors();
}

MSWidgetOutput::MSWidgetOutput(MSWidget *owner_) :
MSWidget(owner_),
_shadow(this)
{
  init();
  initColors();
}

MSWidgetOutput::~MSWidgetOutput(void)
{}

void MSWidgetOutput::outputMode(MSWidgetOutput::OutputMode mode_)  
{
  if (mode_==Print&&displayPrintMode()==MSP::PPM) _outputMode=DrawPPM;
  else _outputMode=mode_;
}

MSWidgetOutput::DisplayPrint::DisplayPrint(void)
{ _displayPrint=0; }

MSWidgetOutput::DisplayPrint::~DisplayPrint(void)
{ if (_displayPrint!=0) delete _displayPrint; }

MSDisplayPrint *MSWidgetOutput::displayPrint(void) 
{
  if (_displayPrint._displayPrint==0) _displayPrint._displayPrint=new MSDisplayPrint;
  return _displayPrint._displayPrint;
}

void MSWidgetOutput::XDrawArc(Display *dpy_,Window id_,GC gc_,
			      int x_,int y_,int w_,int h_,int angle1_,int angle2_)
{
  if (_outputMode==Draw) ::XDrawArc(dpy_,id_,gc_,x_,y_,w_,h_,angle1_,angle2_);
  else if (displayPrintMode()==MSP::PPM)
   {
     int x=x_+_displayPrint._displayPrint->x_org();
     int y=y_+_displayPrint._displayPrint->y_org();
     ::XDrawArc(dpy_,displayPrintPixmap(),gc_,x,y,w_,h_,angle1_,angle2_);
   }
  else _displayPrint._displayPrint->printArc(gc_,x_,y_,w_,h_,angle1_,angle2_);
}
  
void MSWidgetOutput::XDrawArcs(Display *dpy_,Window id_,GC gc_,XArc *arcs_,int n_)
{
  if (_outputMode==Draw) ::XDrawArcs(dpy_,id_,gc_,arcs_,n_);
  else if (displayPrintMode()==MSP::PPM)
   {
     XArc *arcs=new XArc[n_];
     for (unsigned i=0;i<n_;i++)
      {
        arcs[i]=arcs_[i];
        arcs[i].x+=_displayPrint._displayPrint->x_org();
        arcs[i].y+=_displayPrint._displayPrint->y_org();
      }
     ::XDrawArcs(dpy_,displayPrintPixmap(),gc_,arcs,n_);
     delete [] arcs;
   }
  else _displayPrint._displayPrint->printArcs(gc_,arcs_,n_);
}

void MSWidgetOutput::XDrawLine(Display *dpy_,Window id_,GC gc_,
			       int x1_,int y1_,int x2_,int y2_)
{
  if (_outputMode==Draw) ::XDrawLine(dpy_,id_,gc_,x1_,y1_,x2_,y2_);
  else if (displayPrintMode()==MSP::PPM)
   {
     int x1=x1_+_displayPrint._displayPrint->x_org();
     int y1=y1_+_displayPrint._displayPrint->y_org();
     int x2=x2_+_displayPrint._displayPrint->x_org();
     int y2=y2_+_displayPrint._displayPrint->y_org();
     ::XDrawLine(dpy_,displayPrintPixmap(),gc_,x1,y1,x2,y2);
   }
  else _displayPrint._displayPrint->printLine(gc_,x1_,y1_,x2_,y2_);
}
  
void MSWidgetOutput::XDrawLines(Display *dpy_,Window id_,GC gc_,
				XPoint *points_,int n_,int mode_)
{
  if (_outputMode==Draw) ::XDrawLines(dpy_,id_,gc_,points_,n_,mode_);
  else if (displayPrintMode()==MSP::PPM)
   {
     XPoint *points=new XPoint[n_];
     for (unsigned i=0;i<n_;i++)
      {
        points[i].x=points_[i].x+_displayPrint._displayPrint->x_org();
        points[i].y=points_[i].y+_displayPrint._displayPrint->y_org();
      }
     ::XDrawLines(dpy_,displayPrintPixmap(),gc_,points,n_,mode_);
     delete [] points;
   }
  else _displayPrint._displayPrint->printLines(gc_,points_,n_,mode_);
}
  
void MSWidgetOutput::XDrawSegments(Display *dpy_,Window id_,GC gc_,
				   XSegment *segments_,int n_)
{
  if (_outputMode==Draw) ::XDrawSegments(dpy_,id_,gc_,segments_,n_);
  else if (displayPrintMode()==MSP::PPM)
   {
     XSegment *segments=new XSegment[n_];
     for (unsigned i=0;i<n_;i++)
      {
        segments[i].x1=segments_[i].x1+_displayPrint._displayPrint->x_org();
        segments[i].y1=segments_[i].y1+_displayPrint._displayPrint->y_org();
        segments[i].x2=segments_[i].x2+_displayPrint._displayPrint->x_org();
        segments[i].y2=segments_[i].y2+_displayPrint._displayPrint->y_org();
      }
     ::XDrawSegments(dpy_,displayPrintPixmap(),gc_,segments,n_);
     delete [] segments;
   }
  else _displayPrint._displayPrint->printSegments(gc_,segments_,n_);
}
  
void MSWidgetOutput::XDrawPoint(Display *dpy_,Window id_,GC gc_,int x_,int y_)
{
  if (_outputMode==Draw) ::XDrawPoint(dpy_,id_,gc_,x_,y_);
  else if (displayPrintMode()==MSP::PPM)
   {
     int x=x_+_displayPrint._displayPrint->x_org();
     int y=y_+_displayPrint._displayPrint->y_org();
     ::XDrawPoint(dpy_,displayPrintPixmap(),gc_,x,y);
   }
  else _displayPrint._displayPrint->printPoint(gc_,x_,y_);
}

void MSWidgetOutput::XDrawPoints(Display *dpy_,Window id_,GC gc_,
				 XPoint *points_,int n_,int mode_)
{
  if (_outputMode==Draw) ::XDrawPoints(dpy_,id_,gc_,points_,n_,mode_);
  else if (displayPrintMode()==MSP::PPM)
   {
     XPoint *points=new XPoint[n_];
     for (unsigned i=0;i<n_;i++)
      {
        points[i].x=points_[i].x+_displayPrint._displayPrint->x_org();
        points[i].y=points_[i].y+_displayPrint._displayPrint->y_org();
      }
     ::XDrawPoints(dpy_,displayPrintPixmap(),gc_,points,n_,mode_);
     delete [] points;
   }
  else _displayPrint._displayPrint->printPoints(gc_,points_,n_,mode_);
}

void MSWidgetOutput::XDrawRectangle(Display *dpy_,Window id_,GC gc_,
				    int x_,int y_,int w_,int h_)
{
  if (_outputMode==Draw) ::XDrawRectangle(dpy_,id_,gc_,x_,y_,w_,h_);
  else if (displayPrintMode()==MSP::PPM)
   {
     int x=x_+_displayPrint._displayPrint->x_org();
     int y=y_+_displayPrint._displayPrint->y_org();
     ::XDrawRectangle(dpy_,displayPrintPixmap(),gc_,x,y,w_,h_);
   }
  else _displayPrint._displayPrint->printRectangle(gc_,x_,y_,w_,h_);
}

void MSWidgetOutput::XDrawRectangles(Display *dpy_,Window id_,GC gc_,XRectangle *rects_,int n_)
{
  if (_outputMode==Draw) ::XDrawRectangles(dpy_,id_,gc_,rects_,n_);
  else if (displayPrintMode()==MSP::PPM)
   {
     XRectangle *rects=new XRectangle[n_];
     for (unsigned i=0;i<n_;i++)
      {
        rects[i]=rects_[i];
        rects[i].x+=_displayPrint._displayPrint->x_org();
        rects[i].y+=_displayPrint._displayPrint->y_org();
      }
     ::XDrawRectangles(dpy_,displayPrintPixmap(),gc_,rects,n_);
     delete [] rects;
   }
  else _displayPrint._displayPrint->printRectangles(gc_,rects_,n_);
}
 
void MSWidgetOutput::XDrawString(Display *dpy_,Window id_,GC gc_,
				 const XFontStruct *fs_, int x_,int y_,const char *string_,int n_)
{
  if (_outputMode==Draw)
   {
     if (doubleByte (fs_)) ::XDrawString16(dpy_,id_,gc_,x_,y_,(XChar2b*)string_,n_/2);
     else ::XDrawString(dpy_,id_,gc_,x_,y_,string_,n_);
   }
  else if (displayPrintMode()==MSP::PPM)
   {
     int x=x_+_displayPrint._displayPrint->x_org();
     int y=y_+_displayPrint._displayPrint->y_org();
     if (doubleByte (fs_)) ::XDrawString16(dpy_,displayPrintPixmap(),gc_,x,y,(XChar2b*)string_,n_/2);
     else ::XDrawString(dpy_,displayPrintPixmap(),gc_,x,y,string_,n_);
   }
  else _displayPrint._displayPrint->printString(gc_,x_,y_,string_,n_);
}

void MSWidgetOutput::XDrawImageString(Display *dpy_,Window id_,GC gc_,
				      const XFontStruct *fs_, int x_,int y_,
				      const char *string_,int n_)
{
  if (_outputMode==Draw)
   {
     if (doubleByte (fs_)) ::XDrawImageString16(dpy_,id_,gc_,x_,y_,(XChar2b*)string_,n_/2);
     else ::XDrawImageString(dpy_,id_,gc_,x_,y_,string_,n_);
   }
  else if (displayPrintMode()==MSP::PPM)
   {
     int x=x_+_displayPrint._displayPrint->x_org();
     int y=y_+_displayPrint._displayPrint->y_org();
     if (doubleByte (fs_)) ::XDrawImageString16(dpy_,displayPrintPixmap(),gc_,x,y,(XChar2b*)string_,n_/2);
     else ::XDrawImageString(dpy_,displayPrintPixmap(),gc_,x,y,string_,n_);
   }
  else _displayPrint._displayPrint->printImageString(gc_,x_,y_,string_,n_);
}

  
void MSWidgetOutput::XFillArc(Display *dpy_,Window id_,GC gc_,
			      int x_,int y_,int w_,int h_,int angle1_,int angle2_)
{
  if (_outputMode==Draw) ::XFillArc(dpy_,id_,gc_,x_,y_,w_,h_,angle1_,angle2_);
  else if (displayPrintMode()==MSP::PPM)
   {
     int x=x_+_displayPrint._displayPrint->x_org();
     int y=y_+_displayPrint._displayPrint->y_org();
     ::XFillArc(dpy_,displayPrintPixmap(),gc_,x,y,w_,h_,angle1_,angle2_);
   }
  else _displayPrint._displayPrint->printFillArc(gc_,x_,y_,w_,h_,angle1_,angle2_);
}
  
void MSWidgetOutput::XFillArcs(Display *dpy_,Window id_,GC gc_,XArc *arcs_,int n_)
{
  if (_outputMode==Draw) ::XFillArcs(dpy_,id_,gc_,arcs_,n_);
  else if (displayPrintMode()==MSP::PPM)
   {
     XArc *arcs=new XArc[n_];
     for (unsigned i=0;i<n_;i++)
      {
        arcs[i]=arcs_[i];
        arcs[i].x+=_displayPrint._displayPrint->x_org();
        arcs[i].y+=_displayPrint._displayPrint->y_org();
      }
     ::XFillArcs(dpy_,displayPrintPixmap(),gc_,arcs,n_);
     delete [] arcs;
   }
  else _displayPrint._displayPrint->printFillArcs(gc_,arcs_,n_);
}
  
void MSWidgetOutput::XFillPolygon(Display *dpy_,Window id_,GC gc_,
				  XPoint *points_,int n_,int shape_,int mode_)
{
  if (_outputMode==Draw) ::XFillPolygon(dpy_,id_,gc_,points_,n_,shape_,mode_);
  else if (displayPrintMode()==MSP::PPM)
   {
     XPoint *points=new XPoint[n_];
     for (unsigned i=0;i<n_;i++)
      {
        points[i].x=points_[i].x+_displayPrint._displayPrint->x_org();
        points[i].y=points_[i].y+_displayPrint._displayPrint->y_org();
      }
     ::XFillPolygon(dpy_,displayPrintPixmap(),gc_,points,n_,shape_,mode_);
     delete [] points;
   }
  else _displayPrint._displayPrint->printFillPolygon(gc_,points_,n_,shape_,mode_);
}
  
void MSWidgetOutput::XFillRectangle(Display *dpy_,Window id_,GC gc_,
				    int x_,int y_,int w_,int h_)
{
  if (_outputMode==Draw) ::XFillRectangle(dpy_,id_,gc_,x_,y_,w_,h_);
  else if (displayPrintMode()==MSP::PPM)
   {
     int x=x_+_displayPrint._displayPrint->x_org();
     int y=y_+_displayPrint._displayPrint->y_org();
     ::XFillRectangle(dpy_,displayPrintPixmap(),gc_,x,y,w_,h_);
   }
  else _displayPrint._displayPrint->printFillRectangle(gc_,x_,y_,w_,h_);
}
  
void MSWidgetOutput::XFillRectangles(Display *dpy_,Window id_,GC gc_,
				     XRectangle *rects_,int n_)
{
  if (_outputMode==Draw) ::XFillRectangles(dpy_,id_,gc_,rects_,n_);
  else if (displayPrintMode()==MSP::PPM)
   {
     XRectangle *rects=new XRectangle[n_];
     for (unsigned i=0;i<n_;i++)
      {
        rects[i]=rects_[i];
        rects[i].x+=_displayPrint._displayPrint->x_org();
        rects[i].y+=_displayPrint._displayPrint->y_org();
      }
     ::XFillRectangles(dpy_,displayPrintPixmap(),gc_,rects,n_);
     delete [] rects;
   }
  else _displayPrint._displayPrint->printFillRectangles(gc_,rects_,n_);
}
  
void MSWidgetOutput::XBFillPolygon(Display *dpy_,Window id_,GC gc_,
				   XPoint *points_,int n_,int shape_,int mode_)
{
  if (_outputMode==Draw) ::XFillPolygon(dpy_,id_,gc_,points_,n_,shape_,mode_);
  else if (displayPrintMode()==MSP::PPM)
   {
     XPoint *points=new XPoint[n_];
     for (unsigned i=0;i<n_;i++)
      {
        points[i].x=points_[i].x+_displayPrint._displayPrint->x_org();
        points[i].y=points_[i].y+_displayPrint._displayPrint->y_org();
      }
     ::XFillPolygon(dpy_,displayPrintPixmap(),gc_,points,n_,shape_,mode_);
     delete [] points;
   }
  else _displayPrint._displayPrint->printFillPolygon(gc_,points_,n_,shape_,mode_,MSTrue);
}
  
void MSWidgetOutput::XBFillRectangle(Display *dpy_,Window id_,GC gc_,
				     int x_,int y_,int w_,int h_)
{
  if (_outputMode==Draw) ::XFillRectangle(dpy_,id_,gc_,x_,y_,w_,h_);
  else if (displayPrintMode()==MSP::PPM)
   {
     int x=x_+_displayPrint._displayPrint->x_org();
     int y=y_+_displayPrint._displayPrint->y_org();
     ::XFillRectangle(dpy_,displayPrintPixmap(),gc_,x,y,w_,h_);
   }
  else _displayPrint._displayPrint->printFillRectangle(gc_,x_,y_,w_,h_,MSTrue);
}
  
void MSWidgetOutput::XBFillRectangles(Display *dpy_,Window id_,GC gc_,
				      XRectangle *rects_,int n_)
{
  if (_outputMode==Draw) ::XFillRectangles(dpy_,id_,gc_,rects_,n_);
  else if (displayPrintMode()==MSP::PPM)
   {
     XRectangle *rects=new XRectangle[n_];
     for (unsigned i=0;i<n_;i++)
      {
        rects[i]=rects_[i];
        rects[i].x+=_displayPrint._displayPrint->x_org();
        rects[i].y+=_displayPrint._displayPrint->y_org();
      }
     ::XFillRectangles(dpy_,displayPrintPixmap(),gc_,rects,n_);
     delete [] rects;
   }
  else _displayPrint._displayPrint->printFillRectangles(gc_,rects_,n_,MSTrue);
}
  
void MSWidgetOutput::PFillPolygon(Display *dpy_,Window id_,GC gc_,
				  XPoint *points_,int n_,int shape_,int mode_)
{
  if (_outputMode==Draw) ::XFillPolygon(dpy_,id_,gc_,points_,n_,shape_,mode_);
  else if (displayPrintMode()==MSP::PPM)
   {
     XPoint *points=new XPoint[n_];
     for (unsigned i=0;i<n_;i++)
      {
        points[i].x=points_[i].x+_displayPrint._displayPrint->x_org();
        points[i].y=points_[i].y+_displayPrint._displayPrint->y_org();
      }
     ::XFillPolygon(dpy_,displayPrintPixmap(),gc_,points,n_,shape_,mode_);
     delete [] points;
   }
  else _displayPrint._displayPrint->printFillPolygon(gc_,points_,n_,shape_,mode_,MSTrue);
}
  
void MSWidgetOutput::PFillRectangle(Display *dpy_,Window id_,GC gc_,
				    int x_,int y_,int w_,int h_)
{
  if (_outputMode==Draw) ::XFillRectangle(dpy_,id_,gc_,x_,y_,w_,h_);
  else if (displayPrintMode()==MSP::PPM)
   {
     int x=x_+_displayPrint._displayPrint->x_org();
     int y=y_+_displayPrint._displayPrint->y_org();
     ::XFillRectangle(dpy_,displayPrintPixmap(),gc_,x,y,w_,h_);
   }
  else _displayPrint._displayPrint->printFillRectangle(gc_,x_,y_,w_,h_,MSTrue);
}

void MSWidgetOutput::PFillRectangles(Display *dpy_,Window id_,GC gc_,
				     XRectangle *rects_,int n_)
{
  if (_outputMode==Draw) ::XFillRectangles(dpy_,id_,gc_,rects_,n_);
  else if (displayPrintMode()==MSP::PPM)
   {
     XRectangle *rects=new XRectangle[n_];
     for (unsigned i=0;i<n_;i++)
      {
        rects[i]=rects_[i];
        rects[i].x+=_displayPrint._displayPrint->x_org();
        rects[i].y+=_displayPrint._displayPrint->y_org();
      }
     ::XFillRectangles(dpy_,displayPrintPixmap(),gc_,rects,n_);
     delete [] rects;
   }
  else _displayPrint._displayPrint->printFillRectangles(gc_,rects_,n_,MSTrue);
}

void MSWidgetOutput::XCopyArea(Display *dpy_,Window src_,Window dest_,GC gc_,
                               int x_src_,int y_src_,int w_,int h_,int x_dest_,int y_dest_)
{
  if (_outputMode==Draw) ::XCopyArea(dpy_,src_,dest_,gc_,x_src_,y_src_,w_,h_,x_dest_,y_dest_);
  else if (displayPrintMode()==MSP::PPM)
   {
     int x=x_dest_+_displayPrint._displayPrint->x_org();
     int y=y_dest_+_displayPrint._displayPrint->y_org();
     ::XCopyArea(dpy_,src_,displayPrintPixmap(),gc_,x_src_,y_src_,w_,h_,x,y);
   }
}

void MSWidgetOutput::XSetClipRectangles(Display *dpy_,GC gc_,
					int x_,int y_,XRectangle *rects_,int n_,int order_)
{
  if (_outputMode==Draw) ::XSetClipRectangles(dpy_,gc_,x_,y_,rects_,n_,order_);
  else if (displayPrintMode()==MSP::PPM)
   {
     XRectangle *rects=new XRectangle[n_];
     for (unsigned i=0;i<n_;i++)
      {
        rects[i]=rects_[i];
        rects[i].x+=_displayPrint._displayPrint->x_org();
        rects[i].y+=_displayPrint._displayPrint->y_org();
      }
     ::XSetClipRectangles(dpy_,gc_,x_,y_,rects,n_,order_);
     delete [] rects;
   }
  else _displayPrint._displayPrint->printSetClipRectangles(gc_,x_,y_,rects_,n_,order_);
}
  
void MSWidgetOutput::XSetDashes(Display *dpy_,GC gc_,int dash_offset_,const char *dash_list_,int n_)
{
  if (_outputMode==Print) _displayPrint._displayPrint->printSetDashes(gc_,dash_list_,n_);
  else ::XSetDashes(dpy_,gc_,dash_offset_,dash_list_,n_);
}
  
void MSWidgetOutput::XFreeGC(Display *dpy_,GC gc_)
{
  if (_outputMode==Print) _displayPrint._displayPrint->printFreeGC(gc_);
  ::XFreeGC(dpy_,gc_);
}

// #########################################################
// the following hide the interface of MSDisplayPrint:
// #########################################################
MSBoolean MSWidgetOutput::displayPrintOpen(void)
{ return displayPrint()->printOpen(); }
MSBoolean MSWidgetOutput::displayPrintOpen(MSWidget *pWidget_)
{ return displayPrint()->printOpen(pWidget_); }
MSBoolean MSWidgetOutput::displayPrintClose(void)
{ return displayPrint()->printClose(); }
int MSWidgetOutput::displayPrintMode(void)
{ return displayPrint()->outputMode(); }
Window MSWidgetOutput::displayPrintPixmap(void)
{ return displayPrint()->ppmPixmap(); }
void MSWidgetOutput::displayPrintOriginInc(MSWidget *pWidget_)
{ displayPrint()->originInc(pWidget_); }
void MSWidgetOutput::displayPrintOriginDec(MSWidget *pWidget_)
{ displayPrint()->originDec(pWidget_); }
void MSWidgetOutput::displayPrintFileName(const char *pString_)
{ displayPrint()->fileName(pString_); }
void MSWidgetOutput::displayPrintXorigin(int x_)
{ displayPrint()->x_org(x_); }
void MSWidgetOutput::displayPrintYorigin(int y_)
{ displayPrint()->y_org(y_); }
void MSWidgetOutput::displayPrintInit(MSWidget *pWidget_)
{ displayPrint()->printInit(pWidget_); }
void MSWidgetOutput::displayPrintClear(void)
{ displayPrint()->printClear(); }
ofstream& MSWidgetOutput::displayPrintStream(void)
{ return displayPrint()->pout; }

// #########################################################
// basic widget drawing methods:
//
// drawFlat
// drawBevel
// drawEtched
// drawDiamond
// #########################################################

static void getRects(XRectangle *rects,int count_,int offset,
                     int x,int y,int width,int height,int pos_top,int pos_left,
                     int pos_bottom,int pos_right)
{
  int offsetX2;
  for (int i=0;i<count_;i++,offset++)
   {
     offsetX2=offset+offset;
     
     //  Top segments  
     rects[pos_top+i].x=x+offset;
     rects[pos_top+i].y=y+offset;
     rects[pos_top+i].width=width-offsetX2-1;
     rects[pos_top+i].height=1;
     
     //  Left segments  
     rects[pos_left+i].x=x+offset;
     rects[pos_left+i].y=y+offset;
     rects[pos_left+i].width=1;
     rects[pos_left+i].height=height-offsetX2-1;
     
     //  Bottom segments  
     rects[pos_bottom+i].x=x+offset;
     rects[pos_bottom+i].y=y+height-offset-1;
     rects[pos_bottom+i].width=width-offsetX2;
     rects[pos_bottom+i].height=1;
     
     //  Right segments  
     rects[pos_right+i].x=x+width-offset-1;
     rects[pos_right+i].y=y+offset;
     rects[pos_right+i].width=1;
     rects[pos_right+i].height=height-offsetX2;
   }
}

void MSWidgetOutput::drawFlatShadow(Window window_,const MSRect& aRect_,int thickness_,GC gc_)
{
  if (mapped()==MSTrue&&thickness_>0)
   {
     XRectangle rect[4];
     rect[0].x=aRect_.x();
     rect[0].y=aRect_.y();
     rect[0].width=aRect_.width();
     rect[0].height=thickness_;
     
     rect[1].x=aRect_.x();
     rect[1].y=aRect_.y();
     rect[1].width=thickness_;
     rect[1].height=aRect_.height();
     
     rect[2].x=aRect_.x()+aRect_.width()-thickness_;
     rect[2].y=aRect_.y();
     rect[2].width=thickness_;
     rect[2].height=aRect_.height();
     
     rect[3].x=aRect_.x();
     rect[3].y=aRect_.y()+aRect_.height()-thickness_;
     rect[3].width=aRect_.width();
     rect[3].height=thickness_;
     
     XBFillRectangles(display(),window_,gc_,rect,4);
   }
}

void MSWidgetOutput::drawEtchedShadow(Window window_,const MSRect& aRect_,MSShadowStyle style_,
				      int thickness_,GC topGC_,GC bottomGC_)
{
  if (mapped()==MSTrue&&thickness_>0)
   {
     int x=aRect_.x();
     int y=aRect_.y();
     int width=aRect_.width();
     int height=aRect_.height();
  
     int half_size;
     int size2;
     int size3;
     int pos_top,pos_left,pos_bottom,pos_right;
  
     if (thickness_<=0) return;
     if (thickness_==1) 
      {
	GC gc=bottomGC_;
	if (style_==MSEtchedIn) gc=topGC_;
	drawFlatShadow(window_,aRect_,thickness_,gc);
        return;
      } 
  
     if (thickness_>(width>>1))  thickness_=(width>>1);
     if (thickness_>(height>>1)) thickness_=(height>>1);
     if (thickness_<=0) return;
  
     thickness_=(thickness_%2)?(thickness_-1):(thickness_);
  
     half_size=(thickness_>>1);
     size2=thickness_+thickness_;
     size3=size2+thickness_;
  
     XRectangle *rects=new XRectangle[thickness_*4];
  
     pos_top   =0;
     pos_left  =half_size;
     pos_bottom=size2;
     pos_right =size2+half_size;
  
     getRects(rects,half_size,0,x,y,width,height,
   	      pos_top,pos_left,pos_bottom,pos_right);
  
     pos_top   =size3;
     pos_left  =size3+half_size;
     pos_bottom=thickness_;
     pos_right =thickness_+half_size;
  
     getRects(rects,half_size,half_size,x,y,width,height,
      	      pos_top,pos_left,pos_bottom,pos_right);
  
    XBFillRectangles(display(),window_,bottomGC_,&rects[size2],size2);
    XBFillRectangles(display(),window_,topGC_,&rects[0],size2);
    delete [] rects;
  }
}

void MSWidgetOutput::drawBevelShadow(Window window_,const MSRect& aRect_,
				     int thickness_,GC topGC_,GC bottomGC_)
{
  XPoint points[7];
  if (mapped()==MSTrue&&thickness_>0)
   {
     points[0].x=points[1].x=points[6].x=aRect_.x();
     points[0].y=points[6].y=aRect_.y()+aRect_.height();
     points[1].y=points[2].y=aRect_.y();
     points[2].x=aRect_.x()+aRect_.width();
     points[3].x=aRect_.x()+aRect_.width()-thickness_;
     points[3].y=points[4].y=aRect_.y()+thickness_;
     points[4].x=points[5].x=aRect_.x()+thickness_;
     points[5].y=aRect_.y()+aRect_.height()-thickness_;
     
     XBFillRectangle(display(),window_,bottomGC_,
		     aRect_.x(),aRect_.y()+aRect_.height()-thickness_,
		     aRect_.width(),thickness_);
     XBFillRectangle(display(),window_,bottomGC_,
		     aRect_.x()+aRect_.width()-thickness_,aRect_.y(),
		     thickness_,aRect_.height());
     XBFillPolygon(display(),window_,topGC_,points,7,Nonconvex,CoordModeOrigin);
   }
}

void MSWidgetOutput::drawDiamondShadow(Window window_,const MSRect& aRect_,MSBoolean armed_,
				       GC topGC_,GC bottomGC_,GC backgroundGC_,GC armedGC_)
{
  if (mapped()==MSTrue)
   {
     XSegment seg[12];
     XPoint   pt[5];
     int x=aRect_.x();
     int y=aRect_.y();
     int size=aRect_.height();
     int midX=x+((size+1)>>1);
     int midY=y+((size+1)>>1);

     XFillRectangle(display(),window_,backgroundGC_,x,y,size,size); 
     
     // Counter Reverse Drawing Effect On Tiny RadioButtons
     if (size<=3)
      {
	//  The top shadow segments  
	seg[0].x1=x+size-1;	
	seg[0].y1=midY-1;
	seg[0].x2=midX-1;	
	seg[0].y2=y+size-1;
	
	seg[1].x1=x+size-2;	
	seg[1].y1=midY-1;
	seg[1].x2=midX-1;	
	seg[1].y2=y+size-2;
	
	seg[2].x1=x+size-3;	
	seg[2].y1=midY-1;
	seg[2].x2=midX-1;
	seg[2].y2=y+size-3;
	
	/*--*/
	
	seg[3].x1=midX-1;	
	seg[3].y1=y+size-1;
	seg[3].x2=x;		
	seg[3].y2=midY-1;
	
	seg[4].x1=midX-1;	
	seg[4].y1=y+size-2;
	seg[4].x2=x+1;		
	seg[4].y2=midY-1;
	
	seg[5].x1=midX-1;	
	seg[5].y1=y+size-3;
	seg[5].x2=x+2;		
	seg[5].y2=midY-1;
	
	//  The bottom shadow segments  
	seg[6].x1=x+size-1;	
	seg[6].y1=midY-1;
	seg[6].x2=midX-1;	
	seg[6].y2=y;
	
	seg[7].x1=x+size-2;	
	seg[7].y1=midY-1;
	seg[7].x2=midX-1;	
	seg[7].y2=y+1;
	
	seg[8].x1=x+size-3;	
	seg[8].y1=midY-1;
	seg[8].x2=midX-1;	
	seg[8].y2=y+2;
	
	/*--*/
	
	seg[9].x1=midX-1;	
	seg[9].y1=y;
	seg[9].x2=x;		
	seg[9].y2=midY-1;
	
	seg[10].x1=midX-1;	
	seg[10].y1=y+1;
	seg[10].x2=x+1;	
	seg[10].y2=midY-1;
	
	seg[11].x1=midX-1;	
	seg[11].y1=y+2;
	seg[11].x2=x+2;	
	seg[11].y2=midY-1;
      }
     else    // Normal Sized RadioButtons
      {
	//  The top shadow segments  
	seg[0].x1=x;		
	seg[0].y1=midY-1;
	seg[0].x2=midX-1;	
	seg[0].y2=y;
	
	seg[1].x1=x+1;		
	seg[1].y1=midY-1;
	seg[1].x2=midX-1;	
	seg[1].y2=y+1;
	
	seg[2].x1=x+2;		
	seg[2].y1=midY-1;
	seg[2].x2=midX-1;	
	seg[2].y2=y+2;
	
	/*--*/
	
	seg[3].x1=midX-1;	
	seg[3].y1=y;
	seg[3].x2=x+size-1;	
	seg[3].y2=midY-1;
	
	seg[4].x1=midX-1;		
	seg[4].y1=y+1;
	seg[4].x2=x+size-2;	
	seg[4].y2=midY-1;
	
	seg[5].x1=midX-1;	
	seg[5].y1=y+2;
	seg[5].x2=x+size-3;	
	seg[5].y2=midY-1;
	
	
	//  The bottom shadow segments  
	
	seg[6].x1=x;		
	seg[6].y1=midY-1;
	seg[6].x2=midX-1;	
	seg[6].y2=y+size-1;
	
	seg[7].x1=x+1;		
	seg[7].y1=midY-1;
	seg[7].x2=midX-1;	
	seg[7].y2=y+size-2;
	
	seg[8].x1=x+2;		
	seg[8].y1=midY-1;
	seg[8].x2=midX-1;	
	seg[8].y2=y+size-3;
	
	/*--*/
	
	seg[9].x1=midX-1;	
	seg[9].y1=y+size-1;
	seg[9].x2=x+size-1;	
	seg[9].y2=midY-1;
	
	seg[10].x1=midX-1;	
	seg[10].y1=y+size-2;
	seg[10].x2=x+size-2;	
	seg[10].y2=midY-1;
	
	seg[11].x1=midX-1;	
	seg[11].y1=y+size-3;
	seg[11].x2=x+size-3;	
	seg[11].y2=midY-1;
      } 
     
     XDrawSegments(display(),window_,(armed_==MSTrue)?bottomGC_:topGC_,&seg[3],3);
     XDrawSegments(display(),window_,(armed_==MSTrue)?topGC_:bottomGC_,&seg[6],6);
     XDrawSegments(display(),window_,(armed_==MSTrue)?bottomGC_:topGC_,&seg[0],3);

     // For Fill 
     if (armed_==MSTrue)
      {
	pt[0].x=x+3;
	pt[0].y=midY-1;
	pt[1].x=midX-1 ;
	pt[1].y=y+2;
	pt[2].x=x+size-3;
	pt[2].y=midY-1;
	pt[3].x=midX-1 ;
	pt[3].y=y+size-3;
      }
     else
      {
	pt[0].x=x+4;
	pt[0].y=midY-1;
	pt[1].x=midX-1;
	pt[1].y=y+3;
	pt[2].x=x+size-4;
	pt[2].y=midY-1;
	pt[3].x=midX-1;
	pt[3].y=y+size-4;
      }
     
     // Counter Reverse Drawing Effect On Tiny RadioButtons
     if (pt[0].x>pt[1].x)
      {
	pt[1].x=pt[0].x;
	pt[2].x=pt[0].x;
	pt[3].x=pt[0].x;
      }
     if (pt[0].y<pt[1].y)
      {
	pt[1].x=pt[0].x;
	pt[2].x=pt[0].x;
	pt[3].x=pt[0].x;
      }

     if(outputMode()==Draw || armed_==MSTrue)
       {
	 XBFillPolygon(display(),window_,(armed_==MSTrue)?armedGC_:backgroundGC_,
		      pt,4,Convex,CoordModeOrigin);
       }
   }
}

// ##############################################################################
// 
// ##############################################################################

void MSWidgetOutput::initColors(void)
{
  if (_server!=0)
   {
     _highlightColor=_server->defaultHighlightColor();
     createGCs();
   }
}

void MSWidgetOutput::init(void)
{
  _highlightThickness=0;
  _shadowThickness=0;
  _highlighted=MSFalse;
  _shadowStyle=MSSunken;
  _topShadowOffset=0;
  _highlightColor=0;
}

void MSWidgetOutput::createGCs(void)
{
  unsigned long valueMask=(GCForeground);
  XGCValues values;
  values.foreground=highlightColor();
  _highlightMSGC.setGCValues(_server,MSTrue,&values,valueMask);
}

Window MSWidgetOutput::windowForDrawing(void)
{ return (_window==0)?_owner->window():_window; }
MSBoolean MSWidgetOutput::canDraw(void)
{ return (_window==0)?_owner->mapped():mapped(); }
int MSWidgetOutput::xDrawingOffset(void)
{ return (_window==0)?x_origin():0; }
int MSWidgetOutput::yDrawingOffset(void)
{ return (_window==0)?y_origin():0;}

MSGC& MSWidgetOutput::highlightMSGC(void)
{ return _highlightMSGC; }

MSShadow& MSWidgetOutput::shadow(void)
{ return _shadow; }
const MSShadow& MSWidgetOutput::shadow(void) const
{ return _shadow; }

GC MSWidgetOutput::highlightGC(void) const
{ return _highlightMSGC.gc(); }
GC MSWidgetOutput::bottomShadowGC(void) const
{ return _shadow.bottomShadowGC(); }
GC MSWidgetOutput::topShadowGC(void) const
{ return _shadow.topShadowGC(); }
GC MSWidgetOutput::backgroundShadowGC(void) const
{ return _shadow.backgroundShadowGC(); }
GC MSWidgetOutput::selectShadowGC(void) const
{ return _shadow.selectShadowGC(); }
unsigned long MSWidgetOutput::selectShadowColor(void) const
{ return _shadow.select(); }
unsigned long MSWidgetOutput::topShadowColor(void) const
{ return _shadow.topShadow(); }
unsigned long MSWidgetOutput::bottomShadowColor(void) const
{ return _shadow.bottomShadow(); }

void MSWidgetOutput::highlightColor(const char *color_)
{ highlightColor(_server->pixel(color_)); }

void MSWidgetOutput::highlightColor(unsigned long pixel_)
{
  if (pixel_!=highlightColor())
   {
     _highlightColor=pixel_;
     highlightMSGC().foreground(highlightColor());
     if (highlighted()==MSTrue) drawHighlight();
   }
}

void MSWidgetOutput::highlightThickness(int ht_)
{
  if (ht_!=highlightThickness())
   {
     _highlightThickness=ht_;
     computeSize();
   }
}

void MSWidgetOutput::shadowThickness(int st_) 
{
  if (st_!=shadowThickness())
   {
     _shadowThickness=st_;
     computeSize();
   }
}

void MSWidgetOutput::topShadowOffset(int offset_) 
{ 
  if (topShadowOffset()!=offset_) 
   { 
     _topShadowOffset=offset_; 
     if (canDraw()==MSTrue) redraw();
   } 
}

void MSWidgetOutput::highlight(void)
{
  _highlighted=MSTrue;
  drawHighlight();
}

void MSWidgetOutput::unHighlight(void)
{
  _highlighted=MSFalse;
  undrawHighlight();
}

void MSWidgetOutput::drawHighlight(void)
{ if (canDraw()==MSTrue) drawHighlightBorder(highlightThickness()); }
void MSWidgetOutput::undrawHighlight(void)
{ if (canDraw()==MSTrue) undrawHighlightBorder(highlightThickness()); }

void MSWidgetOutput::drawShadow(void)
{ drawShadow(shadowStyle()); }
void MSWidgetOutput::undrawShadow(void)
{ undrawBevel(); }
void MSWidgetOutput::drawRaised(void)
{ drawShadow(MSRaised); }
void MSWidgetOutput::drawSunken(void)
{ drawShadow(MSSunken); }
void MSWidgetOutput::drawEtchedIn(void)
{ drawShadow(MSEtchedIn); }
void MSWidgetOutput::drawEtchedOut(void)
{ drawShadow(MSEtchedOut); }

void MSWidgetOutput::drawShadow(MSShadowStyle style_)
{
  if (canDraw()==MSTrue&&shadowThickness()>0)
   {
     int offset=highlightThickness();
     MSRect aRect(xDrawingOffset()+offset,
                  yDrawingOffset()+topShadowOffset()+offset,
                  width()-2*offset,
                  height()-topShadowOffset()-2*offset);
     drawBevel(windowForDrawing(),aRect,style_,shadowThickness());                 
   }
}

void MSWidgetOutput::shadowStyle(MSShadowStyle style_)
{
  if (shadowStyle()!=style_)
   {
     _shadowStyle=style_;
     if (canDraw()==MSTrue) drawShadow();
   }
}

void MSWidgetOutput::updateBackground(unsigned long)
{
  shadow().color(background());
  if (canDraw()==MSTrue)
   {
     XFillRectangle(display(),windowForDrawing(),backgroundShadowGC(),
		    xDrawingOffset(),yDrawingOffset(),width(),height());  
     drawShadow();
     if (highlighted()==MSTrue) drawHighlight();
   }
}

void MSWidgetOutput::clear(void)
{
  if (canDraw()==MSTrue)
   {
     int offset=highlightThickness()+shadowThickness();
     XFillRectangle(display(),windowForDrawing(),backgroundShadowGC(),
		    xDrawingOffset()+offset,yDrawingOffset()+offset,
                    width()-2*offset,height()-2*offset);  
   }
}

void MSWidgetOutput::drawBackground(void)
{
  if (canDraw()==MSTrue)
   {
     int offset=highlightThickness()+shadowThickness();
     XFillRectangle(display(),windowForDrawing(),backgroundShadowGC(),
		    xDrawingOffset()+offset,yDrawingOffset()+offset,
		    width()-2*offset,height()-2*offset);
   }
}

void MSWidgetOutput::undrawBevel(void)
{
  if (canDraw()==MSTrue)
   {
     int offset=highlightThickness();
     MSRect aRect(xDrawingOffset()+offset,yDrawingOffset()+offset+topShadowOffset(),
                  width()-2*offset,height()-topShadowOffset()-2*offset);
     drawBevelShadow(windowForDrawing(),aRect,
		     shadowThickness(),backgroundShadowGC(),backgroundShadowGC());
   }
}

void MSWidgetOutput::undrawBevel(const MSRect& aRect_,int thickness_)
{
  if (canDraw()==MSTrue)
   {
     drawBevelShadow(windowForDrawing(),aRect_,thickness_,
		     backgroundShadowGC(),backgroundShadowGC());
   }
} 

void MSWidgetOutput::undrawBevel(Window window_,const MSRect& aRect_,int thickness_)
{ drawBevelShadow(window_,aRect_,thickness_,backgroundShadowGC(),backgroundShadowGC()); } 

void MSWidgetOutput::drawBevel(void)
{ drawBevel(shadowThickness()); }
void MSWidgetOutput::drawBevel(int thickness_)
{
  if (canDraw()==MSTrue)
   {
     int offset=highlightThickness();
     MSRect aRect(xDrawingOffset()+offset,yDrawingOffset()+offset+topShadowOffset(),
                  width()-2*offset,height()-topShadowOffset()-2*offset);
     drawBevel(windowForDrawing(),aRect,shadowStyle(),thickness_);
   }
}

void MSWidgetOutput::drawBevel(const MSRect& aRect_,MSShadowStyle style_)
{ drawBevel(windowForDrawing(),aRect_,style_,shadowThickness());}
void MSWidgetOutput::drawBevel(const MSRect& aRect_,MSShadowStyle style_,int thickness_)
{ drawBevel(windowForDrawing(),aRect_,style_,thickness_); }
void MSWidgetOutput::drawRaised(Window window_,const MSRect& aRect_,int thickness_)
{ drawBevelShadow(window_,aRect_,thickness_,topShadowGC(),bottomShadowGC()); }
void MSWidgetOutput::drawSunken(Window window_,const MSRect& aRect_,int thickness_)
{ drawBevelShadow(window_,aRect_,thickness_,bottomShadowGC(),topShadowGC()); }

void MSWidgetOutput::drawBevel(Window window_,const MSRect& aRect_,
			       MSShadowStyle style_,int st_)
{
  if (st_>0)
   {
     GC topGC=topShadowGC();
     GC bottomGC=bottomShadowGC();
     switch (style_)
      {
      case MSRaised:    drawBevelShadow(window_,aRect_,st_,topGC,bottomGC);              break;
      case MSSunken:    drawBevelShadow(window_,aRect_,st_,bottomGC,topGC);              break;
      case MSEtchedIn:  drawEtchedShadow(window_,aRect_,MSEtchedIn,st_,bottomGC,topGC);  break;
      case MSEtchedOut: drawEtchedShadow(window_,aRect_,MSEtchedOut,st_,topGC,bottomGC); break;
      case MSFlat:      drawFlatShadow(window_,aRect_,st_,bottomGC);                     break;
      }
   }
}

// obsolete - use drawBevelShadow
void MSWidgetOutput::drawBevel(Window window_,const MSRect& aRect_,int thickness_,
			       GC topGC_,GC bottomGC_)
{ drawBevelShadow(window_,aRect_,thickness_,topGC_,bottomGC_); }

void MSWidgetOutput::drawHighlightBorder(int thickness_)
{
  drawFlatShadow(windowForDrawing(),
		 MSRect(xDrawingOffset(),yDrawingOffset()+topShadowOffset(),
                        width(),height()-topShadowOffset()),
		 thickness_,highlightGC());
}

void MSWidgetOutput::undrawHighlightBorder(int thickness_)
{
  drawFlatShadow(windowForDrawing(),
		 MSRect(xDrawingOffset(),yDrawingOffset()+topShadowOffset(),
                        width(),height()-topShadowOffset()),
		 thickness_,backgroundShadowGC());
}

void MSWidgetOutput::drawHighlightBorder(GC gc_,int x_,int y_,int w_,int h_,int thickness_)
{
  if (canDraw()==MSTrue)
   { drawFlatShadow(windowForDrawing(),MSRect(x_,y_,w_,h_),thickness_,gc_); }
}

void MSWidgetOutput::drawEtchedIn(Window window_,const MSRect& aRect_,int thickness_)
{ drawEtchedShadow(window_,aRect_,MSEtchedIn,thickness_,bottomShadowGC(),topShadowGC()); }

void MSWidgetOutput::drawEtchedOut(Window window_,const MSRect& aRect_,int thickness_)
{ drawEtchedShadow(window_,aRect_,MSEtchedOut,thickness_,topShadowGC(),bottomShadowGC()); }

void MSWidgetOutput::copyPixmap(Display* dpy_,const MSPixmap& pix_,Window dest_,GC gc_,
				int x_,int y_)
{
  copyPixmap(dpy_,pix_,dest_,gc_,0,0,pix_.width(),pix_.height(),x_,y_,x_,y_);
}

void MSWidgetOutput::copyPixmap(Display* dpy_,const MSPixmap& pix_,Window dest_,GC gc_,
				int x_src_,int y_src_,int w_,int h_, int x_dest_,int y_dest_,
				int x_clip_,int y_clip_)
{
  Pixmap clipMask=pix_.clipMask();
  if (clipMask==0) XSetClipMask(dpy_,gc_,None);
  else
   {
     XSetClipOrigin(dpy_,gc_,x_clip_,y_clip_);
     XSetClipMask(dpy_,gc_,clipMask);
   }
  if(pix_.depth()==1)
   {
     XCopyPlane(dpy_,pix_.pixmap(),dest_,gc_,x_src_,y_src_,w_,h_,x_dest_,y_dest_,1);
   }
  else
   {
     XCopyArea(dpy_,pix_.pixmap(),dest_,gc_,x_src_,y_src_,w_,h_,x_dest_,y_dest_);     
   }
}

//#############################################################################################
// set and get methods to support the application builder

void MSWidgetOutput::set(MSAttrValueList& avList_)
{
  MSWidget::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="highlightThickness")
      highlightThickness(avList_[i].value().asInt()),index<<i;
     else if (avList_[i].attribute()=="shadowThickness")
      shadowThickness(avList_[i].value().asInt()),index<<i;
     else if (avList_[i].attribute()=="highlightColor")
      highlightColor(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="shadowStyle")
      {
        shadowStyle(MSAttrValue::stringToShadowStyle(avList_[i].value()));
	index<<i;
      }
   } 
  avList_.remove(index);
}

MSAttrValueList& MSWidgetOutput::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("highlightThickness",MSString(highlightThickness()));
  avList_<<MSAttrValue("shadowThickness",MSString(shadowThickness()));  
  avList_<<MSAttrValue("shadowStyle",MSAttrValue::shadowStyleToString(shadowStyle()),
		       MSStringVector("MSEtchedIn\nMSEtchedOut\nMSFlat\nMSRaised\nMSSunken"));
  avList_<<MSAttrValue("highlightColor",_server->colorName(highlightColor()),MSAttrValue::Color);
  return MSWidget::get(avList_);
}

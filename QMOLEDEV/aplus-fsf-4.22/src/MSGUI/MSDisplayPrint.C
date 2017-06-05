///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <time.h>
#include <pwd.h>
#include <math.h>
#include <unistd.h>
#include <MSGUI/MSWidgetCommon.H>
#include <MSGUI/MSDisplayPrint.H>
#include <MSTypes/MSMessageLog.H>
#if HAVE_IOMANIP
#include <iomanip>
#else
#include <iomanip.h>
#endif

extern const double PSFactor;
extern const int MSPageSizeXTable[];
extern const int MSPageSizeYTable[];
extern MSString applicationVersionString(void);

static const char *DefaultFilename="print.ps";
  
MSDisplayPrint::MSDisplayPrint(void) 
{ 
  _special=MSFalse;		
  _defaultFontID=0;
  fileName(DefaultFilename);
  init();
}

void MSDisplayPrint::init(void)
{
  _owner 	   =0; 
  _clipCount	   =0;
  _activeClip	   =0;
  _activeDash	   =0;
  _updateDash      =MSFalse;
  MSPostScript::init();
}

MSDisplayPrint::~MSDisplayPrint(void) 
{
  deleteAllClipGC();
  deleteAllDashGC();
}

void MSDisplayPrint::originInc(MSWidget *widget_)
{
  x_org(x_org()+widget_->x_origin());
  y_org(y_org()+widget_->y_origin());
  if (outputMode()!=PPM) setClipWindow(widget_->width(),widget_->height()); 
}

void MSDisplayPrint::originDec(MSWidget *widget_) 
{
  x_org(x_org()-widget_->x_origin());
  y_org(y_org()-widget_->y_origin());
  if (outputMode()!=PPM) unsetClipWindow(); 
}

void MSDisplayPrint::printInit(MSWidget *owner_)
{
  _owner=owner_;
  whitePixel(owner()->server()->pixel("white"));
  blackPixel(owner()->server()->pixel("black"));
  if (defaultFontID()==0) defaultFont(owner()->server()->fontID(defaultFontString().string()));
  x_org(0);
  y_org(0);
}

void MSDisplayPrint::printClear(void)
{
  if (activeClip()!=0) unsetClip();
  deleteAllClipGC();
  deleteAllDashGC();
  init();
}

MSBoolean MSDisplayPrint::printOpen(MSWidget *owner_)
{
  MSBoolean status;
  printInit(owner_);
  if ((status=MSPostScript::printOpen())==MSTrue) 
   {
     if (outputMode()==PPM)
      {
        _ppmPixmap=new MSPixmap(owner()->server(),"MSGeneral",owner()->width(),
                                owner()->height(),BlackPixelOfScreen(owner()->screen()),
                                WhitePixelOfScreen(owner()->screen()));
      }
     else
      {
        if (disclaimer().style()>NoDisclaimer) disclaimer().computeSize();
        computePrintScale();
        printProlog();
        printSetup();
        printScale();
      }
   }
  return status;
}

MSBoolean MSDisplayPrint::printOpen(void)
{return MSPostScript::printOpen();}

MSBoolean MSDisplayPrint::printClose(void)
{
  MSBoolean status=MSTrue;
  if (outputMode()==PPM)
   {
     printPixmap();
     pout.close();
   }
  else 
   {
     printClear();
     status=MSPostScript::printClose();
   }
  return status;
}

void MSDisplayPrint::printPixmap(void)
{
  if (outputMode()==PPM&&_ppmPixmap!=0)
   {
     struct tm *tp;
     struct timeval tv;
     struct passwd *pwd;
     gettimeofday(&tv,NULL);
     const time_t *tmp=(const time_t*)&tv.tv_sec;
#if defined(MS_THREAD_SAFE_FUNCTIONS)
     //see comment in MSDefines.H on mt-safe system function usage.
     struct tm tmStruct;
     struct passwd pwdStruct;
     char charBuf[1024];
#endif
     tp=MS_LOCALTIME(tmp,&tmStruct);
     int k=0,count=0;
     int MaxBufSize=(int)XMaxRequestSize(owner()->display());
     int bytes=4;
     int headerSize=8;
     int sizePerColor=16;
     MaxBufSize=(MaxBufSize*bytes-headerSize)/sizePerColor;
     XImage *image=XGetImage(owner()->display(),ppmPixmap(),0,0,owner()->width(),
                             owner()->height(),AllPlanes,ZPixmap);
     int n=image->width*image->height;
     int bufSize=n>MaxBufSize?MaxBufSize:n;
     XColor *rgb=new XColor[bufSize];
     pout<<"P3"<<endl;
     pout<<"# Creator: ";
#if defined(MS_WINDOWS)
     pout<<"winnt";
#else
     MS_GETPWUID(geteuid(),&pwdStruct,charBuf,1024,pwd);
     pout<<((pwd!=0)?pwd->pw_name:"");
#endif
     pout<<" - "<<"MStk Release "<<applicationVersionString()<<endl;
     pout<<"# CreationDate: "<<MS_ASCTIME(tp,charBuf,1024);
     pout<<image->width<<" "<<image->height<<endl;
     pout<<"255";
     pout.fill('0');
     for (int y=0;y<image->height;y++) for (int x=0;x<image->width;x++)
      {
        rgb[k++].pixel=XGetPixel(image,x,y);
        if (k>=bufSize)
         {
           XFlush(owner()->display());
           XQueryColors(owner()->display(),owner()->server()->colormap(),rgb,k);
           for (unsigned i=0;i<k;i++,count++)
            {
              if (count%5==0) pout<<endl; else pout<<"  ";
              pout<<setw(3)<<(rgb[i].red>>8);
              pout<< " ";
              pout<<setw(3)<<(rgb[i].green>>8);
              pout<<" ";
              pout<<setw(3)<<(rgb[i].blue>>8);
            }
           k=0;
         }
      }
     if (k>0)
      {
        XQueryColors(owner()->display(),owner()->server()->colormap(),rgb,k);
        for (unsigned i=0;i<k;i++,count++)
         {
           if (count%5==0) pout<<endl; else pout<<"  ";
           pout<<setw(3)<<(rgb[i].red>>8);
           pout<< " ";
           pout<<setw(3)<<(rgb[i].green>>8);
           pout<<" ";
           pout<<setw(3)<<(rgb[i].blue>>8);
         }
      }
     pout<<endl;
     XDestroyImage(image);
     delete _ppmPixmap;
     delete [] rgb;
     _ppmPixmap=0;
   }
}

void MSDisplayPrint::printScale(void)
{
  pout<<"%%Page:1 1"<< endl;
  pout<<"12/"<<defaultFontString()<<" font"<< endl;
  pout<<"gs 1 sg 1 w n"<< endl;
  pout<<x_translate();
  pout<<" ";
  pout<<y_translate();
  pout<<" ";
  pout<<"translate";
  if (outputMode()!=EPS)
   {
     pout<<" ";
     pout<<x_printScale();
     pout<<" ";
     pout<<y_printScale();
     pout<<" ";
     pout<<"scale";
   }
  if (pageOrientation()==Landscape) pout<<" 90 rotate";
  pout<< endl;
  setClipWindow(owner()->width(),owner()->height());
}

void MSDisplayPrint::computePrintScale(void)
{
  double	x_range;
  double 	y_range;
  int		x_trans;
  int		y_trans;
  int 		psHeight,psWidth;
  int 		pagesize=pageSize()-Letter;
  int 		margin;
  int 		disOffset=disclaimer().height()+disclaimer().topPixel()+disclaimer().bottomPixel();
  
  PageOrientation orient=disclaimer().orientation()==1?pageOrientation():disclaimer().orientation();

  x_range=MSPageSizeXTable[pagesize]-leftPixel()-rightPixel();
  y_range=MSPageSizeYTable[pagesize]-topPixel()-bottomPixel();

  x_printScale(PSFactor);
  y_printScale(PSFactor);
  psWidth=(int)(PSFactor*owner()->width());
  psHeight=(int)(PSFactor*owner()->height());

  if (pageOrientation()==Landscape)
   {
     x_trans=(int)((MSPageSizeXTable[pagesize]+psHeight)/2);
     y_trans=(int)((MSPageSizeYTable[pagesize]-psWidth)/2);
   }
  else
   {
     x_trans=(MSPageSizeXTable[pagesize]-psWidth)/2;
     y_trans=(MSPageSizeYTable[pagesize]-psHeight)/2;
   }
  if (outputMode()==EPS)
   {
     if (pageOrientation()==Landscape)
      {
	bwidth(owner()->height());
	bheight(owner()->width());
	x_translate(bwidth());
      }
     else
      {
	bheight(owner()->height());
	bwidth(owner()->width());
	x_translate(0);
      }
     y_translate(0);
     disclaimer().height(0);
   }     
  else
   {
     if (pageLayout()&MSLeft)
      {
	if (pageOrientation()==Landscape)
	 {
	   if (disOffset>0&&orient==Portrait) y_trans=disOffset;
	   else y_trans=leftPixel();
	   x_trans=(int)((MSPageSizeXTable[pagesize]+psHeight)/2);
	 }
	else x_trans=leftPixel();
      }
     if (MSRight&pageLayout())
      {
	if (pageOrientation()==Landscape) 
	 {
	   y_trans=MSPageSizeYTable[pagesize]-rightPixel()-psWidth;
	   x_trans=(int)((MSPageSizeXTable[pagesize]+psHeight)/2);
	 }
	else x_trans=MSPageSizeXTable[pagesize]-rightPixel()-psWidth;
      }
     if (MSTop&pageLayout())
      {
	if (pageOrientation()==Landscape) 
	 {
	   x_trans=topPixel()+psHeight;
	 }
	else y_trans=MSPageSizeYTable[pagesize]-topPixel()-psHeight;
      }
     if (MSBottom&pageLayout())
      {
	if (pageOrientation()==Landscape)
	 { 
	   if (disOffset>0&&orient==Landscape) margin=disOffset;
	   else margin=bottomPixel();
	   x_trans=MSPageSizeXTable[pagesize]-margin;
	 }
	else y_trans=bottomPixel();
      }
     if (pageLayout()==0)
      {
	y_trans=bottomPixel();
	if (pageOrientation()==Landscape)
	 {
	   x_printScale(x_range/owner()->height());
	   y_printScale(y_range/owner()->width());
	   x_trans=MSPageSizeXTable[pagesize]-bottomPixel();
	 }
	else 
	 {
	   x_printScale(x_range/owner()->width());
	   y_printScale(y_range/owner()->height());
	   x_trans=bottomPixel();
	 }
      }
   }
  if (pageOrientation()==Landscape)
   {
     if (disOffset>0&&orient==Landscape)
      {
	margin=disOffset;
	x_range=MSPageSizeXTable[pagesize]-leftPixel()-margin;
      }
     else margin=bottomPixel();
     if (x_trans<topPixel()||x_trans>MSPageSizeXTable[pagesize]-margin)
      {
	x_trans=psHeight<x_range?topPixel()+psHeight:MSPageSizeXTable[pagesize]-margin;
	x_printScale(x_range/owner()->height());
      } 
     if (disOffset>0&&orient==Portrait) 
      {
	margin=disOffset;
	y_range=MSPageSizeYTable[pagesize]-topPixel()-margin;
      }
     else margin=leftPixel();
     if (y_trans<margin||y_trans+psWidth>MSPageSizeYTable[pagesize]-rightPixel())
      {
	y_printScale(y_range/owner()->width());
	y_trans=margin;
      } 
   }
  else
   {
     if (x_trans<leftPixel()||x_trans+psWidth>MSPageSizeXTable[pagesize]-rightPixel())
      {
	x_trans=leftPixel();
	x_printScale(x_range/owner()->width());
      }
     if (disOffset>0&&orient==Portrait) 
      {
	margin=disOffset;
	y_range=MSPageSizeYTable[pagesize]-topPixel()-margin;
      }
     else margin=bottomPixel();
     if (y_trans<margin||y_trans+psHeight>MSPageSizeYTable[pagesize]-topPixel())
      {
	{
	  y_printScale(y_range/owner()->height());
	  y_trans=margin;
	}
      }
   }
  if (outputMode()!=EPS)
   {
     x_translate(x_trans);
     y_translate(y_trans);
   }
}

void MSDisplayPrint::updateForeground(int)
{
  if (fgColor()!=gcValues().foreground)
   {
     fgRGB().pixel=gcValues().foreground;
     XQueryColor(owner()->display(),owner()->server()->colormap(),&fgRGB());
     fgColor(gcValues().foreground);
     bgColor(0);
   }
}

void MSDisplayPrint::updateBackground(void)
{
  if (bgColor()==0||bgColor()!=gcValues().background)
   {
     bgRGB().pixel=gcValues().background;
     XQueryColor(owner()->display(),owner()->server()->colormap(),&bgRGB());
     bgColor(gcValues().background);
     fgColor(0);
   }
}

Font MSDisplayPrint::defaultFont(void) const
{
  return defaultFontID()==0?(owner()==0?MSDisplayServer::defaultDisplayServer()->fontID(fontString()):
         owner()->server()->fontID(fontString())):defaultFontID(); 
}

void MSDisplayPrint::defaultFont(Font font_)
{
  const char *tempStr,*fontStr;
  MSDisplayServer *printServer=owner()==0?MSDisplayServer::defaultDisplayServer():owner()->server();
  
  if (font_!=_defaultFontID)
   {
     tempStr=formatFontString(printServer->fontName(font_));
     if (tempStr!=0)
      {
	fontStr=(char *)fontHashTable()->lookup(tempStr);
	if (fontStr!=0) 
	 {
	   defaultFontName(fontStr);
	   defaultFontID(font_);
	 }
	else
	 {
	   MSMessageLog::errorMessage("Error: invalid default font selected-using %s\n",defaultFontString().string());
	 }
      }   
   }
}

void MSDisplayPrint::updateFont(void)
{
  const char *tempStr,*fontStr;
  
  if (gcValues().font!=0)
   {
     tempStr=formatFontString(owner()->server()->fontName(gcValues().font));
     if (tempStr!=0)
      {
	fontStr=(char *)fontHashTable()->lookup(tempStr);
	if (fontStr!=0) 
	 {
	   fontString(fontStr);
	   fontID(gcValues().font);
	 }
	else 
	 {
	   fontString((char*)defaultFontString().string());
	   fontID(owner()->server()->fontID(defaultFontString().string()));
	 }
	_fontInfo=(XFontStruct *)_owner->server()->fontStruct(fontID());
      }
     else _fontInfo=(XFontStruct *)_owner->server()->fontStruct(defaultFontID());
   }
}     

void MSDisplayPrint::setAttributes(void)
{
  MSBoolean	update=MSFalse;
  
  if (setForeground()==MSTrue) update=MSTrue;
  if (setLineAttributes()==MSTrue) update=MSTrue;	
  if (update==MSTrue) pout<<endl;
}

MSBoolean MSDisplayPrint::setLineAttributes(void)
{
  MSBoolean	status=MSFalse;
  
  if (lineWidth()!=gcValues().line_width)
   {
     lineWidth(gcValues().line_width);
     pout<< ((lineWidth()>2)?lineWidth()-1:lineWidth());
     pout<< " ";
     pout<< "w";
     pout<< " ";
     status=MSTrue;
   }
  if (lineStyle()!=gcValues().line_style||updateDash()==MSTrue)
   {
     dashOffset(gcValues().dash_offset);
     switch (gcValues().line_style)
      { 
      case LineOnOffDash:
      case LineDoubleDash:
	if (updateDash()==MSTrue&&activeDash()!=0)
	 {
	   pout<<"[ ";
	   for (int i=0; i<activeDash()->dashCt(); i++) pout<<activeDash()->dash(i)<<" ";
	   pout<<"]";
	   pout<<" ";
	   pout<<dashOffset();
	   pout<<" ";
	   pout<<"d";
	   status=MSTrue;
	 }
	break;

      case LineSolid:
      default:
	pout<< "[] 0 d";
	status=MSTrue;
      }
     pout<< " ";
     lineStyle(gcValues().line_style);
   }
  if (capStyle()!=gcValues().cap_style)
   {
     capStyle(gcValues().cap_style);
     pout<< capStyle()-(capStyle()!=0?1:0);
     pout<< " ";
     pout<< "lc";
     pout<< " ";
     status=MSTrue;
   }
  if (joinStyle()!=gcValues().join_style)
   {
     joinStyle(gcValues().join_style);
     pout<< joinStyle();
     pout<< " ";
     pout<< "lj";
     status=MSTrue;
   }
  return status;
}

void MSDisplayPrint::setFontAttributes(void)
{
  if (fontID()==0||fontID()!=gcValues().font)
   {
     updateFont();
     pout<< fontHeight()*((y_printScale()==PSFactor)?y_printScale():1);
     pout<< "/";
     pout<< fontString();
     pout<< " ";
     pout<< "font";
     pout<< endl;
   }
}

void MSDisplayPrint::setClipMask(void)
{
}

void MSDisplayPrint::setFillStyle(void)
{
}

void MSDisplayPrint::setFillRule(void)
{
}

void MSDisplayPrint::setArcMode(void)
{
}

void MSDisplayPrint::printArc(GC gc_,int x_,int y_,int w_,int h_,int angle1_,int angle2_)
{
  updateGC(gc_);
  setAttributes();
  printArc(x_org()+x_,owner()->height()-y_org()-y_,w_,h_,angle1_,angle2_,gcValues().arc_mode,Stroke);
}

void MSDisplayPrint::printArcs(GC gc_,XArc *arcs_,int n_)
{
  updateGC(gc_);
  setAttributes();
  for (int i=0; i<n_; i++)
   {
     printArc(x_org()+(int)arcs_[i].x,owner()->height()-y_org()-(int)arcs_[i].y,(int)arcs_[i].width,
	      (int)arcs_[i].height,(int)arcs_[i].angle1,(int)arcs_[i].angle2,gcValues().arc_mode,Stroke);
   }
}

void MSDisplayPrint::printLine(GC gc_,int x1_,int y1_,int x2_,int y2_)
{
  updateGC(gc_);
  setAttributes();
  printLine(x_org()+x1_,owner()->height()-y_org()-y1_,x_org()+x2_,owner()->height()-y_org()-y2_);
}

void MSDisplayPrint::printLines(GC gc_,XPoint *point_,int n_,int mode_)
{
  int offset=6;

  if (n_<=0) return;
  updateGC(gc_);
  setAttributes();
  if (mode_==CoordModeOrigin)
   {
     int count,j=n_-1;
     if (n_>offset+2) j=offset;
     pout<< x_org()+point_[j].x;
     pout<< " ";
     pout<< owner()->height()-y_org()-point_[j].y;
     pout<< " ";
     pout<< "M";
     pout<< " ";
     for (int i=count=0;i<n_;i++)
      {
        pout<< x_org()+point_[i].x;
        pout<< " ";
        pout<< owner()->height()-y_org()-point_[i].y;
        pout<< " ";
        count++;
        if (i==j-1&&j!=n_-1)
         {
           pout<< count;
           pout<< " ";
           pout<< "DL ";
           pout<< " ";
           pout<< "st";
           pout<< endl;
           j=(n_>j+offset+2)?j+offset:n_-1;
           pout<< x_org()+point_[j].x;
           pout<< " ";
           pout<< owner()->height()-y_org()-point_[j].y;
           pout<< " ";
           pout<< "M";
           pout<< " ";
           if (gcValues().line_style==LineSolid) i--;
           count=0;
         }
      }
     pout<< count;
     pout<< " ";
     pout<< "DL";
     pout<< " ";
     pout<< "st";
     pout<< endl;
   }
  else
   {
     pout<< x_org()+point_[0].x;
     pout<< " ";
     pout<< owner()->height()-y_org()-point_[0].y;
     pout<< " ";
     pout<< "M";
     pout<< " ";
     for (int i=n_-1;i>0;i--)
      {
        pout<< point_[i].x;
        pout<< " ";
        pout<< -point_[i].y;
        pout<< " ";
      }
     pout<< n_-1;
     pout<< " ";
     pout<< "D";
     pout<< " ";
     pout<< "st";
     pout<< endl;
    }
}

void MSDisplayPrint::printSegments(GC gc_,XSegment *segment_,int n_)
{
  updateGC(gc_);
  setAttributes();
  for (int i=0; i<n_; i++)
   {
     printLine(x_org()+segment_[i].x1,owner()->height()-y_org()-segment_[i].y1,
	       x_org()+segment_[i].x2,owner()->height()-y_org()-segment_[i].y2);
   }
}

void MSDisplayPrint::printPoint(GC gc_,int,int)
{
  updateGC(gc_);
}

void MSDisplayPrint::printPoints(GC gc_,XPoint*,int,int)
{
  updateGC(gc_);
}

void MSDisplayPrint::printRectangle(GC gc_,int x_,int y_,int w_,int h_)
{
  updateGC(gc_);
  setAttributes();
  printRectangle(x_org()+x_,owner()->height()-y_org()-y_,w_,h_);
  pout<< "st";
  pout<< endl;
}

void MSDisplayPrint::printRectangles(GC gc_,XRectangle *rects_,int n_)
{
  updateGC(gc_);
  setAttributes();
  for (int i=0; i<n_; i++)
   {
     printRectangle(x_org()+(int)rects_[i].x,owner()->height()-y_org()-(int)rects_[i].y,
		    (int)rects_[i].width,(int)rects_[i].height);
     pout<< "st";
     pout<< endl;
   }
}

void MSDisplayPrint::printString(GC gc_,int x_,int y_,const char *string_,int n_)
{
  if (n_!=0)
   {
     updateGC(gc_);
     setForeground();
     setFontAttributes();
     printString(x_org()+x_,owner()->height()-y_org()-y_,string_,n_);
   }
}

void MSDisplayPrint::printImageString(GC gc_,int x_,int y_,const char *string_,int n_)
{
  if (n_!=0)
   {
     updateGC(gc_);
     setFontAttributes();
     if (printMode()==Colorfg) gcValues().background=whitePixel();
     if (printMode()==Color||printMode()==Colorfg) setBackground(-1);
     else setBackground();
     printRectangle(x_org()+x_,owner()->height()-y_org()-y_+fontInfo()->ascent,XTextWidth(fontInfo(),string_,n_),
		    fontHeight()
		   );
     pout<< "f";
     pout<< endl;
     setForeground();
     pout<< endl;
     printString(x_org()+x_,owner()->height()-y_org()-y_,string_,n_);
   }
}

void MSDisplayPrint::printString(int x_,int y_,const char *string_,int n_)
{
  MSString aString(string_,n_);
  aString.change("\\","\\\\").change("(","\\(").change(")","\\)").change("\n","");
  
  if (aString.length()>0)
   {
     pout<< x_;
     pout<< " ";
     pout<< y_;
     pout<< " ";
     pout<< "M";
     pout<< " ";
     pout<< XTextWidth(fontInfo(),string_,n_);
     pout<< " ";
     pout<< "(";
     pout<< aString;
     pout<< ")";
     pout<< " ";
     pout<< "ST";
     pout<< endl;
   } 
}

void MSDisplayPrint::printFillArc(GC gc_,int x_,int y_,int w_,int h_,int angle1_,int angle2_)
{
  updateGC(gc_);
  setBackground(printMode()!=Color?1:0);
  printArc(x_org()+x_,owner()->height()-y_org()-y_,w_,h_,angle1_,angle2_,gcValues().arc_mode,Fill);
}

void MSDisplayPrint::printFillArcs(GC gc_,XArc *arcs_,int n_)
{
  updateGC(gc_);
  setBackground(printMode()!=Color?1:0);
  for (int i=0; i<n_; i++)
   {
     printArc(x_org()+(int)arcs_[i].x,owner()->height()-y_org()-(int)arcs_[i].y,(int)arcs_[i].width,
	      (int)arcs_[i].height,(int)arcs_[i].angle1,(int)arcs_[i].angle2,gcValues().arc_mode,Fill);
   }
}

void MSDisplayPrint::printFillPolygon(GC gc_,XPoint *points_,int n_,int,int mode_,MSBoolean fill_)
{
 /*
   *            1 *-------------------------------* 2
   *              |   n-2                         |
   *              |   *---------------------------* 3
   *              |   |
   *              |   |
   *       origin *---* n-1
   *
   *   xy deltas are pushed on stack from last point backwards to origin:
   *
   *   origin(x,y) M delta(n-1,n-2)...delta(1,2),delta(origin-1) n-1 D P f
   *
   *   where n=point count
   *
   */  
  if (n_>1)
   {
     int i,j,last_x,last_y;
     
     updateGC(gc_);
     if (printMode()==Colorfg&&fill_!=MSTrue) gcValues().foreground=whitePixel();
     setBackground(printMode()!=Color&&fill_==MSTrue?1:0);
     j=(points_[n_-1].x==points_[0].x && points_[n_-1].y==points_[0].y)?2:1;
     last_x=(int)points_[n_-j].x;
     last_y=(int)points_[n_-j].y;
     pout<< "n";
     pout<< " ";
     pout<< x_org()+(int)points_[0].x;
     pout<< " ";
     pout<< owner()->height()-y_org()-(int)points_[0].y;
     pout<< " ";
     pout<< "M";
     pout<< " ";
     for (i=n_-j-1;i>-1;i--)
      {
        if (mode_==CoordModeOrigin)
         {
           pout<< last_x-(int)points_[i].x;
           pout<< " ";
           pout<< (int)points_[i].y-last_y;
           pout<< " ";
           last_x=(int)points_[i].x;
           last_y=(int)points_[i].y;
         }
        else
         {
           pout<< (int)points_[i+1].x;
           pout<< " ";
           pout<< -(int)points_[i+1].y;
           pout<< " ";
         }
      }
     pout<< n_-j;
     pout<< " ";
     pout<< "D";
     pout<< " ";
     pout<< "P";
     pout<< " ";
     pout<< "f";
     pout<< endl;
   }
}

void MSDisplayPrint::printFillRectangle(GC gc_,int x_,int y_,int w_,int h_,MSBoolean fill_)
{
  updateGC(gc_);
  if (printMode()==Colorfg&&fill_!=MSTrue) gcValues().foreground=whitePixel();
  setBackground(printMode()!=Color&&fill_==MSTrue?1:0);
  printRectangle(x_org()+x_,owner()->height()-y_org()-y_,w_,h_);
  pout<< "f";
  pout<< endl;
}

void MSDisplayPrint::printFillRectangles(GC gc_,XRectangle *rects_,int n_,MSBoolean fill_)
{
  updateGC(gc_);
  if (printMode()==Colorfg&&fill_!=MSTrue) gcValues().foreground=whitePixel();
  setBackground(printMode()!=Color&&fill_==MSTrue?1:0);
  for (int i=0; i<n_; i++)
   {
     printRectangle(x_org()+(int)rects_[i].x,owner()->height()-y_org()-(int)rects_[i].y,
		    (int)rects_[i].width,(int)rects_[i].height);
     pout<< "f";
     pout<< endl;
   }
}

void MSDisplayPrint::printSetClipRectangles(GC gc_,int x_,int y_,XRectangle *rect_,int n_,int)
{
  MSClipArea  *clip=updateClipGC(gc_);
  
  clip->clipNumber(clipCount());
  if (clip!=0)
   {
     for (int i=0; i<n_; i++)
      {
	pout<< "/clip";
	pout<< clipCount();
	pout<< " ";
	pout<< "{";
	setClipRectangle(x_org()+x_+rect_[i].x,owner()->height()-y_org()-(y_+rect_[i].y),rect_[i].width,rect_[i].height);
	if (i==n_-1) 
	 {
	   pout<< "}";
	   pout<< "def";
	   pout<< endl;
	 }
	else pout<< endl;
      }  
   }
}

void MSDisplayPrint::setClipWindow(int width_,int height_)
{
  lineWidth(-1);
  lineStyle(-1);
  if (activeClip()!=0) unsetClip();
  pout<< "gs";
  pout<< " ";
  setClipRectangle(x_org(),owner()->height()-y_org(),width_,height_);
  pout<< endl;
}

void MSDisplayPrint::unsetClipWindow(void)
{
  if (printMode()==Reverse) gscale(1); else gscale(0);
  pout<< "gr";
  pout<< " ";
  pout<< "%unset clip window";
  pout<< endl;
  pout<< gscale();
  pout<< " ";
  pout<< "sg";
  pout<< endl;
  fgColor(LONG_MAX);
  bgColor(LONG_MAX);
  fontID(0);
}

void MSDisplayPrint::printSetDashes(GC gc_,const char *dashList_,int n_)
{
  MSNodeItem *hp=_dashGCListHead.address();
  MSNodeItem *np=hp;
  MSDashLine *dash;
  
  while ((np=np->next())!=hp)
   {
     dash=(MSDashLine *)np->data();
     if (dash->dashID()==gc_)
      {
	if (dash==activeDash()) activeDash(0);
	delete dash;
	delete np;
	np=hp;
      }
   }
  dash=new MSDashLine(gc_,dashList_,n_);
  np=new MSNodeItem((void *)dash);
  np->insert(hp->next());
}   

MSDashLine *MSDisplayPrint::findDashGC(GC gc_)
{
  MSNodeItem *hp=_dashGCListHead.address();
  MSNodeItem *np=hp;
  MSDashLine *temp,*dash=0;
  
  while ((np=np->next())!=hp)
   {
     temp=(MSDashLine *)np->data();
     if (temp->dashID()==gc_)
      {
	dash=temp;
	break;
      }
   }
  return dash;
}   

void MSDisplayPrint::deleteAllDashGC(void)
{
  MSNodeItem *hp=_dashGCListHead.address();
  MSNodeItem *np=hp;
  
  while ((np=np->next())!=hp)
   {
     MSDashLine *dash=(MSDashLine *)np->data();
     if (dash!=0) delete dash;
     delete np;
     np=hp;
   }
}   

void MSDisplayPrint::updateGC(GC gc_)
{ 
  unsigned long mask;
  MSClipArea   *clip;
  MSDashLine   *dash;
  
  mask=GCLineWidth|GCLineStyle|GCForeground|GCBackground|GCFont|
  GCCapStyle|GCJoinStyle|GCFillStyle|GCFillRule|GCDashOffset|GCArcMode;
  
  XGetGCValues(owner()->display(),gc_,mask,&gcValues()); 
  
  if ((dash=findDashGC(gc_))!=0)
   {
     if (dash!=activeDash()) activeDash(dash);
     else updateDash(MSFalse);
   }
  
  if ((clip=findClipGC(gc_))!=0)
   {
     if (clip!=activeClip())
      { 
	if (activeClip()!=0) unsetClip();
	setClip(clip);
	lineWidth(-1);
      }
   }
  else if (activeClip()!=0) unsetClip();
}

void MSDisplayPrint::unsetClip(void)
{
  if (printMode()==Reverse) gscale(0); else gscale(1);
  pout<< "gr";
  pout<< " ";
  pout<< "%clip";
  pout<< activeClip()->clipNumber();
  pout<< endl;
  pout<< gscale();
  pout<< " ";
  pout<< "sg";
  pout<< endl;
  fontID(0);
  activeClip(0);
  fgColor(LONG_MAX);
  bgColor(LONG_MAX);
}

void MSDisplayPrint::setClip(MSClipArea *clip_)
{
  activeClip(clip_);
  pout<< "gs";
  pout<< " ";
  pout<< "clip";
  pout<< clip_->clipNumber();
  pout<< endl;
}

MSClipArea *MSDisplayPrint::updateClipGC(GC gc_)
{
  MSNodeItem *hp=_clipGCListHead.address();
  MSNodeItem *np=hp;
  MSClipArea *clip;
  
  while ((np=np->next())!=hp)
   {
     clip=(MSClipArea *)np->data();
     if (clip!=0&&clip->clipID()==gc_)
      {
	if (clip==activeClip()) unsetClip();
	delete clip;
	delete np;
	np=hp;
      }
   }
  clip=new MSClipArea(gc_);
  np=new MSNodeItem((void *)clip);
  np->insert(hp->next());
  _clipCount++;
  return clip;
}   

MSClipArea *MSDisplayPrint::findClipGC(GC gc_)
{
  MSNodeItem *hp=_clipGCListHead.address();
  MSNodeItem *np=hp;
  MSClipArea *tempClip,*clip=0;
  
  while ((np=np->next())!=hp)
   {
     tempClip=(MSClipArea *)np->data();
     if (tempClip->clipID()==gc_)
      {
	clip=tempClip;
	break;
      }
   }
  return clip;
}   

void MSDisplayPrint::deleteAllClipGC(void)
{
  MSNodeItem *hp=_clipGCListHead.address();
  MSNodeItem *np=hp;
  
  while ((np=np->next())!=hp)
   {
     MSClipArea *clip=(MSClipArea *)np->data();
     if (clip!=0) delete clip;
     delete np;
     np=hp;
   }
}   

void MSDisplayPrint::printFreeGC(GC gc_)
{
  MSNodeItem *hp=_clipGCListHead.address();
  MSNodeItem *np=hp;

  while ((np=np->next())!=hp)
   {
     MSClipArea *clip=(MSClipArea *)np->data();
     if (clip!=0&&clip->clipID()==gc_)
      {
	if (clip==activeClip()) unsetClip();
	delete clip;
	delete np;
	np=hp;
      }
   }
  np=hp=_dashGCListHead.address();
  while ((np=np->next())!=hp)
   {
     MSDashLine *dash=(MSDashLine *)np->data();
     if (dash!=0&&dash->dashID()==gc_)
      {
	if (dash==activeDash()) activeDash(0);
	delete dash;
	delete np;
	np=hp;
      }
   }
}   

void MSDisplayPrint::comment(const char *x_)
{
  pout<<"% "<<x_<<endl;
}

void MSDisplayPrint::activeDash(MSDashLine *x_)
{
  if (x_!=_activeDash) _updateDash=MSTrue; 
  _activeDash=x_;
}

void MSDisplayPrint::special(MSBoolean mode_)
{
  if (special()!=mode_)
   {
     if (mode_==MSTrue) pout<<"0 w [1 1] 0 d "<<endl;
     else pout<<lineWidth()<<" w [] 0 d "<<endl;
     _special=mode_;
   }
}

void MSDisplayPrint::printArc(int x,int y,int w,int h,int l1,int l2,int arcMode,DrawMode mode)
{MSPostScript::printArc(x,y,w,h,l1,l2,arcMode,mode);}
void MSDisplayPrint::printLine(double x1,double y1,double x2,double y2)
{MSPostScript::printLine(x1,y1,x2,y2);}
void MSDisplayPrint::printRectangle(double x,double y,double w,double h)
{MSPostScript::printRectangle(x,y,w,h);}
void MSDisplayPrint::setClipRectangle(int x,int y,int w,int h)
{MSPostScript::setClipRectangle(x,y,w,h);}


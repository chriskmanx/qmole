///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSHScale.H>

const int DefaultHSliderWidth    =30;
const int DefaultHSliderHeight   =14;
const int DefaultHScaleWidth     =200;
extern const int SliderAreaShadowThickness;

MSHScale::HSlider::HSlider(MSWidget* owner_):MSScale::Slider(owner_)
{}
MSHScale::HSlider::~HSlider(void)
{}

void MSHScale::HSlider::drawSliderEtch(void)  
{
  if (mapped()==MSTrue)
   {
     XDrawLine(display(),window(),bottomShadowGC(),width()/2-1,1,width()/2-1,height()-2);
     XDrawLine(display(),window(),topShadowGC(),width()/2,1,width()/2,height()-2);
   }
}

void MSHScale::HSlider::moveTo(int x_,int y_)
{
  MSWidget::moveTo(x_,y_);
  setValueWinXCoord(x_);
}

MSHScale::MSHScale(MSWidget *owner_,const char *title_) : 
MSScale(owner_,title_)
{ init(); }

MSHScale::MSHScale(MSWidget *owner_,const MSStringVector& title_) : 
MSScale(owner_,title_)
{ init(); }

MSHScale::MSHScale(MSWidget *owner_,MSFloat& model_,const char *title_) : 
MSScale(owner_,model_,title_)
{ init(); }

MSHScale::MSHScale(MSWidget *owner_,MSFloat& model_,const MSStringVector& title_) : 
MSScale(owner_,model_,title_)
{ init(); }

MSHScale::MSHScale(MSWidget *owner_,MSInt& model_,const char *title_) : 
MSScale(owner_,model_,title_)
{ init(); }

MSHScale::MSHScale(MSWidget *owner_,MSInt& model_,const MSStringVector& title_) : 
MSScale(owner_,model_,title_)
{ init(); }

void MSHScale::init(void)
{
  _slider=new HSlider(this);
  _valueAlignment=(unsigned long)MSTop;
  _labelAlignment=(unsigned long)MSBottom;
  sliderWidth(DefaultHSliderWidth);
  sliderHeight(DefaultHSliderHeight);
}

MSHScale::~MSHScale(void) {}

void MSHScale::computeSize(void)
{
  int minh=0,maxh=0,y,w=0,hh=0,h=0;
  int labelHeight=0;
  XFontStruct *fontInfo;
  int offset=highlightThickness()+shadowThickness();
  _titleWidth=_bottomOffset=_leftOffset=_rightOffset=_topOffset=offset;
  _mintitleWidth=_maxtitleWidth=0;
  
  if (mintitle().maxLength()>0)
   {
     fontInfo=(XFontStruct *)server()->fontStruct(mintitleFont());
     _mintitleHeight=fontInfo->ascent+fontInfo->descent;
     _mintitleHeight*=mintitle().length();
     minh=MSTop&mintitleAlignment()?mintitleHeight():MSBottom&mintitleAlignment()?
          mintitleHeight()>slider()->height()?(slider()->height()-mintitleHeight()/2):0:0;
     for (int i=0;i<mintitle().length();i++) 
      {
        w=XTextWidth(fontInfo,mintitle()[i].string(),mintitle()[i].length());
	mintitleWidth(w>mintitleWidth()?w:mintitleWidth());
      }
   }
  if (maxtitle().maxLength()>0)
   {
     fontInfo=(XFontStruct *)server()->fontStruct(maxtitleFont());
     _maxtitleHeight=fontInfo->ascent+fontInfo->descent;
     _maxtitleHeight*=maxtitle().length();
     maxh=MSTop&maxtitleAlignment()?maxtitleHeight():MSBottom&maxtitleAlignment()?
          maxtitleHeight()>slider()->height()?(slider()->height()-maxtitleHeight()/2):0:0;
     for (int i=0;i<maxtitle().length();i++) 
      {
	w=XTextWidth(fontInfo,maxtitle()[i].string(),maxtitle()[i].length());
	maxtitleWidth(w>maxtitleWidth()?w:maxtitleWidth());
      }
   }
  y=minh>maxh?minh:maxh;
  if (title().maxLength()>0)
   {
     fontInfo=(XFontStruct *)server()->fontStruct(titleFont());
     h=fontInfo->ascent+fontInfo->descent;
     h*=title().length();
     for (int i=0;i<title().length();i++) 
      {
	w=XTextWidth(fontInfo,title()[i].string(),title()[i].length());
	titleWidth(w>titleWidth()?w:titleWidth());
      }
   }
  if (subtitle().maxLength()>0)
   {
     fontInfo=(XFontStruct *)server()->fontStruct(subtitleFont());
     hh=fontInfo->ascent+fontInfo->descent;
     hh*=subtitle().length();
     for (int i=0;i<subtitle().length();i++) 
      {
	w=XTextWidth(fontInfo,subtitle()[i].string(),subtitle()[i].length());
	titleWidth(w>titleWidth()?w:titleWidth());
      }
   }
  h+=hh;
  titleHeight(h>y?h:y);
  h+=valueAlignment()==MSNone?0:valueWin()->textHeight()>y?valueWin()->textHeight():0;
  h+=slider()->height();
  h+=slider()->offset();
  if (labelAlignment()>MSNone)
   {
     fontInfo=(XFontStruct *)server()->fontStruct(labelFont());
     labelHeight=fontInfo->ascent+fontInfo->descent;
     labelHeight+=majorTickSize()>minorTickSize()?majorTickSize():minorTickSize();
   }
  h+=labelHeight;
  h+=2*offset;
  h+=2*SliderAreaShadowThickness;
  h+=MSBottom&mintitleAlignment()?mintitleHeight():MSCenter&mintitleAlignment()?0:(minh>0?minh:0);
  h+=MSBottom&maxtitleAlignment()?maxtitleHeight():MSCenter&maxtitleAlignment()?0:(maxh>0?maxh:0);
  naturalScaleSize(h);
  w=titleWidth()+2*(labelSpacing()+offset)+2*SliderAreaShadowThickness;
  resize(DefaultHScaleWidth>w?DefaultHScaleWidth:w,h);
}

void MSHScale::setSliderPosition(int x_)
{
  int x=x_<x_org()+slider()->offset()?x_org()+slider()->offset():
        x_>x_end()-slider()->width()-slider()->offset()?
	x_end()-slider()->width()-slider()->offset():x_;
  int y=y_org()+SliderAreaShadowThickness;
  if (x!=slider()->x_origin()||y!=slider()->y_origin()) slider()->moveTo(x,y);
}

void MSHScale::computeSliderAreaSize(void)
{
  int offset=valueWin()->offset()>0?0:-valueWin()->offset();
  if (labelAlignment()>MSNone)
   {
     XFontStruct *fontInfo=(XFontStruct *)server()->fontStruct(labelFont());
     MSString buffer;
     formatLabel(buffer,valueMin());
     int wmin=XTextWidth(fontInfo,buffer,buffer.length())/2;
     wmin=wmin>labelOffset()?wmin-labelOffset():0;
     formatLabel(buffer.removeAll(),valueMax());
     int wmax=XTextWidth(fontInfo,buffer,buffer.length())/2;
     wmax=wmax>labelOffset()?wmax-labelOffset():0;
     int labelWidth=wmin>wmax?wmin:wmax;
     offset=labelWidth>offset?labelWidth:offset;
   }
  offset-=slider()->offset();
  int h=slider()->height()+2*(slider()->offset());
  int xoffset=offset>mintitleWidth()?offset:mintitleWidth();
  int woffset=offset>maxtitleWidth()?offset:maxtitleWidth();
  offset=highlightThickness()+shadowThickness();
  sliderAreaRect().x(xoffset+offset+labelSpacing()+SliderAreaShadowThickness);
  sliderAreaRect().width(width()-xoffset-woffset-2*offset-2*labelSpacing()-2*SliderAreaShadowThickness);
  int ypos=offset;
  ypos+=MSBottom&valueAlignment()||valueAlignment()==MSNone?0:valueWin()->height();
  ypos+=MSBottom&titleAlignment()?0:titleHeight();
  if (height()>naturalScaleSize())
   {
     int yy=(height()-h)/2;
     ypos=ypos>yy?ypos:yy;
   }
  sliderAreaRect().y(ypos+slider()->offset());
  sliderAreaRect().height(h);
  valueWin()->y_org(MSBottom&valueAlignment()?y_end():y_org()-valueWin()->height());
}

void MSHScale::computeTickInc(void)
{
  int 		i,j;
  int 		size;
  double 	min_inc,r,tmpScale;
  XFontStruct  *fi=(XFontStruct *)server()->fontStruct(labelFont());
  int           offset=labelOffset()==0?1:slider()->width();
  int 		w=sliderAreaRect().width()-offset-slider()->offset();
  double	range=fabs(valueMax()-valueMin());
  double 	inc,tempInc;
  MSBoolean	evaluate=MSTrue;
   
  if (labelInc()<=0.0)
   {
     min_inc=outFmt().minimumNumber(*labelOut());
     if (min_inc==0) min_inc=range/w;
     tmpScale=1/min_inc;
     MSString buffer;
     formatLabel(buffer,valueMin());
     i=XTextWidth(fi,buffer,buffer.length());
     formatLabel(buffer.removeAll(),valueMax());
     j=XTextWidth(fi,buffer,buffer.length());
     i=(i=j>i?j:(int)(i*1.5))!=0?i:100;
     inc=range*i/w;
     tempInc=outFmt().snapNumber(inc,*labelOut());
     inc=tempInc;
     while(evaluate==MSTrue&&inc>0)
      {
	size=0;
	r=valueMin()-fmod(valueMin(),inc);
	while (r<=valueMin()) r+=inc;
	while (r<=valueMax())
	 {
	   formatLabel(buffer.removeAll(),r);
	   size=(i=XTextWidth(fi,buffer,buffer.length()))>size?i:size;
	   r+=inc;
	 }
	if (tmpScale*inc<1.5*size)
	 {
	   min_inc=inc;
	   inc=outFmt().snapNumber((inc*1.001),*labelOut());
	 }
	else if (tmpScale*inc>3.*size)
	 {
	   r=outFmt().snipNumber((inc*.999),*labelOut());
	   if (r>min_inc) inc=r;
	   else evaluate=MSFalse;
	 }
	else evaluate=MSFalse;
      }
     incData(inc);
   }     
  else incData(labelInc());
}

void MSHScale::computeSliderScale(void)
{
  int offset=labelOffset()==0?1:slider()->width();
  int w=sliderAreaRect().width()-offset-2*slider()->offset();
  base(valueMin());
  scale(w/(valueMax()-valueMin()));
  scale((scale()>INT_MAX/2)?INT_MAX/2:scale());
}

void MSHScale::drawSliderTitles(void)
{
  int offset=highlightThickness()+shadowThickness();
  int x,len,w,y,center=y_org()+slider()->height()/2;
  int labelHeight=0;

  GC gc=XCreateGC(display(),window(),0,0);
  
  if (mintitle().maxLength()>0) 
   {
     if (labelAlignment()>MSNone)
      {
        XFontStruct *fontInfo=(XFontStruct *)server()->fontStruct(labelFont());
        labelHeight=fontInfo->ascent+fontInfo->descent;
        labelHeight+=majorTickSize()>minorTickSize()?majorTickSize():minorTickSize();
      }
     XFontStruct *fontInfo=(XFontStruct *)server()->fontStruct(mintitleFont());
     XSetFont(display(),gc,mintitleFont());
     XSetForeground(display(),gc,mintitleForeground());
     y=MSTop&mintitleAlignment()?y_org()-mintitleHeight()-(MSTop&labelAlignment()?labelHeight:0):
       MSBottom&mintitleAlignment()?y_end()+(MSBottom&labelAlignment()?labelHeight:0):
       center-mintitleHeight()/2;
     y+=fontInfo->ascent;
     for (int i=0; i<mintitle().length(); i++) 
      {
        len=mintitle()[i].length();
        const char *cp=mintitle()[i].string();
	w=XTextWidth(fontInfo,cp,len);
	x=MSCenter&mintitleAlignment()?x_org()-w-labelSpacing():MSLeft&mintitleAlignment()?
          x_org():MSRight&mintitleAlignment()?x_org()-w:(x_org()-w/2);
	XDrawString(display(),window(),gc,fontInfo,x,y,cp,len);
	y+=fontInfo->ascent+fontInfo->descent;
      }
   }
  if (maxtitle().maxLength()!=0)
   {
     XFontStruct *fontInfo=(XFontStruct *)server()->fontStruct(maxtitleFont());
     XSetFont(display(),gc,maxtitleFont());
     XSetForeground(display(),gc,maxtitleForeground());
     y=MSTop&maxtitleAlignment()?y_org()-maxtitleHeight():MSBottom&maxtitleAlignment()?y_end():
       center-maxtitleHeight()/2;
     y+=fontInfo->ascent;
     for (int i=0; i<maxtitle().length(); i++) 
      {
	len=maxtitle()[i].length();
	const char *cp=maxtitle()[i].string();
	w=XTextWidth(fontInfo,cp,len);
	x=MSCenter&maxtitleAlignment()?x_end()+labelSpacing():MSLeft&maxtitleAlignment()?
	  x_end():MSRight&maxtitleAlignment()?x_end()-w:(x_end()-w/2);
	XDrawString(display(),window(),gc,fontInfo,x,y,cp,len);
	y+=fontInfo->ascent+fontInfo->descent;
      }
   }
  XFreeGC(display(),gc);
}

void MSHScale::drawTickLabels(void)
{
  if (labelAlignment()!=MSNone)
   {
     int               	majorTick=0,minorTick;
     int               	w;
     int               	x,yy,y=MSTop&labelAlignment()?y_org():y_end();
     int               	ct=0,sign=MSTop&labelAlignment()?-1:1;
     int                tickPositionLength;
     double            	r,s,inc;
     XSegment     	*segments;
     XFontStruct        *fontInfo=(XFontStruct *)server()->fontStruct(labelFont());
     MSString           buffer;
     int                bufSize;

     tickPositionLength=labelOut()->tickPositionLength();
     if(tickPositionLength!=0) bufSize=tickPositionLength+10;
     else bufSize=(int)(10+(valueMax()-valueMin())/incData()*(1+minorTickCount()));

     XSetForeground(display(),gc(),labelForeground());
     XSetFont(display(),gc(),labelFont());
     segments=new XSegment[bufSize];
  
     if(tickPositionLength!=0)
       {
	 int gridWidthLength=labelOut()->gridWidthLength();
	 int labelLength=labelOut()->labelLength();
	 int tickSizeLength=labelOut()->tickSizeLength();
	 int tickSize;
	 
	 for (unsigned i=0;i<tickPositionLength;i++)
	   {
	     if (labelOut()->tickPosition(i)<valueMin()||
		 labelOut()->tickPosition(i)>valueMax()) continue;
	     x=valueToPixel(labelOut()->tickPosition(i))+labelOffset();
	     tickSize=tickSizeLength==0?majorTickSize():int(majorTickSize()*labelOut()->tickSize(i));
	     if (tickSize>0&&ct<bufSize)
	       {
		 segments[ct].x1=segments[ct].x2=x;
		 segments[ct].y1=y-tickSize*sign;
		 segments[ct++].y2=y+tickSize*sign;
	       }
	     if (labelLength>0)
	       {
		 if (i<labelLength) labelOut()->label(buffer.removeAll(),i);
	       }
	     else
	       {
		 s=labelOut()->tickPosition(i);
		 formatLabel(buffer.removeAll(),s);
	       }
	     if (buffer.length()>0)
	       {
		 w=XTextWidth(fontInfo,buffer,buffer.length());
		 x=MSRight&labelAlignment()?x-w:MSLeft&labelAlignment()?x:x-w/2;
		 yy=y+((sign==1?fontInfo->ascent:fontInfo->descent)+majorTickSize())*sign;
		 XDrawString(display(),window(),gc(),fontInfo,x,yy,buffer,buffer.length());
	       }
	   }
       }
     else
       {
	 if ((labelOut()->formatType()==MSFormat::Date||
	      labelOut()->formatType()==MSFormat::Time)&&valueMax()>0)
	   {
	     r=outFmt().snapTime(valueMin(),incData());
	   }
	 else r=valueMin()-fmod(valueMin(),incData());
	 minorTick=minorTickCount()+1;
	 inc=incData()/minorTick;
	 
	 while (r<valueMin())
	   {
	     r+=inc;
	     majorTick++;
	   }
	 while (r<=valueMax()&&ct<bufSize)
	   {
	     x=valueToPixel(r)+labelOffset();
	     if (majorTick%minorTick==0)
	       {
		 if (majorTickSize()>0)
		   {
		     segments[ct].x1=segments[ct].x2=x;
		     segments[ct].y1=y-majorTickSize()*sign;
		     segments[ct++].y2=y+majorTickSize()*sign;
		   }
		 s=(fabs(r)/inc<1.0)?0:r;
		 formatLabel(buffer.removeAll(),s);
		 if (buffer.length()!=0)
		   {
		     w=XTextWidth(fontInfo,buffer,buffer.length());
		     x=MSRight&labelAlignment()?x-w:MSLeft&labelAlignment()?x:x-w/2;
		     yy=y+((sign==1?fontInfo->ascent:fontInfo->descent)+majorTickSize())*sign;
		     XDrawString(display(),window(),gc(),fontInfo,x,yy,buffer,buffer.length());
		   }
	       }
	     else
	       {
		 if (minorTickSize()>0)
		   {
		     segments[ct].x1=segments[ct].x2=x;
		     segments[ct].y1=y-minorTickSize()*sign;
		     segments[ct++].y2=y+minorTickSize()*sign;
		   }
	       }
	     r+=inc;
	     majorTick++;
	   }
       }
     XDrawSegments(display(),window(),gc(),segments,ct);
     delete [] segments;
   }  
}  

void MSHScale::moveSlider(const XEvent *event_)
{
  int			x=0;
  int 			ix,iy,rx,ry;
  int 			xoffset;
  unsigned int		keys,mask=0;
  Window 		root,child;
  
  if (event_->xbutton.button==1)
   {
     mask=Button1Mask;
     xoffset=event_->xbutton.x;
     x=slider()->x_origin()+xoffset;
   }
  else if (event_->xbutton.button==2)
   {
     setSliderPosition(event_->xbutton.x); 
     mask=Button2Mask;
     xoffset=slider()->width()/2;
   }
  keys=mask;
  while (keys&mask)
   {
     XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
     if (ix!=x)
      {
	setSliderPosition(ix-xoffset);
	setValue(pixelToValue(ix-xoffset));
      }
     x=ix;
   }
}

// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################
void MSHScale::computeLabelOffset(void)
{ labelOffset(slider()->width()/2-1); }

double MSHScale::pixelToValue(int x_)
{ return (x_-x_org()-slider()->offset())/scale()+base(); }

int MSHScale::valueToPixel(double x_)
{ return (int)((x_-base())*scale())+x_org()+slider()->offset(); }

int MSHScale::incFactor(int x_,int)
{ return x_>slider()->x_origin()?1:-1; } 

int MSHScale::sliderSize(void) const
{ return slider()->height(); }

void MSHScale::sliderSize(int x_)
{slider()->height(x_);}

void MSHScale::drawSliderEtch(void)
{slider()->drawSliderEtch();}


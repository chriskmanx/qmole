///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSVScale.H>

const int DefaultVSliderWidth    =14;
const int DefaultVSliderHeight   =30;
const int DefaultVScaleHeight    =200;
extern const int SliderAreaShadowThickness;

MSVScale::VSlider::VSlider(MSWidget* owner_): MSScale::Slider(owner_)
{}
MSVScale::VSlider::~VSlider()
{}

void MSVScale::VSlider::drawSliderEtch(void)  
{
  if (mapped()==MSTrue)
   {
     XDrawLine(display(),window(),bottomShadowGC(),1,height()/2-1,width()-2,height()/2-1);
     XDrawLine(display(),window(),topShadowGC(),1,height()/2,width()-2,height()/2);
   }
}

void MSVScale::VSlider::moveTo(int x_,int y_)
{
  MSWidget::moveTo(x_,y_);
  setValueWinYCoord(y_);
}

MSVScale::MSVScale(MSWidget *owner_,const char *title_) : 
MSScale(owner_,title_)
{ init(); }

MSVScale::MSVScale(MSWidget *owner_,const MSStringVector& title_) : 
MSScale(owner_,title_)
{ init(); }

MSVScale::MSVScale(MSWidget *owner_,MSFloat& model_,const char *title_) : 
MSScale(owner_,model_,title_)
{ init(); }

MSVScale::MSVScale(MSWidget *owner_,MSFloat& model_,const MSStringVector& title_) : 
MSScale(owner_,model_,title_)
{ init(); }

MSVScale::MSVScale(MSWidget *owner_,MSInt& model_,const char *title_) : 
MSScale(owner_,model_,title_)
{ init(); }

MSVScale::MSVScale(MSWidget *owner_,MSInt& model_,const MSStringVector& title_) : 
MSScale(owner_,model_,title_)
{ init(); }

void MSVScale::init(void)
{
  _slider=new VSlider(this);
  _labelAlignment=(unsigned long)(MSLeft);
  _valueAlignment=(unsigned long)(MSRight);
  sliderWidth(DefaultVSliderWidth);
  sliderHeight(DefaultVSliderHeight);
}

MSVScale::~MSVScale(void) {}

void MSVScale::home(void)
{ assignValue(valueMax()); }

void MSVScale::end(void)
{ assignValue(valueMin()); }

void MSVScale::computeSize(void)
{
  int w,h;
  XFontStruct *fontInfo=(XFontStruct *)server()->fontStruct(titleFont());
  _topOffset=highlightThickness()+shadowThickness();
  _titleWidth=_bottomOffset=_leftOffset=_rightOffset=_topOffset;
  _mintitleWidth=_maxtitleWidth=0;
  
  if (title().maxLength()>0)
   {
     h=(fontInfo->ascent+fontInfo->descent)*title().length();
     _topOffset+=MSBottom&titleAlignment()?0:h;
     _bottomOffset+=MSTop&titleAlignment()?0:h;
     for (int i=0;i<title().length();i++) 
      {
	w=XTextWidth(fontInfo,title()[i].string(),title()[i].length());
	titleWidth(w>titleWidth()?w:titleWidth());
      }
   }
  if (subtitle().maxLength()>0)
   {
     fontInfo=(XFontStruct *)server()->fontStruct(subtitleFont());
     h=(fontInfo->ascent+fontInfo->descent)*subtitle().length();
     _topOffset+=MSBottom&subtitleAlignment()?0:h;
     _bottomOffset+=MSTop&subtitleAlignment()?0:h;
     for (int i=0;i<subtitle().length();i++) 
      {
	w=XTextWidth(fontInfo,subtitle()[i].string(),subtitle()[i].length());
	titleWidth(w>titleWidth()?w:titleWidth());
      }
   }
  _titleWidth+=2*leftOffset();
  if (maxtitle().maxLength()>0)
   {
     fontInfo=(XFontStruct *)server()->fontStruct(maxtitleFont());
     _maxtitleHeight=fontInfo->ascent+fontInfo->descent;
     _maxtitleHeight*=maxtitle().length();
     _maxtitleHeight+=labelSpacing();
     _topOffset+=MSCenter&maxtitleAlignment()?maxtitleHeight():0;
     for (int i=0;i<maxtitle().length();i++) 
      {
	w=XTextWidth(fontInfo,maxtitle()[i].string(),maxtitle()[i].length());
	maxtitleWidth(w>maxtitleWidth()?w:maxtitleWidth());
      }
     _leftOffset+=MSLeft&maxtitleAlignment()?MSCenter&maxtitleAlignment()?0:maxtitleWidth():0;
     _rightOffset+=MSRight&maxtitleAlignment()?MSCenter&maxtitleAlignment()?0:maxtitleWidth():0;
   }
  if (mintitle().maxLength()>0)
   {
     fontInfo=(XFontStruct *)server()->fontStruct(mintitleFont());
     _mintitleHeight=fontInfo->ascent+fontInfo->descent;
     _mintitleHeight*=mintitle().length();
     _mintitleHeight+=labelSpacing();
     _bottomOffset+=MSCenter&mintitleAlignment()?mintitleHeight():0;
     for (int i=0;i<mintitle().length();i++) 
      {
	w=XTextWidth(fontInfo,mintitle()[i].string(),mintitle()[i].length());
	mintitleWidth(w>mintitleWidth()?w:mintitleWidth());
      }
     _leftOffset+=MSLeft&mintitleAlignment()?MSCenter&mintitleAlignment()?0:mintitleWidth():0;
     _rightOffset+=MSRight&mintitleAlignment()?MSCenter&mintitleAlignment()?0:mintitleWidth():0;
   }
  w=MSCenter&mintitleAlignment()?0:mintitleWidth();
  titleWidth(w>titleWidth()?w:titleWidth());
  w=MSCenter&maxtitleAlignment()?0:maxtitleWidth();
  titleWidth(w>titleWidth()?w:titleWidth());
  if (valueAlignment()!=0)
   {
     computeValueWinSize();
     _leftOffset+=MSRight&valueAlignment()?0:valueWin()->width();
     _rightOffset+=MSLeft&valueAlignment()?0:valueWin()->width();
   }  
  if (labelAlignment()!=0)
   {
     fontInfo=(XFontStruct *)server()->fontStruct(labelFont());
     MSString buffer;
     formatLabel(buffer,valueMin());
     int wmin=XTextWidth(fontInfo,buffer,buffer.length());
     wmin=wmin>labelOffset()?wmin-labelOffset():0;
     formatLabel(buffer.removeAll(),valueMax());
     int wmax=XTextWidth(fontInfo,buffer,buffer.length());
     wmax=wmax>labelOffset()?wmax-labelOffset():0;
     int labelWidth=wmin>wmax?wmin:wmax;
     labelWidth+=3+(majorTickSize()>minorTickSize()?majorTickSize():minorTickSize());
     _leftOffset+=MSRight&labelAlignment()?0:labelWidth;
     _rightOffset+=MSLeft&labelAlignment()?0:labelWidth;
   }
  _leftOffset=leftOffset()>rightOffset()?leftOffset():rightOffset();
  _rightOffset=rightOffset()>leftOffset()?rightOffset():leftOffset();
  int left=leftOffset()==0?SliderAreaShadowThickness:leftOffset();
  int right=rightOffset()==0?SliderAreaShadowThickness:rightOffset();
  w=slider()->width()+2*slider()->offset()+left+right;
  w=w>titleWidth()?w:titleWidth();
  w+=2*SliderAreaShadowThickness;
  naturalScaleSize(w);
  resize(w,DefaultVScaleHeight);
}

void MSVScale::setSliderPosition(int y_)
{
  int y=y_<y_org()+slider()->offset()?y_org()+slider()->offset():
        y_>y_end()-slider()->offset()-slider()->height()?
	y_end()-slider()->offset()-slider()->height():y_;
  int x=x_org()+SliderAreaShadowThickness;
  if (x!=slider()->x_origin()||y!=slider()->y_origin()) slider()->moveTo(x,y);
}

void MSVScale::computeSliderAreaSize(void)
{
  int top=topOffset();
  int bottom=bottomOffset();
  int h,labelHeight=0,valueHeight=0;
  if (labelAlignment()!=0)
   {
     XFontStruct *fontInfo=(XFontStruct *)server()->fontStruct(labelFont());
     labelHeight=(fontInfo->ascent+fontInfo->descent)/2;
     labelHeight=labelHeight>labelOffset()?labelHeight-labelOffset():labelHeight;
     labelHeight=labelHeight>mintitleHeight()?labelHeight-mintitleHeight():0;
   }
  if (valueAlignment()!=0)
   {
     valueHeight=valueWin()->textHeight()/2-labelOffset();
     valueHeight=valueHeight>mintitleHeight()?valueHeight-mintitleHeight():0;
   }
  h=(labelHeight>valueHeight?labelHeight:valueHeight)+SliderAreaShadowThickness;
  sliderAreaRect().y(top+h);
  sliderAreaRect().height(height()-y_org()-bottom-h);
  sliderAreaRect().x((width()-slider()->width())/2-slider()->offset());
  sliderAreaRect().width(slider()->width()+2*slider()->offset());
  valueWin()->x_org(MSLeft&valueAlignment()?x_org()-valueWin()->width():x_end());
}

void MSVScale::computeTickInc(void)
{
  XFontStruct  *fontInfo=(XFontStruct *)server()->fontStruct(labelFont());
  int           offset=labelOffset()==0?1:slider()->height();
  int 		h=sliderAreaRect().height()-offset-slider()->offset();
  double	range=fabs(valueMax()-valueMin());
  
  if (labelInc()<=0.0)
   {
     double r=range*fontInfo->ascent*3/h;
     incData(outFmt().snapNumber(r,*labelOut()));
   }
  else incData(labelInc());
}

void MSVScale::computeSliderScale(void)
{
  int offset=labelOffset()==0?0:slider()->height();
  int h=sliderAreaRect().height()-offset-2*SliderAreaShadowThickness;
  base(valueMin());
  scale(h/(valueMax()-valueMin()));
  scale((scale()>INT_MAX/2)?INT_MAX/2:scale());
}

void MSVScale::drawSliderTitles(void)
{
  int offset=highlightThickness()+shadowThickness();
  int x,len,w,y,center=x_org()+slider()->width()/2;
  XFontStruct *fontInfo;
  GC gc=XCreateGC(display(),window(),0,0);
  
  if (mintitle().maxLength()>0) 
   {
     fontInfo=(XFontStruct *)server()->fontStruct(mintitleFont());
     XSetFont(display(),gc,mintitleFont());
     XSetForeground(display(),gc,mintitleForeground());
     y=MSCenter&mintitleAlignment()?y_end()+labelSpacing():y_end()-mintitleHeight();
     y+=fontInfo->ascent;
     for (int i=0;i<mintitle().length();i++) 
      {
        len=mintitle()[i].length();
        const char *cp=mintitle()[i].string();
	w=XTextWidth(fontInfo,cp,len);
	x=MSCenter&mintitleAlignment()?center-w/2:MSLeft&mintitleAlignment()?x_org()-w:x_end();
	XDrawString(display(),window(),gc,fontInfo,x,y,cp,len);
	y+=fontInfo->ascent+fontInfo->descent;
      }
   }
  if (maxtitle().maxLength()!=0)
   {
     fontInfo=(XFontStruct *)server()->fontStruct(maxtitleFont());
     XSetFont(display(),gc,maxtitleFont());
     y=MSCenter&maxtitleAlignment()?y_org()-maxtitleHeight():y_org();
     XSetForeground(display(),gc,maxtitleForeground());
     y+=fontInfo->ascent;
     for (int i=0;i<maxtitle().length();i++) 
      {
	len=maxtitle()[i].length();
	const char *cp=maxtitle()[i].string();
	w=XTextWidth(fontInfo,cp,len);
	x=MSCenter&maxtitleAlignment()?center-w/2:MSLeft&maxtitleAlignment()?x_org()-w:x_end();
	XDrawString(display(),window(),gc,fontInfo,x,y,cp,len);
	y+=fontInfo->ascent+fontInfo->descent;
      }
   }
  XFreeGC(display(),gc);
}

void MSVScale::drawTickLabels(void)
{
  if (labelAlignment()!=MSNone)
   {
     int          majorTick=0,minorTick=minorTickCount()+1;
     int          y,xx,x=MSRight&labelAlignment()?x_end():x_org();
     int          ct=0,sign=MSRight&labelAlignment()?-1:1;
     int          tickPositionLength;
     int          offset=labelOffset()==0?1:labelOffset();
     double       r,s,inc;
     XSegment    *segments;
     XFontStruct *fontInfo=(XFontStruct *)server()->fontStruct(labelFont());
     int          bufSize;
     MSString     buffer;
     
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
	     y=valueToPixel(labelOut()->tickPosition(i))-offset +slider()->height();
	     tickSize=tickSizeLength==0?majorTickSize():int(majorTickSize()*labelOut()->tickSize(i));
	     if (tickSize>0&&ct<bufSize)
	       {
		 segments[ct].x1=x;
		 segments[ct].y1=segments[ct].y2=y;
		 segments[ct++].x2=x-tickSize*sign;
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
		 xx=x-(majorTickSize()+3)*sign;
		 xx+=MSRight&labelAlignment()|MSBottom&labelAlignment()?0:sign*fontInfo->descent/4;
		 xx-=MSRight&labelAlignment()?0:XTextWidth(fontInfo,buffer,buffer.length());
		 y+=MSBottom&labelAlignment()?-fontInfo->descent/2:MSTop&labelAlignment()?
		   fontInfo->ascent:fontInfo->ascent/2;
		 XDrawString(display(),window(),gc(),fontInfo,xx,y,buffer,buffer.length());
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
	 inc=incData()/minorTick;
	 
	 while (r<valueMin())
	   {
	     r+=inc;
	     majorTick++;
	   }
	 while (r<=valueMax()&&ct<bufSize)
	   {
	     y=valueToPixel(r)-offset+slider()->height();
	     if (majorTick%minorTick==0)
	       {
		 if (majorTickSize()>0)
		   {
		     segments[ct].x1=x;
		     segments[ct].y1=segments[ct].y2=y;
		     segments[ct++].x2=x-majorTickSize()*sign;
		   }
		 s=(fabs(r)/inc<1.0)?0:r;
		 formatLabel(buffer.removeAll(),s);
		 xx=x-(majorTickSize()+3)*sign;
		 xx+=MSRight&labelAlignment()|MSBottom&labelAlignment()?0:sign*fontInfo->descent/4;
		 xx-=MSRight&labelAlignment()?0:XTextWidth(fontInfo,buffer,buffer.length());
		 y+=MSBottom&labelAlignment()?-fontInfo->descent/2:MSTop&labelAlignment()?
		   fontInfo->ascent:fontInfo->ascent/2;
		 XDrawString(display(),window(),gc(),fontInfo,xx,y,buffer,buffer.length());
	       }
	     else
	       {
		 if (minorTickSize()>0)
		   {
		     segments[ct].x1=x;
		     segments[ct].y1=segments[ct].y2=y;
		     segments[ct++].x2=x-minorTickSize()*sign;
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

void MSVScale::moveSlider(const XEvent *event_)
{
  int			y=0;
  int 			ix,iy,rx,ry;
  int 			yoffset;
  unsigned int		keys,mask=0;
  Window 		root,child;
  
  if (event_->xbutton.button==1)
   {
     mask=Button1Mask;
     yoffset=event_->xbutton.y;
     y=slider()->y_origin()+yoffset;
   }
  else if (event_->xbutton.button==2)
   {
     setSliderPosition(event_->xbutton.y); 
     mask=Button2Mask;
     yoffset=slider()->height()/2;
   }
  keys=mask;
  while (keys&mask)
   {
     XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
     if (iy!=y)
      {
	setSliderPosition(iy-yoffset);
	setValue(pixelToValue(iy-yoffset));
      }
     y=iy;
   }
}


// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################

int MSVScale::sliderSize(void) const
{ return slider()->width(); }

void MSVScale::sliderSize(int x_)
{ slider()->width(x_); }

void MSVScale::computeLabelOffset(void)
{ labelOffset(slider()->height()/2+1); }

double MSVScale::pixelToValue(int y_)
{ return (y_end()-slider()->offset()-slider()->height()-y_)/scale()+base(); }

int MSVScale::valueToPixel(double y_)
{ return (int)(y_end()-slider()->offset()-slider()->height()-(y_-base())*scale()); }

int MSVScale::incFactor(int,int y_)
{ return y_>slider()->y_origin()?-1:1; } 

void MSVScale::drawSliderEtch(void)
{slider()->drawSliderEtch();}

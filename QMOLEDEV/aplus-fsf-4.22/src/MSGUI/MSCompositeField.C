///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSCompositeField.H>
#include <MSGUI/MSShadow.H>

static const unsigned MSCompositeFieldDefaultValueWidth=9;
static const unsigned MSCompositeFieldDefaultShadowThickness=0;
static const unsigned MSCompositeFieldDefaultHighlightThickness=1;
static const unsigned MSCompositeFieldDefaultSpacing=2;
static const unsigned MSCompositeFieldDefaultEditWidth=UINT_MAX-1;

static const unsigned MSCompositeFieldValueShadowThickness=2;
static const unsigned MSCompositeFieldValueHighlightThickness=0;
static const unsigned MSCompositeFieldLabelShadowThickness=0;
static const unsigned MSCompositeFieldLabelHighlightThickness=0;
static const unsigned MSCompositeFieldValueDefaultMargin=2;

MSCompositeField::FieldValue::FieldValue(MSWidget *owner_) :
MSTextRect(owner_), _shadow(owner_),
_shadowThickness(MSCompositeFieldValueShadowThickness),
_shadowStyle(MSSunken)
{}

MSCompositeField::FieldValue::~FieldValue(void)
{ }

const MSShadow& MSCompositeField::FieldValue::shadow(void) const
{ return _shadow; }

int MSCompositeField::FieldValue::marginWidth(void) const
{ return MSCompositeFieldValueDefaultMargin; }
int MSCompositeField::FieldValue::highlightThickness(void) const
{ return MSCompositeFieldValueHighlightThickness; }
int MSCompositeField::FieldValue::shadowThickness(void) const
{ return _shadowThickness; }
MSShadowStyle MSCompositeField::FieldValue::shadowStyle(void) const
{ return _shadowStyle; }

int MSCompositeField::FieldValue::offset(void) const
{ return highlightThickness()+shadowThickness()+marginWidth(); }

void MSCompositeField::FieldValue::shadowThickness(int sht_)
{ _shadowThickness=sht_; }
void MSCompositeField::FieldValue::shadowStyle(MSShadowStyle style_)
{ _shadowStyle=style_; }

void MSCompositeField::FieldValue::naturalSize(void)
{ 
  unsigned offset=2*(highlightThickness()+shadowThickness());
  unsigned w=2*marginWidth()+textWidth();
  unsigned h=textHeight();
  resize(offset+w,offset+h);
}

void MSCompositeField::FieldValue::updateBackground(unsigned long)
{ _shadow.color(background()); }

MSCompositeField::FieldLabel::FieldLabel(MSWidget *owner_) :
MSTextRect(owner_)
{}
MSCompositeField::FieldLabel::FieldLabel(MSWidget *owner_,const char *label_) : 
MSTextRect(owner_,label_)
{}
MSCompositeField::FieldLabel::FieldLabel(MSWidget *owner_,const MSString& label_) : 
MSTextRect(owner_,label_)
{}
MSCompositeField::FieldLabel::~FieldLabel(void)
{}

int MSCompositeField::FieldLabel::marginWidth(void) const
{ return (length()>0)?MSCompositeFieldValueDefaultMargin:0; }
int MSCompositeField::FieldLabel::highlightThickness(void) const
{ return MSCompositeFieldLabelHighlightThickness; }
int MSCompositeField::FieldLabel::shadowThickness(void) const
{ return MSCompositeFieldLabelShadowThickness; }
int MSCompositeField::FieldLabel::offset(void) const
{ return highlightThickness()+shadowThickness()+marginWidth(); }

int MSCompositeField::FieldLabel::naturalWidth(void) const
{ return 2*(highlightThickness()+shadowThickness()+marginWidth())+textWidth(); }

void MSCompositeField::FieldLabel::naturalSize(void)
{
  unsigned offset=2*(highlightThickness()+shadowThickness());
  unsigned w=2*marginWidth()+textWidth();
  unsigned h=textHeight();
  resize(offset+w,offset+h);
}

// ##################################################################
// MSCompositeField
// ##################################################################

MSCompositeField::MSCompositeField(MSWidget *owner_,const MSSymbol& tag_) : 
_tag(tag_),MSComposite(owner_) 
{ 
  _fieldLabel=new FieldLabel(this);
  init(); 
}

MSCompositeField::MSCompositeField(MSWidget *owner_,const char *label_,const MSSymbol& tag_) : 
_tag(tag_),MSComposite(owner_) 
{ 
  _fieldLabel=new FieldLabel(this,label_);
  init(); 
}

MSCompositeField::~MSCompositeField(void)
{
  if (_fieldLabel!=0)  delete _fieldLabel;
  if (_fieldValue!=0)  delete _fieldValue;
  XFreeGC(display(),_drawGC);
}

void MSCompositeField::init(void)
{
  if(server()->defaultValueBackground()==0)
   {
     server()->defaultValueBackground(server()->colorName(server()->defaultBackground()));
   }
  freeze();
  _clipMode=MSNoClipping;
  _fieldValue=new FieldValue(this);
  _labelAlignment=MSLeft;
  _labelJustification=MSRight;
  _valueAlignment=MSLeft;  
  _labelSpacing=MSCompositeFieldDefaultSpacing;
  _shadowThickness=MSCompositeFieldDefaultShadowThickness;
  _highlightThickness=MSCompositeFieldDefaultHighlightThickness;
  _valueWidth=MSCompositeFieldDefaultValueWidth;
  _editWidth=MSCompositeFieldDefaultEditWidth;
  _marginHeight=0;
  _clipIndicatorForeground=foreground();
  shadowStyle(MSSunken);
  resizeConstraints(At::Top|At::MaintainHeight);
  addToFocusList();
  if (server()->defaultValueBackground()!=server()->defaultBackground())
   {
     _fieldValue->background(server()->defaultValueBackground());
   }
  _drawGC=XCreateGC(display(),window(),0,0);
  XSetFont(display(),drawGC(),font());
  XSetStipple(display(),drawGC(),_fieldValue->textMSGC().stipple());
}

MSBoolean MSCompositeField::validate(const char *) 
{ return MSFalse; }
const char *MSCompositeField::formatOutput(MSString &buffer_)
{ return buffer_.string(); }

void MSCompositeField::firstMapNotify(void)
{
  unfreeze();
  adjustSize();
}

void MSCompositeField::labelPixelWidth(unsigned labelPixelWidth_) 
{
  if (_fieldLabel!=0)
   {
     if (_fieldLabel->length()==0) labelPixelWidth_=0;
     if (_fieldLabel->width()!=labelPixelWidth_)
      {
	int offset=highlightThickness()+shadowThickness();

	if (labelAlignment()==MSTop)
	 {
	   unsigned lh=_fieldLabel->textHeight()+
	            2*(_fieldLabel->shadowThickness()+_fieldLabel->highlightThickness());

	   _fieldLabel->moveTo(offset,offset);
	   _fieldValue->moveTo(offset,offset+lh);
	   _fieldLabel->width(labelPixelWidth_);
	   _fieldValue->width(width()-2*offset);
	 }
	else
	 {
	   int trueWidth=width()-2*offset;
	   int vw=trueWidth-int(labelPixelWidth_);
	   _fieldLabel->width(labelPixelWidth_);
	   _fieldValue->width(vw);
	   _fieldLabel->moveTo(offset,offset);
	   _fieldValue->moveTo(offset+labelPixelWidth_,offset);
	 }
	redraw();
      }
   }
}

void MSCompositeField::placement(void) 
{
  if (_fieldValue!=0&&_fieldLabel!=0)
   {
     int offset=highlightThickness()+shadowThickness();
     int offset2=offset<<1;
     int vh=_fieldValue->textHeight()+
         2*(marginHeight()+_fieldValue->shadowThickness()+_fieldValue->highlightThickness());
     int lh=_fieldLabel->textHeight()+
         2*(_fieldLabel->shadowThickness()+_fieldLabel->highlightThickness());
     int lw=_fieldLabel->width();
     int vw=_fieldValue->width();
     int trueWidth=width()-offset2;

     if (labelAlignment()==MSTop)
      {
	_fieldLabel->moveTo(offset,offset);
	_fieldValue->moveTo(offset,offset+lh+labelSpacing());
	_fieldLabel->width(lw);
	_fieldValue->width(trueWidth);
      }
     else
      {
	vw=trueWidth-_fieldLabel->width();
	int h=(vh>lh)?vh:lh;
	if (vh==lh) height(h+offset2);
	else if (h+offset2>height()) height(h+offset2);   
	
	_fieldLabel->height(h);
	_fieldValue->resize(vw,h);
	_fieldLabel->moveTo(offset,offset);
	_fieldValue->moveTo(offset+_fieldLabel->width(),offset);
      }
     redraw();
   }
}

void MSCompositeField::adjustSize(void)
{
  if (frozen()==MSFalse)
   {
     int offset=2*(highlightThickness()+shadowThickness());
     int vw=computeValuePixelWidth();
     int lw=2*_fieldLabel->offset()+_fieldLabel->textWidth();
     int vh=_fieldValue->textHeight()+
            2*(marginHeight()+_fieldValue->shadowThickness()+_fieldValue->highlightThickness());
     int lh=_fieldLabel->textHeight()+
            2*(_fieldLabel->shadowThickness()+_fieldLabel->highlightThickness());
     int w,h;

     if (labelAlignment()==MSTop)
      {
	w=(vw>lw)?vw:lw;
	h=lh+vh+labelSpacing();
	_fieldLabel->resize(lw,lh);
	_fieldValue->resize(w,vh);  
      }
     else
      {
	w=lw+vw;
	h=(vh>lh)?vh:lh;
	_fieldLabel->resize(lw,h);
	_fieldValue->resize(vw,h);  
      }
     w+=offset;
     h+=offset;
     if (w!=width()||h!=height()) resize(w,h);
     else placement();
   }
}

unsigned MSCompositeField::computeLabelPixelWidth(void)
{ return _fieldLabel->width(); }

unsigned MSCompositeField::computeValuePixelWidth(void)
{ return valueWidth()*_fieldValue->charWidth()+2*_fieldValue->offset(); }
  
void MSCompositeField::computeSize(void)
{ adjustSize(); }
void MSCompositeField::naturalSize(void) 
{ adjustSize(); }
void MSCompositeField::configure(void) 
{ placement(); }

void MSCompositeField::redraw(void)
{
  if (frozen()==MSFalse&&mapped()==MSTrue)
   {
     drawFieldLabel();
     drawFieldValue();
     drawShadow();
     if (highlighted()==MSTrue) drawHighlight();
   }
}

void MSCompositeField::drawFieldLabel(void)
{
  if (frozen()==MSFalse&&owner()->mapped()==MSTrue&&mapped()==MSTrue)
   {
     XFillRectangle(display(),window(),backgroundShadowGC(),
		    _fieldLabel->x(),_fieldLabel->y(),
		    _fieldLabel->width(),_fieldLabel->height());
     
     const char *pString=_fieldLabel->string();
     if (pString!=0)
      {
	unsigned len=_fieldLabel->length();
	if (len>0)
	 {
	   int margin=(_fieldLabel->height()-_fieldLabel->textHeight())>>1;
           margin=(margin>0)?margin:0;
	   int yy=_fieldLabel->y()+margin+_fieldLabel->textAscent();
           int xx;
           int tw=_fieldLabel->textWidth();
           int lw;
           if (labelAlignment()==MSLeft) lw=_fieldLabel->width();
           else lw=_fieldValue->width();
           int loffset=_fieldLabel->offset();
           if (labelJustification()==MSCenter)
            {
              if (lw>=tw+loffset*2)
               {
                 xx=_fieldLabel->x()+(lw-tw)/2;
               }
              else
               {
                 xx=_fieldLabel->x()+loffset;
               }
            }
           else if (labelJustification()==MSRight)
            {
              xx=_fieldLabel->x()+(lw-loffset-tw);
            }
           else
            {
              xx=_fieldLabel->x()+loffset;
            }
	   XDrawString(display(),window(),_fieldLabel->textGC(),_fieldLabel->textFontStruct(),
		       xx,yy,pString,len);
	 }
      }
   }
}

void MSCompositeField::drawFieldValue(void)
{ drawFieldValue(_fieldValue->foreground(),_fieldValue->background()); }

void MSCompositeField::drawFieldValue(unsigned long fg_,unsigned long bg_)
{
  if (frozen()==MSFalse&&owner()->mapped()==MSTrue&&mapped()==MSTrue)
   {
     int sht=_fieldValue->shadowThickness();
     XFillRectangle(display(),window(),
		    _fieldValue->shadow().backgroundShadowGC(),
		    _fieldValue->x()+sht,
		    _fieldValue->y()+sht,
		    _fieldValue->width()-(2*sht),
		    _fieldValue->height()-(2*sht));
     
     MSString buffer;
     const char *pString=formatOutput(buffer);
     if (pString!=0&&buffer.length()>0)
      {
	int len=buffer.length();
	int fulllen=len;
	int clipIndicatorWidth=0;
	int clipIndicatorMargin=2;
	int dw=_fieldValue->width()-2*_fieldValue->offset();
	if (clipMode()==MSClipIndicator)
	 {
	   if (_fieldValue->textWidth(pString,len)>dw)
	    {
	      clipIndicatorWidth=_fieldValue->charWidth()+clipIndicatorMargin;
	      dw-=clipIndicatorWidth;
	      if (dw<0)
	       {
		 dw=0;
		 clipIndicatorWidth=0;
	       }
	    }
	 }
	if (dw>0)
	 {
	   while (_fieldValue->textWidth(pString,len)>dw) len--;
	   if (len>0)
	    {
	      int offset=_fieldValue->highlightThickness()+_fieldValue->shadowThickness();
	      int margin=(_fieldValue->height()-(2*offset+_fieldValue->textHeight()))/2;
	      int xx=_fieldValue->x()+_fieldValue->offset();
	      int yy=_fieldValue->y()+offset+((margin>0)?margin:0)+_fieldValue->textAscent();

              if (valueAlignment()==MSCenter&&fulllen==len)
               {
                 int tw=_fieldValue->textWidth(pString,len);
                 xx+=(dw-tw)/2;
               }
              else if (valueAlignment()==MSRight||(clipIndicatorWidth>0&&fulllen>len))
	       {
		 int tw=_fieldValue->textWidth(pString,len);
		 xx=_fieldValue->x()+_fieldValue->width()-_fieldValue->offset()-tw-clipIndicatorWidth;
	       }
	      XSetForeground(display(),drawGC(),fg_);
	      XSetBackground(display(),drawGC(),bg_);
	      XDrawString(display(),window(),
			  drawGC(),_fieldValue->textFontStruct(),
			  xx,yy,pString,len);
	    }
	 }
	if (fulllen>len&&clipIndicatorWidth>0)
	 {
	   int offset=_fieldValue->highlightThickness()+_fieldValue->shadowThickness()+3;
	   int xx=_fieldValue->x()+_fieldValue->width()-offset-clipIndicatorWidth+clipIndicatorMargin;
	   XPoint points[3];
	   points[0].x=xx;
	   points[0].y=offset;
	   points[1].x=clipIndicatorWidth;
	   points[1].y=(height()-2*offset)/2;
	   points[2].x=-clipIndicatorWidth;
	   points[2].y=points[1].y;
	   XSetForeground(display(),drawGC(),clipIndicatorForeground());
	   XFillPolygon(display(),window(),drawGC(),points,3,
			Convex,CoordModePrevious);
	 }
      }
     drawFieldValueShadow();
   }
}

void MSCompositeField::drawFieldValueShadow(void)
{
  int st=_fieldValue->shadowThickness();
  if (st>0)
   {
     MSRect aRect(_fieldValue->x(),_fieldValue->y(),
		  _fieldValue->width(),_fieldValue->height());

     //Have to draw Bevel ourselves, since shadow may be different
     //from ours.
     GC topGC=_fieldValue->shadow().topShadowGC();
     GC bottomGC=_fieldValue->shadow().bottomShadowGC();
     Window id=window();
     switch (valueShadowStyle())
      {
      case MSRaised:    drawBevelShadow(id,aRect,st,topGC,bottomGC);              break;
      case MSSunken:    drawBevelShadow(id,aRect,st,bottomGC,topGC);              break;
      case MSEtchedIn:  drawEtchedShadow(id,aRect,MSEtchedIn,st,bottomGC,topGC);  break;
      case MSEtchedOut: drawEtchedShadow(id,aRect,MSEtchedOut,st,topGC,bottomGC); break;
      case MSFlat:      drawFlatShadow(id,aRect,st,bottomGC);                     break;
      }
   }
}

void MSCompositeField::labelAlignment(MSAlignment alignment_) 
{ 
  if (alignment_!=labelAlignment())
   {
     if (alignment_==MSLeft||alignment_==MSTop)
      {
	_labelAlignment=alignment_;
	computeSize();
      }
   }
}

void MSCompositeField::labelJustification(MSAlignment justification_) 
{ 
  if (justification_!=labelJustification())
   {
     if (justification_==MSLeft||justification_==MSRight||justification_==MSCenter)
      {
	_labelJustification=justification_;
	computeSize();
      }
   }
}

void MSCompositeField::valueAlignment(MSAlignment alignment_) 
{ 
  if (alignment_!=valueAlignment())
   {
     if (alignment_==MSLeft||alignment_==MSRight||alignment_==MSCenter)
      {
	_valueAlignment=alignment_;
	drawFieldValue();
      }
   }
}

void MSCompositeField::labelForeground(const char *fg_) 
{ labelForeground(server()->pixel(fg_)); }
void MSCompositeField::labelForeground(unsigned long fg_) 
{ 
  if (fg_!=_fieldLabel->foreground())
   {
     _fieldLabel->foreground(fg_);  
     drawFieldLabel();
   }
}
   
void MSCompositeField::labelFont(const char *font_) 
{ labelFont(server()->fontID(font_)); }
  
void MSCompositeField::labelFont(Font font_) 
{ 
  if (font_!=_fieldLabel->font())
   {
     _fieldLabel->font(font_);  
     if (dynamic()==MSTrue) computeSize();
     else drawFieldLabel();
   }
}

void MSCompositeField::label(const MSString& aString_) 
{
  if (_fieldLabel->label(aString_)==MSTrue) 
   { 
     _fieldLabel->naturalSize();
     placement();
   }
  else drawFieldLabel();
}

void MSCompositeField::label(const char *pString_)
{
  if (_fieldLabel->label(pString_)==MSTrue) 
   { 
     _fieldLabel->naturalSize();
     placement();
   }
  else drawFieldLabel();
}

MSShadowStyle MSCompositeField::valueShadowStyle(void) const
{ return _fieldValue->shadowStyle(); }
int MSCompositeField::valueShadowThickness(void) const  
{ return _fieldValue->shadowThickness(); }

void MSCompositeField::valueShadowStyle(MSShadowStyle style_)
{
  if (style_!=valueShadowStyle())
   {
     _fieldValue->shadowStyle(style_);
     drawFieldValue();
   }
}
void MSCompositeField::valueShadowThickness(int sht_)
{
  if (sht_!=valueShadowThickness())
   {
     _fieldValue->shadowThickness(sht_);
     computeSize();
   }
}

void MSCompositeField::valueBackground(const char *bg_) 
{ valueBackground(server()->pixel(bg_)); }
void MSCompositeField::valueBackground(unsigned long bg_) 
{ 
  if (bg_!=_fieldValue->background())
   {
     _fieldValue->background(bg_);  
     drawFieldValue();
   }
}

void MSCompositeField::valueForeground(const char *fg_) 
{ valueForeground(server()->pixel(fg_)); }
void MSCompositeField::valueForeground(unsigned long fg_) 
{ 
  if (fg_!=_fieldValue->foreground())
   {
     _fieldValue->foreground(fg_);  
     drawFieldValue();
   }
}
void MSCompositeField::valueFont(const char *font_) 
{ valueFont(server()->fontID(font_)); }
  
void MSCompositeField::valueFont(Font font_) 
{ 
  if (font_!=_fieldValue->font())
   {
     _fieldValue->font(font_);  
     XSetFont(display(),drawGC(),font_);
     if (dynamic()==MSTrue) computeSize();
     else drawFieldValue();
   }
}
void MSCompositeField::clipIndicatorForeground(const char *fg_) 
{ clipIndicatorForeground(server()->pixel(fg_)); }
void MSCompositeField::clipIndicatorForeground(unsigned long fg_) 
{ 
  if (_clipIndicatorForeground!=fg_)
   {
     _clipIndicatorForeground=fg_;
     drawFieldValue();
   }
}

void MSCompositeField::editWidth(unsigned editWidth_)
{ if (editWidth()!=editWidth_) _editWidth=editWidth_; }

void MSCompositeField::marginHeight(unsigned marginHeight_)
{
  if (marginHeight()!=marginHeight_)
   {
     _marginHeight=marginHeight_;
     if (firstMap()==MSTrue||dynamic()==MSTrue) computeSize();
   }
}

void MSCompositeField::valueWidth(unsigned valueWidth_)
{
  if (valueWidth_!=valueWidth())
   {
     _valueWidth=valueWidth_;
     if (firstMap()==MSTrue) adjustSize();
   }
}

void MSCompositeField::labelSpacing(unsigned labelSpacing_)
{
  if (labelSpacing_!=labelSpacing())
   {
     _labelSpacing=labelSpacing_;
     if (firstMap()==MSTrue&&labelAlignment()==MSTop) adjustSize();
   }
}

void MSCompositeField::updateFont(Font oldfid_)
{ 
  MSComposite::updateFont(oldfid_);
  if(_fieldValue->font() == oldfid_)
   {
     _fieldValue->font(font()); 
     XSetFont(display(),drawGC(),font());
   }
  if(oldfid_ == _fieldLabel->font()) _fieldLabel->font(font());
  if (dynamic()==MSTrue) computeSize();
  else redraw();
}

void MSCompositeField::updateBackground(unsigned long oldbg_)
{ 
  MSComposite::updateBackground(oldbg_); 
  if (_fieldValue->background()==oldbg_) _fieldValue->background(background());
  _fieldLabel->background(background()); 
  redraw();
}

void MSCompositeField::updateForeground(unsigned long oldfg_)
{
  MSComposite::updateForeground(oldfg_);
  if(_fieldValue->foreground()==oldfg_) _fieldValue->foreground(foreground()); 
  if(_fieldLabel->foreground()==oldfg_) _fieldLabel->foreground(foreground());
  if (clipIndicatorForeground()==oldfg_) _clipIndicatorForeground=foreground();
  redraw();
}

void MSCompositeField::updateSensitivity(void)
{
  _fieldValue->sensitive(sensitive());
  _fieldLabel->sensitive(sensitive()); 
   if (sensitive()==MSTrue) XSetFillStyle(display(),drawGC(),FillSolid);
   else XSetFillStyle(display(),drawGC(),FillStippled);  
  redraw();
}

void MSCompositeField::focusIn(void) 
{ highlight(); }

void MSCompositeField::focusOut(void) 
{ unHighlight(); }

MSBoolean MSCompositeField::loseFocus(void) 
{ 
  unHighlight(); 
  return MSTrue; 
}

void MSCompositeField::set(MSAttrValueList& avList_)
{
  MSComposite::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     // use member functions inherited from MSCompositeField
     if (avList_[i].attribute()=="label")
      label(avList_[i].value()),index<<i;  
     else if (avList_[i].attribute()=="labelFont")
      labelFont(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="valueFont")
      valueFont(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="labelForeground")
      labelForeground(avList_[i].value()),index<<i;     
     else if (avList_[i].attribute()=="valueAlignment")
      {
	if (avList_[i].value()=="MSLeft") valueAlignment(MSLeft);
        else if (avList_[i].value()=="MSCenter") valueAlignment(MSCenter);
	else valueAlignment(MSRight);
	index<<i;
      }
     else if (avList_[i].attribute()=="labelSpacing")
      labelSpacing(avList_[i].value().asInt()),index<<i;
     else if (avList_[i].attribute()=="valueWidth")
      valueWidth(avList_[i].value().asInt()),index<<i;
     else if (avList_[i].attribute()=="valueBackground")
      valueBackground(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="valueForeground")
      valueForeground(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="valueShadowStyle")
      valueShadowStyle(MSAttrValue::stringToShadowStyle(avList_[i].value())),index<<i;
     else if (avList_[i].attribute()=="valueShadowThickness")
      valueShadowThickness(avList_[i].value().asInt()),index<<i;
     else if (avList_[i].attribute()=="labelAlignment")
      {
	if (avList_[i].value()=="MSTop") labelAlignment(MSTop);
	else labelAlignment(MSLeft);
	index<<i;
      }
     else if (avList_[i].attribute()=="labelJustification")
      {
	if (avList_[i].value()=="MSLeft") labelJustification(MSLeft);
	else if (avList_[i].value()=="MSRight") labelJustification(MSRight);
	else if (avList_[i].value()=="MSCenter") labelJustification(MSCenter);
	else labelJustification(MSLeft);
	index<<i;
      }
     else if (avList_[i].attribute()=="marginHeight")
      marginHeight(avList_[i].value().asInt()),index<<i;     
     else if (avList_[i].attribute()=="clipMode")
      {
	if (avList_[i].value()=="MSClipIndicator") clipMode(MSClipIndicator);
        else if(avList_[i].value()=="MSClipStars") clipMode(MSClipStars);
	else clipMode(MSNoClipping);
	index<<i;
      }
     else if (avList_[i].attribute()=="clipIndicatorForeground")
      clipIndicatorForeground(avList_[i].value()),index<<i;
     else if(avList_[i].attribute()=="tag")
       {
         if(avList_[i].value().length()==0) tag(MSSymbol::nullSymbol());
         else tag(MSSymbol(avList_[i].value()));
         index<<i;
       }
   }
  avList_.remove(index);
}

MSAttrValueList& MSCompositeField::get(MSAttrValueList& avList_)
{
  char *value;
  avList_<<MSAttrValue("tag",tag().symbolName(),MSAttrValue::Control|MSAttrValue::String);
  avList_<<MSAttrValue("label",label(),MSAttrValue::String);
  switch (labelAlignment())
   {
   case MSTop:  value="MSTop";  break;
   default:     value="MSLeft"; break;
   }
  avList_<<MSAttrValue("labelAlignment",value,
		       MSStringVector("MSLeft\nMSTop"));
  switch (labelJustification())
   {
   case MSRight:  value="MSRight";  break;
   case MSCenter: value="MSCenter"; break;
   case MSLeft: 
   default:       value="MSLeft";   break;
   }
  avList_<<MSAttrValue("labelFont",
		       server()->fontName(labelFont()),MSAttrValue::Font);
  avList_<<MSAttrValue("labelForeground",
		       server()->colorName(labelForeground()),MSAttrValue::Color);  
  avList_<<MSAttrValue("labelJustification",value,
		       MSStringVector("MSLeft\nMSRight\nMSCenter"));
  avList_<<MSAttrValue("labelSpacing",MSString(labelSpacing()));
  switch (valueAlignment())
   {
   case MSLeft:   value="MSLeft";  break;
   case MSCenter: value="MSCenter";  break;
   case MSRight:
   default:       value="MSRight"; break;
   }
  avList_<<MSAttrValue("marginHeight",MSString(marginHeight()));
  avList_<<MSAttrValue("valueAlignment",value,
		       MSStringVector("MSLeft\nMSRight\nMSCenter"));
  avList_<<MSAttrValue("valueBackground",
		       server()->colorName(valueBackground()),MSAttrValue::Color);
  avList_<<MSAttrValue("valueForeground",
		       server()->colorName(valueForeground()),MSAttrValue::Color);
  avList_<<MSAttrValue("valueFont",
		       server()->fontName(valueFont()),MSAttrValue::Font);
  avList_<<MSAttrValue("valueShadowStyle",MSAttrValue::shadowStyleToString(valueShadowStyle()),
		       MSStringVector("MSEtchedIn\nMSEtchedOut\nMSFlat\nMSRaised\nMSSunken"));
  avList_<<MSAttrValue("valueShadowThickness",MSString(valueShadowThickness()));  
  avList_<<MSAttrValue("valueWidth",MSString(valueWidth()));
  switch (clipMode())
   {
   case MSClipIndicator:  value="MSClipIndicator"; break;
   case MSClipStars:      value="MSClipStars";     break;
   case MSNoClipping:
   default:               value="MSNoClipping";    break;
   }
  avList_<<MSAttrValue("clipIndicatorForeground",
		       server()->colorName(clipIndicatorForeground()),MSAttrValue::Color);
  avList_<<MSAttrValue("clipMode",value,
                       MSStringVector("MSNoClipping\nMSClipStars\nMSClipIndicator"));
  return MSComposite::get(avList_);
}

// ##################################################################
// inline methods
// ##################################################################

MSCompositeField::FieldValue *MSCompositeField::fieldValue(void) const
{ return _fieldValue; }
MSCompositeField::FieldLabel *MSCompositeField::fieldLabel(void) const     
{ return _fieldLabel; }

void MSCompositeField::tag(const MSSymbol& tag_)
{ _tag=tag_; }
const MSSymbol& MSCompositeField::tag(void) const           
{ return _tag; }
MSAlignment MSCompositeField::labelAlignment(void) const
{ return _labelAlignment; }
MSAlignment MSCompositeField::labelJustification(void) const
{ return _labelJustification;}
MSAlignment MSCompositeField::valueAlignment(void) const
{ return _valueAlignment; }
unsigned MSCompositeField::labelSpacing(void) const   
{ return _labelSpacing; }
unsigned long MSCompositeField::labelForeground(void) const
{ return _fieldLabel->foreground(); }
unsigned long MSCompositeField::valueBackground(void) const
{ return _fieldValue->background(); }
unsigned long MSCompositeField::valueForeground(void) const
{ return _fieldValue->foreground(); }
Font MSCompositeField::labelFont(void) const
{ return _fieldLabel->font(); }
Font MSCompositeField::valueFont(void) const
{ return _fieldValue->font(); }
const MSString& MSCompositeField::label(void) const
{ return _fieldLabel->label(); }
unsigned MSCompositeField::valueWidth(void) const                 
{ return _valueWidth; }
unsigned MSCompositeField::editWidth(void) const                  
{ return _editWidth; }
unsigned MSCompositeField::marginHeight(void) const                  
{ return _marginHeight; }

unsigned long MSCompositeField::clipIndicatorForeground(void) const
{ return _clipIndicatorForeground; }

MSClipMode MSCompositeField::clipMode(void) const
{ return _clipMode;}
  
void MSCompositeField::clipMode(MSClipMode clipMode_)
{
  if(_clipMode!=clipMode_)
   {
     _clipMode=clipMode_;
     redraw();
   }
}


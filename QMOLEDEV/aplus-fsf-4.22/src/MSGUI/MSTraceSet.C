///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <float.h>
#include <MSGUI/MSTraceSet.H>
#include <MSTypes/MSMessageLog.H>

#ifndef MSGraphHEADER
#include <MSGUI/MSGraph.H>
#endif

static const int   MaxTraceLineWidth      =35;
static const int   MaxTraceSymbolSize     =100;
static const char *DefaultTraceFont="lucidasanstypewriter-12";
MSStringVector     MSTraceSet::_text;

MSTraceSet::MSTraceSet(MSGraph *gr_,const char *legend_,const MSSymbol& tag_):
_tag(tag_),_legend(legend_),MSWidgetView(gr_)
{ init(); }

MSTraceSet::MSTraceSet(MSGraph *gr_,const MSStringVector& legend_,const MSSymbol& tag_):
_tag(tag_),_legend(legend_),MSWidgetView(gr_)
{ init(); }

void MSTraceSet::textFont(const char *font_)
{
  textFont(owner()->server()->fontID(font_));
  int ct=traceList().count();
  for (int i=0;i<ct;i++) trace(i)->font(textFont());
  graph()->redrawImmediately(); 
}

void MSTraceSet::textFont(Font font_)
{
  if (font_!=textFont())
   {
     _textFont=font_;
     graph()->redrawImmediately();
   }
}

void MSTraceSet::textForeground(const char *fg_)
{ textForeground(owner()->server()->pixel(fg_)); }

void MSTraceSet::textForeground(unsigned long fg_)
{
  if (fg_!=textForeground())
   {
     _textForeground=fg_;
     graph()->redrawImmediately();
   }
}

void MSTraceSet::init(void)
{
  _textForeground         =0;
  _updateIndex	          =0;
  _selectRow	          =0;
  _selectCol	          =0;
  _xMax	                  =0.0;
  _xMin	                  =0.0;
  _yMax	                  =0.0;
  _yMin	                  =0.0;
  _xDelta	          =0.0;
  _xOffset	          =0.0;
  _yOffset	          =0.0;
  _pieDepthFactor         =1.0;
  _pieAspectRatio         =0.6;
  _primarySlice           =-1;
  _primarySliceAlignment  =MSTop;
  _pieAngle               =0;
  _constraint             =HoldX;
  _pieLegendAlignment     =MSCenter;
  _pieValueAlignment      =MSNone;
  _piePercentAlignment    =MSNone;
  _piePercentFormat       =MSFloat::Decimal0;
  _pieProfiles            =0;
  _pieOffsets             =0;
  _pieOffset              =0;
  _lastDataCount          =0;
  _normalizedOffset       =0;
  _selectable             =MSFalse;
  _overlap                =MSFalse;
  _textFont               =server()->fontID(DefaultTraceFont);
  childCreateNotify();    
}                         

MSTraceSet::~MSTraceSet(void)
{
  deleteTraces();
  if (pieOffsets()!=0) delete pieOffsets();
  if (pieProfiles()!=0) delete pieProfiles();
}
// virtual methods
const char *MSTraceSet::formatOutput(MSString &buffer_,unsigned,unsigned) {return buffer_.string();}
void MSTraceSet::selectable(MSBoolean selectable_)
{_selectable=selectable_;} 
void MSTraceSet::constraint(unsigned long constraint_)
{_constraint=constraint_;}
MSBoolean MSTraceSet::selectable(void) const
{return _selectable;} 
unsigned long MSTraceSet::constraint(void) const
{return _constraint;}
void MSTraceSet::unmap(void) {}
unsigned MSTraceSet::textLength(void) const
{return _text.length();}
MSStringVector& MSTraceSet::text(void)
{return _text;}
const MSStringVector& MSTraceSet::text(void) const
{return _text;}
MSUnsignedVector MSTraceSet::lineWidth(void) const
{
  MSUnsignedVector vector;
  for (int i=0; i<traceList().count(); i++) vector<<(unsigned)trace(i)->lineWidth();
  return vector;
}
MSUnsignedVector MSTraceSet::symbolSize(void) const
{
  MSUnsignedVector vector;
  for (int i=0; i<traceList().count(); i++) vector<<(unsigned)trace(i)->symbolSize();
  return vector;
}

MSUnsignedVector MSTraceSet::lineWeight(void) const
{
  MSUnsignedVector vector;
  for (int i=0; i<traceList().count(); i++) vector<<(unsigned)trace(i)->lineWeight();
  return vector;
}

void MSTraceSet::xOrigin(double) {}
void MSTraceSet::yOrigin(double) {}
void MSTraceSet::create(void) {}
void MSTraceSet::map(void) {}
void MSTraceSet::setData(void) {}
void MSTraceSet::validate(int,int,double,double){}
MSBoolean MSTraceSet::moveTraceValidate(double,double)
{ return MSFalse; }
MSTraceSet* MSTraceSet::copyTraceValidate(MSGraph*,const char*, const MSSymbol&)
{ return (MSTraceSet*)0; }
int MSTraceSet::numColumns(void) const
{return 1;}
int MSTraceSet::dataCount(void) const
{return 0;}
double MSTraceSet::x(int index_) const
{return (double)index_;}     
double MSTraceSet::y(int,int) const
{return 0.0;}     

MSBoolean MSTraceSet::mapped(void) const
{ return  owner()!=0?owner()->mapped():MSFalse; }

void MSTraceSet::hide(void)    
{
  int ct=traceList().count();
  for (int i=0;i<ct;i++) trace(i)->hidden(MSTrue);
  graph()->redrawImmediately(); 
}

void MSTraceSet::show(void)    
{
  int ct=traceList().count();
  for (int i=0;i<ct;i++) trace(i)->hidden(MSFalse);
  graph()->redrawImmediately(); 
}

void MSTraceSet::deleteTraces(void)
{
  MSTrace *pTrace=0;
  for (int i=traceList().count();i>0;i--)
   {
     if ((pTrace=trace(i-1))!=0)
      {
	traceList().remove(pTrace); 
	if (graph()->selectTrace()!=0&&pTrace==graph()->selectTrace()) graph()->selectTrace(0);
	graph()->traceList().remove(pTrace); 
	delete pTrace;
      }
   }
  lineColors().removeAll();
  fillColors().removeAll();
  graph()->legend()->redraw();
  graph()->redrawImmediately();
}

void MSTraceSet::computeExtents(const MSIndexVector& iv_)
{
  MSTrace* pTrace;
  unsigned int length=iv_.length();
  MSBoolean append=MSTrue;   
  MSBoolean needNormalize=MSFalse;
  unsigned cols=numColumns();   

  // optimize for append, otherwise result in computeExtents() 
  if (lastDataCount()==0||dataCount()<=lastDataCount()) append=MSFalse;
  if (append==MSTrue)
   {
     for (unsigned i=0;i<length;i++)
      {
        unsigned row = iv_(i)/cols;
        unsigned col = iv_(i)%cols;
        if (row+1<=lastDataCount())
         {
           append=MSFalse;
           break;
         }
        else
         {
           int tcount=cols<=1?0:col>0?col-1:(unsigned int)-1;
           if (tcount>=0&&tcount<traceList().count()&&(pTrace=trace(tcount))!=0)
            {
              double yy=pTrace->y(row);
              if (yy>=DBL_MAX||yy<=-DBL_MAX) yy=0;
              if (yy<pTrace->yMin()) pTrace->yMin(yy);
              if (yy>pTrace->yMax()) pTrace->yMax(yy);
              if (yy<yMin()) yMin(pTrace->yMin());
              if (yy>yMax()) yMax(pTrace->yMax());
            }
           double xx=x(row);
           if (row>0&&overlap()!=MSTrue&&xx<x(row-1)) overlap(MSTrue);
           if (xx>=DBL_MAX||xx<=-DBL_MAX) xx=0;
           if (xx<xMin()) xMin(xx);
           if (xx>xMax()) xMax(xx);
           if (col==0) needNormalize=MSTrue;
         }
      }
     if (needNormalize==MSTrue) graph()->computeNormalizedOffsets();
   }
  if (append==MSFalse) computeExtents();
}

void MSTraceSet::computeExtents(void)
{
  MSTrace *pTrace=trace(0);
  overlap(MSFalse);
  int count=dataCount();
  for (unsigned i=0;i<traceList().count();i++)
   {
     if (i==0&&count>0&&(pTrace=trace(i))!=0)
      {
        double yy=pTrace->y(0);
	_yMin=_yMax=yy>=DBL_MAX||yy<=-DBL_MAX?0:yy;
	double xx=pTrace->x(0);
	_xMin=_xMax=xx>=DBL_MAX||xx<=-DBL_MAX?0:xx;
      }
     if ((pTrace=trace(i))!=0)
      {
	for (unsigned k=0;k<count;k++)
	 {
           double yy=pTrace->y(0);
           if (yy>=DBL_MAX||yy<=-DBL_MAX) yy=0;
	   if (k==0) pTrace->_yMin=pTrace->_yMax=yy;
	   if (pTrace->style()>Close)
	    {
	      int cols=numColumns()>1?numColumns()-1:1;
	      for (int j=0;j<cols;j++)
	       {
		 double yy=pTrace->y(k,j);
		 if (yy>=DBL_MAX||yy<=-DBL_MAX) yy=0;
		 if (yy<pTrace->yMin()) pTrace->yMin(yy);
		 if (yy>pTrace->yMax()) pTrace->yMax(yy);
	       }
	    }
	   else
	    {
	      double yy=pTrace->y(k);
	      if (yy>=DBL_MAX||yy<=-DBL_MAX) yy=0;
	      if (yy<pTrace->yMin()) pTrace->yMin(yy);
	      if (yy>pTrace->yMax()) pTrace->yMax(yy);
	    }
	 }
	if (pTrace->yMin()<yMin()) yMin(pTrace->yMin());
	if (pTrace->yMax()>yMax()) yMax(pTrace->yMax());
      }
   }
  if (count>0)
   {
     double xv,xx=x(0);
     for (unsigned k=0;k<count;k++)
      {
	xv=x(k);
	overlap(overlap()==MSTrue||xv<xx?MSTrue:MSFalse);
        if (xv>=DBL_MAX||xv<=-DBL_MAX) xv=0;
	if (xv<xMin()) xMin(xv);
	if (xv>xMax()) xMax(xv);
	xx=xv;
      }
   }
  graph()->computeNormalizedOffsets();
}

const char *MSTraceSet::legend(unsigned i_)
{
  int ct=legend().length();
  return ct>0?legend()(i_<ct?i_:ct-1).string():0;
}

const char *MSTraceSet::formatText(MSString &buffer_,unsigned i_)
{
  if (i_<text().length()) buffer_=text()(i_);
  return buffer_.string();
}

void MSTraceSet::validateText(const MSStringVector x_)
{text()=x_;}

const MSStringVector& MSTraceSet::textSymbol(unsigned i_)
{
  int ct=traceList().count();
  return trace(i_<ct?i_:ct-1)->textSymbol(); 
}

Font MSTraceSet::font(unsigned i_) const
{
  int ct=traceList().count();
  return trace(i_<ct?i_:ct-1)->font(); 
}

Pixmap MSTraceSet::stipple(unsigned i_) const
{
  int ct=traceList().count();
  return trace(i_<ct?i_:ct-1)->stipple(); 
}

unsigned long MSTraceSet::lineStyle(unsigned i_) const
{
  int ct=traceList().count();
  return trace(i_<ct?i_:ct-1)->lineStyle(); 
}

MSAlignment MSTraceSet::xAxis(unsigned i_) const
{
  int ct=traceList().count();
  return trace(i_<ct?i_:ct-1)->xAxis()==1?MSTop:MSBottom; 
}

MSAlignment MSTraceSet::yAxis(unsigned i_) const
{
  int ct=traceList().count();
  return trace(i_<ct?i_:ct-1)->yAxis()==1?MSRight:MSLeft; 
}

unsigned long MSTraceSet::style(unsigned i_) const
{
  int ct=traceList().count();
  return trace(i_<ct?i_:ct-1)->style(); 
}

unsigned long MSTraceSet::symbol(unsigned i_) const
{
  int ct=traceList().count();
  return trace(i_<ct?i_:ct-1)->symbol(); 
}

unsigned MSTraceSet::lineWidth(unsigned i_) const
{
  int ct=traceList().count();
  return trace(i_<ct?i_:ct-1)->lineWidth(); 
}

unsigned MSTraceSet::symbolSize(unsigned i_) const
{
  int ct=traceList().count();
  return trace(i_<ct?i_:ct-1)->symbolSize(); 
}

unsigned MSTraceSet::lineWeight(unsigned i_) const
{
  int ct=traceList().count();
  return trace(i_<ct?i_:ct-1)->lineWeight(); 
}

unsigned long MSTraceSet::lineColor(unsigned,unsigned) const 
{return 0;}

unsigned long MSTraceSet::lineColor(unsigned i_) const 
{
  unsigned len=lineColors().length();
  if (len>0) return lineColors()(i_<len?i_:len-1); 
  else return len;
}

unsigned long MSTraceSet::fillColor(unsigned,unsigned) const
{return 0;}

unsigned long MSTraceSet::fillColor(unsigned i_) const
{
  unsigned len=fillColors().length();
  if (len>0) return fillColors()(i_<len?i_:len-1); 
  else return len;
}

unsigned long MSTraceSet::traceLineColor(int,int c_) const
{
  unsigned len=lineColors().length();
  if (len>0) return lineColors()(c_<len?c_:len-1); 
  else return len;
}

unsigned long MSTraceSet::traceFillColor(int r_,int c_) const
{
  unsigned len=fillColors().length();
  unsigned index=style(0)&Pie?r_:c_;
  if (len>0) return fillColors()(index<len?index:len-1); 
  else return len;
}

void MSTraceSet::pieLegendAlignment(unsigned long align_)
{
  if (align_!=_pieLegendAlignment)
   {
     _pieLegendAlignment=align_;
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::pieValueAlignment(unsigned long align_)
{
  if (align_!=_pieValueAlignment)
   {
     _pieValueAlignment=align_;
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::piePercentAlignment(unsigned long align_)
{
  if (align_!=_piePercentAlignment)
   {
     _piePercentAlignment=align_;
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::pieOffsets(double x_)
{
  if (x_>=0.0&&x_<=100.0)
   {
     if (x_!=_pieOffset)
      {
        _pieOffset=x_;
        graph()->redrawImmediately(); 
      }
   }
  else
   {
     MSMessageLog::warningMessage("Warning:  MSTraceSet::pieOffsets value out of range");
   }
}

void MSTraceSet::pieOffsets(const MSFloatVector& x_)
{
  for (unsigned i=0;i<x_.length();i++) if (x_(i)<0.0||x_(i)>100.0)
   {
     MSMessageLog::warningMessage("Warning:  MSTraceSet::pieOffsets value out of range");
   }
  if (pieOffsets()==0) _pieOffsets=new MSFloatVector(x_);
  else *pieOffsets()=x_;
  graph()->redrawImmediately(); 
}

double MSTraceSet::pieOffset(unsigned i_) const
{
  double offset=_pieOffset;
  if (pieOffsets()!=0&&pieOffsets()->length()>0) offset=(*pieOffsets())(i_%pieOffsets()->length());
  return offset>1.0?offset<=100.0?offset/100.0:0.0:offset;
}

void MSTraceSet::pieProfiles(const MSFloatVector& x_)
{
  for (unsigned i=0;i<x_.length();i++) if (x_(i)<0.0||x_(i)>100.0)
   {
     MSMessageLog::warningMessage("Warning:  MSTraceSet::pieProfiles value out of range");
   }
  if (pieProfiles()==0) _pieProfiles=new MSFloatVector(x_);
  else *pieProfiles()=x_;
  graph()->redrawImmediately(); 
}

double MSTraceSet::pieProfile(unsigned i_) const
{
  double profile=1.0;
  if (pieProfiles()!=0&&pieProfiles()->length()>0) profile=(*pieProfiles())(i_%pieProfiles()->length());
  return profile>1.0?profile<100.0?profile/100.0:1.0:profile;
}

void MSTraceSet::pieDepthFactor(double x_)
{
  if (x_>0.0&&x_<=6.0)
   {
     _pieDepthFactor=x_;
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::pieAspectRatio(double x_)
{
  if (x_>0.0&&x_<=1.0)
   {
     _pieAspectRatio=x_;
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::primarySlice(int x_)
{
  if (x_!=primarySlice())
   {
     _primarySlice=x_;
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::primarySliceAlignment(MSAlignment x_)
{
  if (x_!=primarySliceAlignment())
   {
     _primarySliceAlignment=x_;
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::pieAngle(int x_)
{
  if (x_!=pieAngle())
   {
     _pieAngle=x_%360;
     if (primarySliceAlignment()==MSNone) graph()->redrawImmediately(); 
   }
}

void MSTraceSet::legend(const char *x_,unsigned i_)    
{
  if (i_<traceList().count()&&legend()(i_)!=x_) 
   {
     legend()[i_]=x_; 
     graph()->drawLegends(); 
   }
}

void MSTraceSet::textSymbol(const MSStringVector& x_,unsigned i_)    
{
  if (i_<traceList().count())
   {
     trace(i_)->textSymbol(x_); 
     graph()->updateLegendStatus(MSTrue); 
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::font(Font x_,unsigned i_)    
{
  if (i_<traceList().count())
   {
     trace(i_)->font(x_); 
     graph()->legend()->redraw(); 
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::font(const char *x_,unsigned i_)    
{
  if (i_<traceList().count())
   {
     trace(i_)->font(x_); 
     graph()->legend()->redraw(); 
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::stipple(Pixmap x_,unsigned i_)    
{
  if (i_<traceList().count())
   {
     trace(i_)->stipple(x_); 
     graph()->updateLegendStatus(MSTrue); 
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::lineWidth(unsigned x_,unsigned i_)    
{
  if (i_<traceList().count()&&trace(i_)->lineWidth()!=x_) 
   {
     x_=x_<MaxTraceLineWidth?x_:MaxTraceLineWidth;
     trace(i_)->lineWidth(x_); 
     graph()->updateLegendStatus(MSTrue); 
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::lineWeight(unsigned x_,unsigned i_)    
{
  if (i_<traceList().count()&&trace(i_)->lineWeight()!=x_) 
   {
     trace(i_)->lineWeight(x_); 
     graph()->updateLegendStatus(MSTrue); 
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::lineStyle(unsigned long x_,unsigned i_)    
{
  if (i_<traceList().count()&&trace(i_)->lineStyle()!=x_) 
   {
     trace(i_)->lineStyle(x_); 
     graph()->updateLegendStatus(MSTrue); 
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::xAxis(MSAlignment x_,unsigned i_)    
{
  int x=MSTop&x_?1:0;
  if (i_<traceList().count()&&trace(i_)->xAxis()!=x) 
   {
     trace(i_)->xAxis(x); 
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::yAxis(MSAlignment x_,unsigned i_)    
{
  int x=MSRight&x_?1:0;
  if (i_<traceList().count()&&trace(i_)->yAxis()!=x) 
   {
     trace(i_)->yAxis(x); 
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::style(unsigned long x_,unsigned i_)    
{
  if (i_<traceList().count()&&trace(i_)->style()<Close&&x_<Close) 
   {
     trace(i_)->style(x_); 
     graph()->legend()->redraw(); 
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::symbol(unsigned long x_,unsigned i_)    
{
  if (i_<traceList().count()&&trace(i_)->symbol()!=x_) 
   {
     trace(i_)->symbol(x_); 
     graph()->updateLegendStatus(MSTrue); 
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::symbolSize(unsigned x_,unsigned i_)    
{
  if (i_<traceList().count()&&trace(i_)->symbolSize()!=x_) 
   {
     x_=x_<MaxTraceSymbolSize?x_:MaxTraceSymbolSize;
     trace(i_)->symbolSize(x_); 
     graph()->updateLegendStatus(MSTrue); 
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::lineColor(unsigned long x_,unsigned i_)
{
  unsigned len=lineColors().length();
  if (i_<traceList().count()&&i_<len&&lineColors()(i_)!=x_) 
   {
     lineColors()[i_]=x_; 
     graph()->updateLegendStatus(MSTrue); 
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::fillColor(unsigned long x_,unsigned i_)    
{
  unsigned len=fillColors().length();
  if (i_<traceList().count()&&i_<len&&fillColors()(i_)!=x_) 
   {
     fillColors()[i_]=x_; 
     graph()->updateLegendStatus(MSTrue); 
     graph()->redrawImmediately(); 
   }
}

void MSTraceSet::legend(const char *x_)    
{
  if (x_!=0)
   {
     legend().removeAll();
     for (int i=0;i<traceList().count();i++) legend()<<x_;
     graph()->legend()->redraw(); 
   }
}

void MSTraceSet::font(Font x_)    
{
  for (int i=0; i<traceList().count(); i++) trace(i)->font(x_);
  graph()->redrawImmediately(); 
}

void MSTraceSet::font(const char *x_)    
{
  for (int i=0; i<traceList().count(); i++) trace(i)->font(x_);
  graph()->redrawImmediately(); 
}

void MSTraceSet::stipple(Pixmap x_)    
{
  for (int i=0; i<traceList().count(); i++) trace(i)->stipple(x_);
  graph()->redrawImmediately(); 
}

void MSTraceSet::lineWidth(unsigned x_)    
{
  for (int i=0; i<traceList().count(); i++) trace(i)->lineWidth(x_);
  graph()->updateLegendStatus(MSTrue);
  graph()->redrawImmediately(); 
}

void MSTraceSet::lineWeight(unsigned x_)    
{
  for (int i=0; i<traceList().count(); i++) trace(i)->lineWeight(x_);
  graph()->updateLegendStatus(MSTrue);
  graph()->redrawImmediately(); 
}

void MSTraceSet::lineStyle(MSLineStyle x_)    
{ lineStyle((unsigned long)x_); }
void MSTraceSet::lineStyle(unsigned long x_)    
{
  for (int i=0; i<traceList().count(); i++) trace(i)->lineStyle(x_);
  graph()->updateLegendStatus(MSTrue);
  graph()->redrawImmediately(); 
}

void MSTraceSet::xAxis(MSAlignment x_)    
{
  for (int i=0; i<traceList().count(); i++) trace(i)->xAxis(MSTop&x_?1:0);
  graph()->redrawImmediately(); 
}

void MSTraceSet::yAxis(MSAlignment x_)    
{
  for (int i=0; i<traceList().count(); i++) trace(i)->yAxis(MSRight&x_?1:0);
  graph()->redrawImmediately(); 
}

void MSTraceSet::style(Style x_)    
{ style((unsigned long)x_); }

void MSTraceSet::style(unsigned long x_)    
{
  updateTraceStyle(x_);
  graph()->legend()->redraw(); 
  graph()->redrawImmediately(); 
}

void MSTraceSet::symbol(Symbol x_)    
{ symbol((unsigned long)x_); }

void MSTraceSet::symbol(unsigned long x_)    
{
  for (int i=0; i<traceList().count(); i++) trace(i)->symbol(x_);
  graph()->updateLegendStatus(MSTrue);
  graph()->redrawImmediately(); 
}

void MSTraceSet::symbolSize(unsigned x_)
{
  for (int i=0; i<traceList().count(); i++) trace(i)->symbolSize(x_);
  graph()->updateLegendStatus(MSTrue);
  graph()->redrawImmediately(); 
}

void MSTraceSet::lineColor(unsigned long x_)    
{
  lineColors()=x_;
  graph()->updateLegendStatus(MSTrue); 
  graph()->redrawImmediately(); 
}

void MSTraceSet::fillColor(unsigned long x_)    
{
  fillColors()=x_;
  graph()->updateLegendStatus(MSTrue); 
  graph()->redrawImmediately(); 
}

void MSTraceSet::legend(const MSStringVector& x_)    
{
  if (x_!=legend())
   {
     _legend=x_;
     if (traceList().count()>0&&trace(0)->style()==MSG::Pie)
      {
        graph()->updateLegendStatus(MSTrue); 
        graph()->redrawImmediately(); 
      }
     else graph()->legend()->redraw();
   }
}

void MSTraceSet::textSymbol(const MSStringVector& x_)    
{
  for (int i=0; i<traceList().count(); i++) trace(i)->textSymbol(MSStringVector(x_[i%x_.length()]));
  graph()->updateLegendStatus(MSTrue); 
  graph()->redrawImmediately(); 
}

void MSTraceSet::font(const MSUnsignedLongVector& x_)    
{
  for (int i=0; i<traceList().count(); i++) trace(i)->font(x_[i%x_.length()]);
  graph()->updateLegendStatus(MSTrue); 
  graph()->redrawImmediately(); 
}

void MSTraceSet::font(const MSStringVector& x_)    
{
  for (int i=0; i<traceList().count(); i++) trace(i)->font(x_[i%x_.length()]);
  graph()->updateLegendStatus(MSTrue); 
  graph()->redrawImmediately(); 
}

void MSTraceSet::stipple(const MSUnsignedLongVector& x_)    
{
  for (int i=0; i<traceList().count(); i++) trace(i)->stipple((Pixmap)x_[i%x_.length()]);
  graph()->updateLegendStatus(MSTrue); 
  graph()->redrawImmediately(); 
}

void MSTraceSet::lineWidth(const MSUnsignedVector& x_)    
{
  for (int i=0; i<traceList().count(); i++) trace(i)->lineWidth(x_[i%x_.length()]);
  graph()->updateLegendStatus(MSTrue); 
  graph()->redrawImmediately(); 
}

void MSTraceSet::lineWeight(const MSUnsignedVector& x_)    
{
  for (int i=0; i<traceList().count(); i++) trace(i)->lineWeight(x_[i%x_.length()]);
  graph()->updateLegendStatus(MSTrue); 
  graph()->redrawImmediately(); 
}

void MSTraceSet::lineStyle(const MSUnsignedLongVector& x_)    
{
  for (int i=0; i<traceList().count(); i++) trace(i)->lineStyle(x_[i%x_.length()]);
  graph()->updateLegendStatus(MSTrue); 
  graph()->redrawImmediately(); 
}

void MSTraceSet::xAxis(const MSUnsignedLongVector& x_, MSExplicitInit)
{
  for (int i=0; i<traceList().count(); i++) trace(i)->xAxis(MSTop&x_[i%x_.length()]?1:0);
  graph()->redrawImmediately(); 
}

void MSTraceSet::yAxis(const MSUnsignedLongVector& x_, MSExplicitInit)
{
  for (int i=0; i<traceList().count(); i++) trace(i)->yAxis(MSRight&x_[i%x_.length()]?1:0);
  graph()->redrawImmediately(); 
}

void MSTraceSet::style(const MSUnsignedLongVector& x_)    
{
  int ct=traceList().count();
  if (ct>0)
   {
     if (trace(0)->style()>=Close) style(x_(0));
     if (x_(0)>=Close) style(x_(0));
     else for (int i=0;i<ct;i++) if (x_[i%x_.length()]<Close) trace(i)->style(x_[i%x_.length()]);
   }
  graph()->legend()->redraw(); 
  graph()->redrawImmediately(); 
}

void MSTraceSet::symbol(const MSUnsignedLongVector& x_)    
{
  for (int i=0; i<traceList().count(); i++) trace(i)->symbol(x_[i%x_.length()]);
  graph()->updateLegendStatus(MSTrue); 
  graph()->redrawImmediately(); 
}

void MSTraceSet::symbolSize(const MSUnsignedVector& x_)    
{
  for (int i=0; i<traceList().count(); i++) trace(i)->symbolSize(x_[i%x_.length()]);
  graph()->updateLegendStatus(MSTrue); 
  graph()->redrawImmediately(); 
}

void MSTraceSet::lineColor(const MSUnsignedLongVector& x_)    
{
  lineColors()=x_;
  graph()->updateLegendStatus(MSTrue); 
  graph()->redrawImmediately(); 
}

void MSTraceSet::fillColor(const  MSUnsignedLongVector& x_)    
{
  fillColors()=x_;
  graph()->updateLegendStatus(MSTrue); 
  graph()->redrawImmediately(); 
}

void MSTraceSet::lineColor(const MSStringVector& x_)    
{
  MSUnsignedLongVector colors;
  for (unsigned i=0;i<x_.length();i++) colors<<owner()->server()->pixel(x_(i));
  lineColors()=colors;
  graph()->updateLegendStatus(MSTrue); 
  graph()->redrawImmediately(); 
}

void MSTraceSet::fillColor(const MSStringVector& x_)    
{
  MSUnsignedLongVector colors;
  for (unsigned i=0;i<x_.length();i++) colors<<owner()->server()->pixel(x_(i));
  fillColors()=colors;
  graph()->updateLegendStatus(MSTrue); 
  graph()->redrawImmediately(); 
}

void MSTraceSet::lineColor(const char *x_)    
{
  lineColors()=owner()->server()->pixel(x_); 
  graph()->updateLegendStatus(MSTrue); 
  graph()->redrawImmediately();
}

void MSTraceSet::fillColor(const char *x_)    
{
  fillColors()=owner()->server()->pixel(x_); 
  graph()->updateLegendStatus(MSTrue); 
  graph()->redrawImmediately();
}

void MSTraceSet::updateTraceStyle(unsigned long style_)
{
  MSTrace      	       *pTrace=trace(0);	
  int 			cols=numColumns();
  int 			designateCol=0;
  int 			offset=0;
  MSBoolean 		updateTrace=MSFalse,styleError=MSFalse;

  if (pTrace!=0)
   {
     pTrace->virtualCol(0);
     if (style_>=Close)
      {
	if (style_==HLOC||style_==Candle)
	 {
	   style_=cols>=5?style_:cols>3?HLC:cols>2?HL:cols>1?Close:Line;
	 }
	else if (style_==HLC)
	 {
	   style_=cols>=4?style_:cols>2?HL:cols>1?Close:Line;
	 }
	else if (style_==HL)
	 {
	   style_=cols>=3?style_:cols>1?Close:Line;
	 }
	else if (style_>=Close&&cols>2)
	 {
	   updateTrace=MSTrue; 
	   if (cols==3) style_=HL;
	 }
	updateTrace=style_!=Line?MSTrue:MSFalse; 
	styleError=style_==Line?MSTrue:MSFalse; 
	offset=cols>4?1:0;
      }
     if (updateTrace==MSTrue)
      {
	for (int i=traceList().count()-1;i>-1;i--)
	 {
	   if ((pTrace=trace(i))!=0)
	    {
	      if (pTrace->virtualCol()!=designateCol)
	       {
		 traceList().remove(pTrace); 
		 graph()->traceList().remove(pTrace); 
                 lineColors()=lineColors().drop(-1);
                 fillColors()=fillColors().drop(-1);
                 delete pTrace; 
	       }
	      else
	       {
		 pTrace->style(style_);
		 pTrace->offset(offset);
		 if (style_==Close) pTrace->virtualCol(cols>=5?3:cols==4?2:1);
	       }
	    }  
	 }  
	computeExtents();
      }
     else
      {
	configureTraces();
	if (styleError!=MSTrue)
	 {
	   for (int i=0;i<traceList().count();i++)
	    {
	      if ((pTrace=trace(i))!=0)
	       {
		 pTrace->xShift(0.0);
		 pTrace->style(style_);
	       }
	    }
	 }
      }
   }
  else configureTraces();
}

void MSTraceSet::update(const MSIndexVector& iv_)
{
  unsigned long style=trace(0)!=0?trace(0)->style():MSG::Line;
  if (iv_.length()==0)
   {
     if (trace(0)==0||style>=Close) updateTraceStyle(style);
     else configureTraces();
   }
  else
   {
     computeExtents(iv_);
   }
  graph()->update(this,iv_);
}

// called when a model is bound to this widget or is unbound from this widget
void MSTraceSet::updateData(void)
{
  configureTraces();
  graph()->redrawImmediately();
  graph()->legend()->redraw();
}

void MSTraceSet::configureTraces(void)
{
  if (hasModel()==MSFalse)
   {
     deleteTraces();
   }
  else
   {
     MSTrace    *pTrace=0;
     int         cols=numColumns()>1?numColumns()-1:numColumns();
     int         numTraces=traceList().count();
     
     for (int i=traceList().count()-1;i>-1;i--)
      {
	if ((pTrace=trace(i))!=0)
	 {
	   if (pTrace->virtualCol()>=cols)
	    {
	      traceList().remove(pTrace); 
	      graph()->traceList().remove(pTrace);
              lineColors()=lineColors().drop(-1);
              fillColors()=fillColors().drop(-1);
	      delete pTrace;
	    }
	   else pTrace->virtualCol(i);
	 }  
      }  
     if (cols>traceList().count())
      {
	int ll=graph()->defaultTraceLineColors().length();
	int lf=graph()->defaultTraceFillColors().length();
	int ct=graph()->traceList().count();
	for (int i=traceList().count();i<cols;i++,ct++)
	 {
	   pTrace=new MSTrace(this,i,tag());
	   graph()->traceList().add(pTrace);
	   traceList().add(pTrace); 
	   lineColors()<<graph()->server()->pixel(graph()->defaultTraceLineColors()(ct%ll));
	   fillColors()<<graph()->server()->pixel(graph()->defaultTraceFillColors()(ct%lf));
	 }   
      }   
   }
  computeExtents();
  lastDataCount(dataCount());
}

void MSTraceSet::selected(unsigned row_,unsigned col_)
{
  if (row_>0&&col_<numColumns())
   { 
     selectRow(row_);
     selectCol(col_);
     graph()->highlightPoint(this);
   }
}

void MSTraceSet::format(const MSFormat& aFormat_)
{
  if(format()!=aFormat_)
   {
     if(format().formatType()==MSFormat::NoFormat||
        aFormat_.formatType()==format().formatType())
      {
        _format=aFormat_;
        graph()->redrawImmediately();
      }
   }
}

static const char * traceStyleNames =
"MSNone\nMSG::Area\nMSG::Bar\nMSG::Candle\nMSG::Close\nMSG::ColorProfile\nMSG::Fill\nMSG::HL\n\
MSG::HLC\nMSG::HLOC\nMSG::Line\nMSG::Line|MSG::Scatter\nMSG::MarketProfile\nMSG::Outline\n\
MSG::Pie\nMSG::Scatter\nMSG::Segment\nMSG::Stack\nMSG::Step\nMSG::Step|MSG::Scatter\nMSG::Text";


static int traceStyleValues[]= {
MSNone,MSG::Area,MSG::Bar,MSG::Candle,MSG::Close,MSG::ColorProfile,MSG::Fill,MSG::HL,
MSG::HLC,MSG::HLOC,MSG::Line,MSG::Line|MSG::Scatter,MSG::MarketProfile,MSG::Outline,
MSG::Pie,MSG::Scatter,MSG::Segment,MSG::Stack,MSG::Step,MSG::Step|MSG::Scatter,MSG::Text};


static const char *symbolNames =
"MSNone\nMSG::Circle\nMSG::Circle|MSG::Fill\nMSG::Cross\nMSG::Diamond\nMSG::Diamond|MSG::Fill\nMSG::Square\n\
MSG::Square|MSG::Fill\nMSG::Triangle\nMSG::Triangle|MSG::Fill\nMSG::X\nMSG::Cross|MSG::X";

static int symbolValues[] =
{MSNone,MSG::Circle,MSG::Circle|MSG::Fill,MSG::Cross,MSG::Diamond,MSG::Diamond|MSG::Fill,MSG::Square,
MSG::Square|MSG::Fill,MSG::Triangle,MSG::Triangle|MSG::Fill,MSG::X,MSG::Cross|MSG::X};


void MSTraceSet::set(MSAttrValueList& avList_)
{
  MSWidgetView::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="legend")
      {
        legend(MSAttrValue::stringToStringVector(avList_[i].value()));
        index<<i;
      }
     else if(avList_[i].attribute()=="style")
      {
        int ct = traceList().count();
        if(ct>0)
         {
           MSStringVector aStyleVector(traceStyleNames);
           unsigned index =aStyleVector.indexOf(avList_[i].value());
           if(index!=aStyleVector.length()) style(traceStyleValues[index]);
         }
        index<<i;
      }
     else if(avList_[i].attribute()=="symbol")
      {
        int ct = traceList().count();
        if(ct>0)
         {
           MSStringVector aSymbolVector(symbolNames);
           unsigned index =aSymbolVector.indexOf(avList_[i].value());
           if(index!=aSymbolVector.length()) symbol(symbolValues[index]);
         }
        index<<i;
      }
     else if(avList_[i].attribute()=="lineColor")
      {
        lineColor(MSAttrValue::stringToStringVector(avList_[i].value()));
        index<<i;
      }
     else if(avList_[i].attribute()=="fillColor")
      {
        fillColor(MSAttrValue::stringToStringVector(avList_[i].value()));
        index<<i;
      }
     else if(avList_[i].attribute()=="selectable")
      {
	selectable(avList_[i].value().asBoolean());
	index<<i;
      }
     else if(avList_[i].attribute()=="constraint")
      {
	if(avList_[i].value()=="MSG::HoldX") constraint(HoldX);
	else if(avList_[i].value()=="MSG::HoldY") constraint(HoldY);
	else constraint(0);
	index<<i;
      }
     else if(avList_[i].attribute()=="pieDepthFactor")
      {
	pieDepthFactor(avList_[i].value().asDouble());
	index<<i;
      }
     else if(avList_[i].attribute()=="pieAspectRatio")
      {
	pieAspectRatio(avList_[i].value().asDouble());
	index<<i;
      }
     else if(avList_[i].attribute()=="pieAngle")
      {
	pieAngle(avList_[i].value().asInt());
	index<<i;
      }
     else if(avList_[i].attribute()=="primarySlice")
      {
	primarySlice(avList_[i].value().asInt());
	index<<i;
      }
     else if(avList_[i].attribute()=="primarySliceAlignment")
      {
	primarySliceAlignment((MSAlignment)MSAttrValue::stringToAlignment(avList_[i].value()));
	index<<i;
      }
     else if(avList_[i].attribute()=="pieLegendAlignment")
      {
	pieLegendAlignment(MSAttrValue::stringToAlignment(avList_[i].value()));
	index<<i;
      }
     else if(avList_[i].attribute()=="pieValueAlignment")
      {
	pieValueAlignment(MSAttrValue::stringToAlignment(avList_[i].value()));
	index<<i;
      }
     else if(avList_[i].attribute()=="piePercentAlignment")
      {
	piePercentAlignment(MSAttrValue::stringToAlignment(avList_[i].value()));
	index<<i;
      }
     else if (avList_[i].attribute()=="format")
      {
        format(MSFormat(avList_[i].value()));
        index<<i;
      }
     else if (avList_[i].attribute()=="symbolSize")
      {
        symbolSize((unsigned)avList_[i].value().asInt());
        index<<i;
      }     
     else if (avList_[i].attribute()=="piePercentFormat")
      {
        MSString pieFormat = avList_[i].value();
        pieFormat.change("MSFloat::","");
        piePercentFormat(MSFormat(pieFormat).floatFormat());
        index<<i;
      }
     else if (avList_[i].attribute()=="xAxis")
      {
        xAxis((MSAlignment)MSAttrValue::stringToAlignment(avList_[i].value()));
        index<<i;
      }
     else if (avList_[i].attribute()=="yAxis")
      {
        yAxis((MSAlignment)MSAttrValue::stringToAlignment(avList_[i].value()));
        index<<i;
      }
     else if(avList_[i].attribute()=="lineStyle")
      {
        unsigned long newLineStyle;
        if(avList_[i].value()=="MSSolid") newLineStyle=MSSolid;
        else if(avList_[i].value()=="MSDash") newLineStyle=MSDash;
        else if(avList_[i].value()=="MSDot") newLineStyle=MSDot;
        else newLineStyle=MSDash|MSDot;
        lineStyle(newLineStyle);
        index<<i;
      }
     else if(avList_[i].attribute()=="lineWidth")
      {
        lineWidth((unsigned)avList_[i].value().asInt());
        index<<i;
      }
     else if(avList_[i].attribute()=="lineWeight")
      {
        lineWeight((unsigned)avList_[i].value().asInt());
        index<<i;
      }
     else if(avList_[i].attribute()=="tag")
      {
        if(avList_[i].value().length()==0) tag(MSSymbol::nullSymbol());
        else tag(MSSymbol(avList_[i].value()));
        index<<i;
      }
   }
  avList_.remove(index);
}

MSAttrValueList& MSTraceSet::get(MSAttrValueList& avList_)
{
  MSStringVector aBoolVector("MSFalse\nMSTrue");
  avList_<<MSAttrValue("tag",tag().symbolName(),MSAttrValue::Control|MSAttrValue::String);
  MSStringVector alignmentVector("MSNone\nMSCenter\nMSTop\nMSBottom\nMSLeft\nMSRight"); 
  int i;
  MSString aString;
  const char *value;
  MSStringVector aConstraintVector("MSNone\nMSG::HoldX\nMSG::HoldY");
  unsigned aSize;
  int ct = traceList().count();

  switch(constraint())
   {
   case HoldX: value="MSG::HoldX"; break;
   case HoldY: value="MSG::HoldY"; break;
   default: value="MSNone"; break;
   }
  avList_<<MSAttrValue("constraint",value,aConstraintVector);
  avList_<<MSAttrValue("fillColor",MSAttrValue::colorVectorToString(fillColors(),server()),MSAttrValue::Color|MSAttrValue::List|MSAttrValue::StringVector);
  avList_<<MSAttrValue("legend",MSAttrValue::stringVectorToString(legend()),MSAttrValue::StringVector);
  avList_<<MSAttrValue("lineColor",MSAttrValue::colorVectorToString(lineColors(),server()),MSAttrValue::Color|MSAttrValue::List|MSAttrValue::StringVector);

  aSize=2;
  if(ct>0) aSize=((const MSTraceSet *)this)->lineWeight(0);
  avList_<<MSAttrValue("lineWeight",MSString(aSize));

  aSize=1;
  if(ct>0) aSize=((const MSTraceSet *)this)->lineWidth(0);
  avList_<<MSAttrValue("lineWidth",MSString(aSize));

  aSize=MSSolid;
  if(ct>0) aSize=((const MSTraceSet *)this)->lineStyle(0);
  MSStringVector aLineStyleVector("MSSolid\nMSDash\nMSDot\nMSDash|MSDot");
  int index;
  switch(aSize)
   {
   case MSSolid: index=0; break;
   case MSDash: index=1; break;
   case MSDot: index=2; break;
   case MSDash|MSDot: index=3; break;
   default: index=0; break;
   }
  avList_<<MSAttrValue("lineStyle",aLineStyleVector(index),aLineStyleVector);

  unsigned long traceStyle=MSNone;
  if(ct >0) traceStyle = ((const MSTraceSet *)this)->style(0);
  MSStringVector aStyleVector(traceStyleNames);
  for(i=0;i<aStyleVector.length();i++)
   {
     if(traceStyle==traceStyleValues[i])
      {
        aString=aStyleVector(i);
        break;
      }
   }
  avList_<<MSAttrValue("pieAngle",MSString(pieAngle()));
  avList_<<MSAttrValue("pieAspectRatio",MSString(pieAspectRatio()));
  avList_<<MSAttrValue("pieDepthFactor",MSString(pieDepthFactor()));
  avList_<<MSAttrValue("pieLegendAlignment",MSAttrValue::alignmentToString(pieLegendAlignment()),alignmentVector,MSAttrValue::List);
  avList_<<MSAttrValue("piePercentAlignment",MSAttrValue::alignmentToString(piePercentAlignment()),alignmentVector,MSAttrValue::List);
  MSFormat aFormat(piePercentFormat());
  MSStringVector pieFormats=aFormat.formats();
  for(i=0;i<pieFormats.length();i++)
   {
     pieFormats[i].insert("MSFloat::");
   }
  MSString pieFormat="MSFloat::" + aFormat.asString();
  
  avList_<<MSAttrValue("piePercentFormat",pieFormat,pieFormats);
  avList_<<MSAttrValue("pieValueAlignment",MSAttrValue::alignmentToString(pieValueAlignment()),alignmentVector,MSAttrValue::List);
  avList_<<MSAttrValue("primarySlice",MSString(primarySlice()));
  avList_<<MSAttrValue("primarySliceAlignment",MSAttrValue::alignmentToString(primarySliceAlignment()),alignmentVector,MSAttrValue::List);
  avList_<<MSAttrValue("selectable",aBoolVector(selectable()),aBoolVector);
  avList_<<MSAttrValue("style",aString,aStyleVector);

  unsigned long aSymbol=0;
  if(ct>0) aSymbol=((const MSTraceSet *)this)->symbol(0);
  MSStringVector aSymbolVector(symbolNames);
  aString="";
  for(i=0;i<aSymbolVector.length();i++)
   {
     if(aSymbol==symbolValues[i])
      {
        aString=aSymbolVector(i);
        break;
      }
   }
  avList_<<MSAttrValue("symbol",aString,aSymbolVector);

  if(ct>0) aSize=((const MSTraceSet *)this)->symbolSize(0);
  avList_<<MSAttrValue("symbolSize",MSString(aSize));
  
  MSAlignment alignment = MSNone;
  if(ct>0) alignment = xAxis(0);
  avList_<<MSAttrValue("xAxis",MSAttrValue::alignmentToString(alignment),MSStringVector("MSNone\nMSTop\nMSBottom"));

  alignment=MSNone;
  if(ct>0) alignment = yAxis(0);
  avList_<<MSAttrValue("yAxis",MSAttrValue::alignmentToString(alignment),MSStringVector("MSNone\nMSLeft\nMSRight"));

  
   avList_<<MSAttrValue("format",format().asString(),format().formats(),
                        MSAttrValue::String);  

   avList_<<MSAttrValue("traceptreference","",MSAttrValue::Callback);
   avList_<<MSAttrValue("tracereference","",MSAttrValue::Callback);

   return MSWidgetView::get(avList_);
}


MSFloatMatrix MSTraceSet::asFloatMatrix(void) const
{
  MSFloatMatrix fm;
  return fm;
}

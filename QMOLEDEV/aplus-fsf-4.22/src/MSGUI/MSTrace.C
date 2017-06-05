///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSTraceSet.H>
#include <MSGUI/MSGraph.H>
#include <MSGUI/MSTrace.H>

static const int DefaultTraceSymbolSize =11;

MSTrace::MSTrace(MSTraceSet *traceSet_,int col_,const MSSymbol& tag_): _tag(tag_)
{
  _traceSet=traceSet_;
  virtualCol(col_);
  init();
}

MSTrace::~MSTrace(void) {}

void MSTrace::init()
{
  _yMax=0.0;
  _yMin=0.0;
  _font=0;
  _offset=0;
  _xShift=0;
  _xOffset=0;
  _yOffset=0;
  _stipple=0;
  _xAxis=0;
  _yAxis=0;
  _symbolSize=DefaultTraceSymbolSize;
  _symbol=Cross;
  _style=Line;
  _lineWeight=2;
  _lineWidth=1;
  _lineStyle=MSSolid;
  _autoScale=MSFalse;
  _lastDataPoint=0.;
  _barCount=0;
  _hidden=MSFalse;
}

void MSTrace::font(const char *x_)
{ font(traceSet()->graph()->owner()->server()->fontID(x_)); }

const char *MSTrace::legend(unsigned x_) const
{return traceSet()->legend(x_);}
  
void MSTrace::legend(const char *x_)
{ traceSet()->legend(x_,virtualCol());}

int MSTrace::dataCount(void) const
{ return traceSet()->dataCount(); }

const char *MSTrace::legend(void) const
{ return traceSet()->legend(virtualCol());}

unsigned long MSTrace::lineColor(int r_,int c_) const
{ return traceSet()->traceLineColor(r_,c_); }
unsigned long MSTrace::lineColor(void) const
{ return traceSet()->traceLineColor(0,virtualCol()); }
unsigned long MSTrace::lineColor(int i_) const
{ return traceSet()->traceLineColor(i_,virtualCol()); }

unsigned long MSTrace::fillColor(int r_,int c_) const
{ return traceSet()->traceFillColor(r_,c_); }
unsigned long MSTrace::fillColor(void) const
{ return traceSet()->traceFillColor(0,virtualCol()); }
unsigned long MSTrace::fillColor(int i_) const
{ return traceSet()->traceFillColor(i_,virtualCol()); }

double MSTrace::x(int i_) const
{ return traceSet()->x(i_); }

double MSTrace::y(int r_,int c_) const
{ return traceSet()->y(r_,c_); }

double MSTrace::y(int i_) const
{ return traceSet()->y(i_,virtualCol()); }






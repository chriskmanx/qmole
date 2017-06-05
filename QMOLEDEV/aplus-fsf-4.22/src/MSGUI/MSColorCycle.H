#ifndef MSColorCycleHEADER
#define MSColorCycleHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSWidget.H>
#include <MSTypes/MSUnsignedLongVector.H>
#include <MSIPC/MSTv.H>

class MSGUIExport MSColorCycle
{
private:
  MSWidget            *_widget;
  int                  _row;
  int                  _column;
  unsigned             _count;
  MSUnsignedLongVector _colors;
  MSCycleColorMode     _mode;
  struct timeval       _lastUpdate;
  
public:  
  MSColorCycle(MSWidget *widget_,int row_,int column_,const MSUnsignedLongVector& colors_,
	       MSCycleColorMode mode_=MSForeground);
  ~MSColorCycle(void);

  int row(void) const                            { return _row; }  
  int column(void) const                         { return _column; }  
  unsigned count(void) const                     { return _count; }  
  unsigned numCycles(void) const                 { return (mode()==MSReverseVideo)?1:_colors.length(); }
  const MSUnsignedLongVector& colors(void) const { return _colors; }
  MSCycleColorMode mode(void) const              { return _mode; }
  MSWidget *widget(void) const                   { return _widget; }
  void increment(void) { _count++; }
  int long color(unsigned count_) const 
   { return (count_<_colors.length())?_colors(count_):0; }

  void reset(void) {_count=0;}
  void mode(MSCycleColorMode mode_) {_mode=mode_;}

  const struct timeval &lastUpdate(void) const { return _lastUpdate; }
  void lastUpdate(struct timeval &lastUpdate_) { _lastUpdate=lastUpdate_; }
};

#endif


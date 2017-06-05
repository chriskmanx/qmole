#ifndef MSTraceHEADER
#define MSTraceHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <X11/Xlib.h>
#include <MSTypes/MSStringVector.H>
#include <MSGUI/MSGUIEnum.H>
#include <MSGUI/MSG.H>

class MSTraceSet;

class MSGUIExport MSTrace : public MSG
{
  friend class MSGraph;
  friend class MSTraceSet;

 public:
  MSTrace(MSTraceSet *,int col_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSTrace(void);
  MSTraceSet *traceSet(void)             const {return _traceSet;}
  const MSSymbol& tag(void)              const {return _tag;}
  void tag(const MSSymbol& tag_)               {_tag=tag_;}

 protected:
  void init(void);
  int virtualCol(void)                   const {return _virtualCol;}
  int point(void)                        const {return _point;}
  int barCount(void)                     const {return _barCount;}
  int xOffset(void)                      const {return _xOffset;}
  int yOffset(void)                      const {return _yOffset;}
  int offset(void)                       const {return _offset;}
  double xShift(void)                    const {return _xShift;}
  MSBoolean autoScale(void)              const {return _autoScale;}
  double lastDataPoint(void)             const {return _lastDataPoint;}
  double yMax(void)                      const {return _yMax;}
  double yMin(void)                      const {return _yMin;}
  int dataCount(void) const;
				     
  void virtualCol(int x_)                      {_virtualCol=x_;}  
  void point(int x_)                           {_point=x_;}
  void barCount(int x_)                        {_barCount=x_;}
  void xOffset(int x_)                         {_xOffset=x_;}
  void yOffset(int x_)                         {_yOffset=x_;}
  void offset(int x_)                          {_offset=x_;}
  void xShift(double x_)                       {_xShift=x_;}
  void autoScale(MSBoolean x_)                 {_autoScale=x_;}
  void lastDataPoint(double x_)                {_lastDataPoint=x_;}  
  void yMin(double x_)                         {_yMin=x_;}
  void yMax(double x_)                         {_yMax=x_;}

  Font font(void)                        const {return _font;}
  const MSStringVector& textSymbol(void) const {return _textSymbol;}
  Pixmap stipple(void)                   const {return _stipple;}
  int symbolSize(void)                   const {return _symbolSize;}
  int lineWidth(void)                    const {return _lineWidth;}
  int xAxis(void)                        const {return _xAxis;}
  int yAxis(void)                        const {return _yAxis;}
  unsigned long lineStyle(void)          const {return _lineStyle;}
  unsigned long lineWeight(void)         const {return _lineWeight;}
  unsigned long style(void)              const {return _hidden==MSTrue?MSNone:_style;}
  unsigned long symbol(void)             const {return _symbol;}
  MSBoolean hidden(void)                 const {return _hidden;}
  MSBoolean x(void)                      const {return MSFalse;}
  unsigned long lineColor(void) const;
  unsigned long fillColor(void) const;
  const char *legend(void) const;
  const char *legend(unsigned) const;
  unsigned long lineColor(int,int) const;
  unsigned long fillColor(int,int) const;
  unsigned long lineColor(int) const;
  unsigned long fillColor(int) const;

  virtual double x(int) const;
  virtual double y(int) const;
  virtual double y(int,int) const;

  void font(const char *x_);
  void legend(const char *x_);
  void font(Font x_)                           {_font=x_;}
  void textSymbol(const MSStringVector& x_)    {_textSymbol=x_;}
  void xAxis(int x_)                           {_xAxis=x_;}
  void yAxis(int x_)                           {_yAxis=x_;}
  void stipple(Pixmap x_)                      {_stipple=x_;}
  void symbolSize(int x_)                      {_symbolSize=x_%2==0?x_-1:x_;}
  void lineWidth(int x_)                       {_lineWidth=x_<35?x_:35;}
  void lineStyle(unsigned long x_)             {_lineStyle=x_;}
  void lineWeight(int x_)                      {_lineWeight=x_<0?0:x_>4?4:x_;}
  void symbol(unsigned long x_)                {_symbol=x_;}
  void hidden(MSBoolean x_)                    {_hidden=x_;}
  void style(unsigned long x_)                 {_style=x_;}

private:
  MSTraceSet           *_traceSet;  
  MSSymbol 		_tag;
  MSStringVector	_textSymbol;
  Pixmap 		_stipple;
  int    		_symbolSize;
  int    		_lineWidth;
  int	 	        _xOffset;
  int	 	        _yOffset; 
  int	 	        _offset; 
  int			_xAxis;
  int			_yAxis;
  int                   _virtualCol;
  int 		        _point;
  int		        _barCount;
  int 	                _lineWeight;
  unsigned long 	_symbol;
  unsigned long 	_style;
  unsigned long 	_lineStyle;
  Font                  _font;

  MSBoolean		_autoScale;
  MSBoolean		_hidden;

  double	        _xShift;
  double	        _lastDataPoint;
  double 	        _yMin;
  double 	        _yMax;
};

#endif

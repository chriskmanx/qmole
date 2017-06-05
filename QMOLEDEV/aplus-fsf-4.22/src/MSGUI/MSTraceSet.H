#ifndef MSTraceSetHEADER
#define MSTraceSetHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSFloatVector.H>
#include <MSTypes/MSFloatMatrix.H>
#include <MSTypes/MSUnsignedVector.H>
#include <MSTypes/MSUnsignedLongVector.H>
#include <MSGUI/MSWidgetView.H>
#include <MSGUI/MSPointerArray.H>
#include <MSGUI/MSTrace.H>
#include <MSGUI/MSG.H>
#include <MSTypes/MSFormat.H>

class MSGraph;
class MSTrace;
class MSTraceSet;

class MSGUIExport MSTraceSet:public MSWidgetView,public MSG
{
friend class MSTrace;
friend class MSGraph;

public:
  MSTraceSet(MSGraph *,const char *legend_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSTraceSet(MSGraph *,const MSStringVector& legend_,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSTraceSet(void);
  
  const MSStringVector& legend(void) const;
  Font textFont(void) const;
  unsigned long textForeground(void) const;
  const MSSymbol& tag(void) const;
  MSGraph *graph(void) const;
  MSBoolean overlap(void) const;
  double pieAspectRatio(void) const;
  double pieDepthFactor(void) const;
  int primarySlice(void) const;
  MSAlignment primarySliceAlignment(void) const;
  int pieAngle(void) const;
  void tag(const MSSymbol&);
  void overlap(MSBoolean);
  void pieDepthFactor(double);
  void pieAspectRatio(double);
  void primarySlice(int);
  void primarySliceAlignment(MSAlignment);
  void pieAngle(int);
  void textFont(const char *);
  void textFont(Font);
  void textForeground(const char *);
  void textForeground(unsigned long);

  MSUnsignedLongVector& lineColors(void);
  MSUnsignedLongVector& fillColors(void);
  const MSUnsignedLongVector& lineColors(void) const;
  const MSUnsignedLongVector& fillColors(void) const;

  void pieOffsets(double);
  void pieOffsets(const MSFloatVector&);
  const MSFloatVector *pieOffsets(void) const;
  MSFloatVector *pieOffsets(void);

  unsigned long pieLegendAlignment(void) const;
  void pieLegendAlignment(unsigned long);
  void pieLegendAlignment(MSAlignment);
  unsigned long pieValueAlignment(void) const;
  void pieValueAlignment(unsigned long);
  void pieValueAlignment(MSAlignment);
  unsigned long piePercentAlignment(void) const;
  void piePercentAlignment(unsigned long);
  void piePercentAlignment(MSAlignment);
  MSFloat::MSFloatFormat piePercentFormat(void) const;
  void piePercentFormat(MSFloat::MSFloatFormat);

  double pieOffset(unsigned) const;
  double pieOffset(void) const;

  void pieProfiles(const MSFloatVector&);
  const MSFloatVector *pieProfiles(void) const;
  MSFloatVector *pieProfiles(void);
  double pieProfile(unsigned) const;

  void lineColor(unsigned long);
  void fillColor(unsigned long);
  void lineColor(unsigned long,unsigned);
  void fillColor(unsigned long,unsigned);
  void lineColor(const MSUnsignedLongVector&);
  void fillColor(const MSUnsignedLongVector&);
  void lineColor(const MSStringVector&);
  void fillColor(const MSStringVector&);
  void lineColor(const char *);
  void fillColor(const char *);

  unsigned long lineColor(unsigned) const;
  unsigned long fillColor(unsigned) const;
  unsigned long lineColor(unsigned,unsigned) const;
  unsigned long fillColor(unsigned,unsigned) const;

  virtual void selectable(MSBoolean);
  virtual void constraint(unsigned long);
  virtual MSBoolean selectable(void) const;
  virtual unsigned long constraint(void) const;
  virtual MSBoolean mapped(void) const;
  virtual void unmap(void);
  virtual void hide(void);
  virtual void show(void);

  virtual const char *legend(unsigned);
  virtual const MSStringVector& textSymbol(unsigned);
  virtual Font font(unsigned) const;
  virtual Pixmap stipple(unsigned) const;
  virtual void validateText(const MSStringVector);
  virtual const char *formatText(MSString &,unsigned);
  virtual unsigned textLength(void) const;
  virtual MSStringVector& text(void);
  virtual const MSStringVector& text(void) const;
  virtual MSUnsignedVector lineWidth(void) const;
  virtual MSUnsignedVector symbolSize(void) const;
  virtual MSUnsignedVector lineWeight(void) const;
  virtual MSAlignment xAxis(unsigned) const;
  virtual MSAlignment yAxis(unsigned) const;
  virtual unsigned long style(unsigned) const;
  virtual unsigned long symbol(unsigned) const;
  virtual unsigned lineWidth(unsigned) const;
  virtual unsigned symbolSize(unsigned) const;
  virtual unsigned long lineStyle(unsigned) const;
  virtual unsigned lineWeight(unsigned) const;
  virtual unsigned long traceFillColor(int,int) const;
  virtual unsigned long traceLineColor(int,int) const;

  virtual void legend(const char *);
  virtual void font(Font);
  virtual void font(const char *);
  virtual void stipple(Pixmap);
  virtual void lineWidth(unsigned);
  virtual void lineStyle(MSLineStyle);
  virtual void lineStyle(unsigned long);
  virtual void lineWeight(unsigned);
  virtual void xAxis(MSAlignment);
  virtual void yAxis(MSAlignment);
  virtual void style(Style);
  virtual void style(unsigned long);
  virtual void symbol(Symbol);
  virtual void symbol(unsigned long);
  virtual void symbolSize(unsigned);

  virtual void legend(const char *,unsigned);
  virtual void textSymbol(const MSStringVector&,unsigned);
  virtual void font(Font,unsigned);
  virtual void font(const char *,unsigned);
  virtual void stipple(Pixmap,unsigned);
  virtual void lineWidth(unsigned,unsigned);
  virtual void lineStyle(unsigned long,unsigned);
  virtual void lineWeight(unsigned,unsigned);
  virtual void xAxis(MSAlignment,unsigned);
  virtual void yAxis(MSAlignment,unsigned);
  virtual void style(unsigned long,unsigned);
  virtual void symbol(unsigned long,unsigned);
  virtual void symbolSize(unsigned,unsigned);

  virtual void legend(const MSStringVector&);
  virtual void textSymbol(const MSStringVector&);
  virtual void font(const MSUnsignedLongVector&);
  virtual void font(const MSStringVector&);
  virtual void stipple(const MSUnsignedLongVector&);
  virtual void lineWidth(const MSUnsignedVector&);
  virtual void lineStyle(const MSUnsignedLongVector&);
  virtual void lineWeight(const MSUnsignedVector&);
  virtual void xAxis(const MSUnsignedLongVector&, MSExplicitInit);
  virtual void yAxis(const MSUnsignedLongVector&, MSExplicitInit);
  virtual void style(const MSUnsignedLongVector&);
  virtual void symbol(const MSUnsignedLongVector&);
  virtual void symbolSize(const MSUnsignedVector&);
  virtual void xOrigin(double);
  virtual void yOrigin(double);

  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);
  const MSFormat& format(void) const;
  void format(const MSFormat&);
  unsigned selectRow(void) const;
  unsigned selectCol(void) const;
  
protected:
  MSPointerArray<MSTrace>& traceList(void);
  const MSPointerArray<MSTrace>& traceList(void) const;
  MSTrace *trace(int i_) const;
  MSStringVector& legend(void);
  unsigned updateIndex(void) const;
  unsigned lastDataCount(void) const;
  int normalizedOffset(void) const;
  double xMax(void) const;
  double xMin(void) const;
  double yMax(void) const;
  double yMin(void) const;
  double xDelta(void) const;
  double xOffset(void) const;
  double yOffset(void) const;
  virtual int numColumns(void) const;
  virtual int dataCount(void) const;
  virtual double x(int) const;    
  virtual double y(int,int) const;

  void selectRow(unsigned);
  void selectCol(unsigned);
  void updateIndex(unsigned);
  void lastDataCount(unsigned);
  void normalizedOffset(int);
  void xMin(double);
  void xMax(double);
  void yMin(double);
  void yMax(double);
  void xDelta(double);
  void xOffset(double);
  void yOffset(double);
  
  
  void selected(unsigned,unsigned);
  Font traceFont(MSTrace *);
  void deleteTraces(void);
  void computeExtents(void);
  void computeExtents(const MSIndexVector&);
  void configureTraces(void);
  virtual void create(void);
  virtual void map(void);
  virtual void updateData(void);
  virtual void setData(void);
  virtual void update(const MSIndexVector&);
  virtual void validate(int,int,double,double);
  virtual MSBoolean   moveTraceValidate(double,double);
  virtual MSTraceSet* copyTraceValidate(MSGraph*,const char*, const MSSymbol&);
  virtual MSFloatMatrix asFloatMatrix(void) const;
  virtual void updateTraceStyle(unsigned long);
  void init(void);

  virtual const char *formatOutput(MSString &,unsigned,unsigned);

private:
  MSSymbol                _tag;
  unsigned	          _selectRow;
  unsigned	          _selectCol;
  MSBoolean 	          _selectable;
  MSBoolean		  _overlap;
  double 	          _xMin;
  double 	          _xMax;
  double 	          _yMin;
  double 	          _yMax;
  double 	          _xDelta;
  double 	          _xOffset;
  double 	          _yOffset;
  double 	          _pieDepthFactor;
  double 	          _pieAspectRatio;
  int                     _primarySlice;
  MSAlignment             _primarySliceAlignment;
  double                  _pieOffset;
  int  	                  _pieAngle;
  unsigned 	          _updateIndex;
  unsigned	          _lastDataCount;
  int	                  _normalizedOffset;
  unsigned long           _constraint;
  unsigned long           _pieLegendAlignment;
  unsigned long           _pieValueAlignment;
  unsigned long           _piePercentAlignment;
  MSFloat::MSFloatFormat  _piePercentFormat;
  static MSStringVector   _text;
  MSStringVector          _legend;
  MSUnsignedLongVector    _lineColors;
  MSUnsignedLongVector    _fillColors;
  Font                    _textFont;
  unsigned long           _textForeground;
  MSPointerArray<MSTrace> _traceList;
  MSFloatVector          *_pieOffsets;
  MSFloatVector          *_pieProfiles;
  MSFormat                _format;
};

#ifndef MS_NO_INLINES
#ifndef MSTraceSetINLINES
#include <MSGUI/MSTraceSetInlines.C>
#endif
#endif

// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSGUILIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSPointerArray<MSTrace>
#endif // !__MSGUILIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif

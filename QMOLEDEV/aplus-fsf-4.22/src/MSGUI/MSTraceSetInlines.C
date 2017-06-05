#ifndef MSTraceSetINLINES
#define MSTraceSetINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MS_NO_INLINES
#define INLINELINKAGE inline
#else
#define INLINELINKAGE
#endif

INLINELINKAGE const MSStringVector& MSTraceSet::legend(void)             const {return _legend;}
INLINELINKAGE Font MSTraceSet::textFont(void)                            const {return _textFont;}
INLINELINKAGE unsigned long MSTraceSet::textForeground(void)             const {return _textForeground;}
INLINELINKAGE const MSSymbol& MSTraceSet::tag(void)                      const {return _tag;}
INLINELINKAGE MSGraph *MSTraceSet::graph(void)                           const {return (MSGraph *)owner();}
INLINELINKAGE MSBoolean MSTraceSet::overlap(void)                        const {return _overlap;}
INLINELINKAGE unsigned long MSTraceSet::pieLegendAlignment(void)         const {return _pieLegendAlignment;}
INLINELINKAGE unsigned long MSTraceSet::pieValueAlignment(void)          const {return _pieValueAlignment;}
INLINELINKAGE unsigned long MSTraceSet::piePercentAlignment(void)        const {return _piePercentAlignment;}
INLINELINKAGE MSFloat::MSFloatFormat MSTraceSet::piePercentFormat(void)  const {return _piePercentFormat;}
INLINELINKAGE void MSTraceSet::piePercentFormat(MSFloat::MSFloatFormat x_)     {_piePercentFormat=x_;}
INLINELINKAGE void MSTraceSet::pieLegendAlignment(MSAlignment align_)
{pieLegendAlignment((unsigned long) align_);}
INLINELINKAGE void MSTraceSet::pieValueAlignment(MSAlignment align_)
{pieValueAlignment((unsigned long) align_);}
INLINELINKAGE void MSTraceSet::piePercentAlignment(MSAlignment align_)
{piePercentAlignment((unsigned long) align_);}
INLINELINKAGE double MSTraceSet::pieDepthFactor(void)                    const {return _pieDepthFactor;}
INLINELINKAGE double MSTraceSet::pieAspectRatio(void)                    const {return _pieAspectRatio;}
INLINELINKAGE int MSTraceSet::primarySlice(void)                         const {return _primarySlice;}
INLINELINKAGE MSAlignment MSTraceSet::primarySliceAlignment(void)        const {return _primarySliceAlignment;}
INLINELINKAGE int MSTraceSet::pieAngle(void)                             const {return _pieAngle;}
INLINELINKAGE void MSTraceSet::tag(const MSSymbol& tag_)                       {_tag=tag_;}
INLINELINKAGE void MSTraceSet::overlap(MSBoolean x_)                           {_overlap=x_;}
INLINELINKAGE MSUnsignedLongVector& MSTraceSet::lineColors(void)               {return _lineColors;}
INLINELINKAGE MSUnsignedLongVector& MSTraceSet::fillColors(void)               {return _fillColors;}
INLINELINKAGE const MSUnsignedLongVector& MSTraceSet::lineColors(void)   const {return _lineColors;}
INLINELINKAGE const MSUnsignedLongVector& MSTraceSet::fillColors(void)   const {return _fillColors;}
INLINELINKAGE const MSFloatVector *MSTraceSet::pieOffsets(void)          const {return _pieOffsets;}
INLINELINKAGE double MSTraceSet::pieOffset(void)                         const {return _pieOffset;}
INLINELINKAGE MSFloatVector *MSTraceSet::pieOffsets(void)                      {return _pieOffsets;}
INLINELINKAGE const MSFloatVector *MSTraceSet::pieProfiles(void)         const {return _pieProfiles;}
INLINELINKAGE MSFloatVector *MSTraceSet::pieProfiles(void)                     {return _pieProfiles;}
INLINELINKAGE MSPointerArray<MSTrace>& MSTraceSet::traceList(void)             {return _traceList;}
INLINELINKAGE const MSPointerArray<MSTrace>& MSTraceSet::traceList(void) const {return _traceList;}
INLINELINKAGE MSTrace *MSTraceSet::trace(int i_)                         const {return _traceList.array(i_);}
INLINELINKAGE MSStringVector& MSTraceSet::legend(void)                         {return _legend;}
INLINELINKAGE unsigned MSTraceSet::selectRow(void)                       const {return _selectRow;}
INLINELINKAGE unsigned MSTraceSet::selectCol(void)                       const {return _selectCol;}
INLINELINKAGE unsigned MSTraceSet::updateIndex(void)                     const {return _updateIndex;}
INLINELINKAGE unsigned MSTraceSet::lastDataCount(void)                   const {return _lastDataCount;}
INLINELINKAGE int MSTraceSet::normalizedOffset(void)                     const {return _normalizedOffset;}
INLINELINKAGE double MSTraceSet::xMax(void)                              const {return _xMax;}
INLINELINKAGE double MSTraceSet::xMin(void)                              const {return _xMin;}
INLINELINKAGE double MSTraceSet::yMax(void)                              const {return _yMax;}
INLINELINKAGE double MSTraceSet::yMin(void)                              const {return _yMin;}
INLINELINKAGE double MSTraceSet::xDelta(void)                            const {return _xDelta;}
INLINELINKAGE double MSTraceSet::xOffset(void)                           const {return _xOffset;}
INLINELINKAGE double MSTraceSet::yOffset(void)                           const {return _yOffset;}
INLINELINKAGE void MSTraceSet::selectRow(unsigned x_)                          {_selectRow=x_;}
INLINELINKAGE void MSTraceSet::selectCol(unsigned x_)                          {_selectCol=x_;}
INLINELINKAGE void MSTraceSet::updateIndex(unsigned x_)                        {_updateIndex=x_;}
INLINELINKAGE void MSTraceSet::lastDataCount(unsigned x_)                      {_lastDataCount=x_;}  
INLINELINKAGE void MSTraceSet::normalizedOffset(int x_)                        {_normalizedOffset=x_;}  
INLINELINKAGE void MSTraceSet::xMin(double x_)                                 {_xMin=x_;}  
INLINELINKAGE void MSTraceSet::xMax(double x_)                                 {_xMax=x_;}  
INLINELINKAGE void MSTraceSet::yMin(double x_)                                 {_yMin=x_;}  
INLINELINKAGE void MSTraceSet::yMax(double x_)                                 {_yMax=x_;}  
INLINELINKAGE void MSTraceSet::xDelta(double x_)                               {_xDelta=x_<INT_MAX?x_:1;}
INLINELINKAGE void MSTraceSet::xOffset(double x_)                              {_xOffset=x_;}
INLINELINKAGE void MSTraceSet::yOffset(double x_)                              {_yOffset=x_;}
INLINELINKAGE const MSFormat& MSTraceSet::format(void)                   const { return _format; }

#endif

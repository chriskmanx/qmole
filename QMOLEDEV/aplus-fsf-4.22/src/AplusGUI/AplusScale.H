#ifndef AplusScaleHEADER
#define AplusScaleHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSEventSender.H>
#include <MSGUI/MSVScale.H>
#include <MSGUI/MSHScale.H>
#include <MSGUI/MSVGauge.H>
#include <MSGUI/MSHGauge.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusEvent.H>
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/AplusFormatter.H>

#define SliderType(AplusSliderType, Type) \
class AplusSliderType : public Type\
{\
public:\
  AplusSliderType(MSWidget *);\
  ~AplusSliderType(void);\
\
  virtual double currentValue(void);\
  virtual double currentValue(void) const;\
  virtual MSBoolean assignValue(double);\
\
  virtual void receiveEvent(MSEvent &);\
  virtual void addSenderNotify(MSEventSender *m_);\
  virtual MSBoolean validate(const char *str_);\
  A defaultInFunc(V, const char *);\
\
  virtual AplusModel *model(void)                       { return (AplusModel*) _model; }\
  virtual MSBoolean verifyData(V,A)            	        { return MSTrue;               }\
  AplusSliderType& operator=(const MSFloat& value_)     { value(value_); return *this; }\
  AplusSliderType& operator=(double value_)             { value(value_); return *this; }\
  static AplusFormatter *outFmt(void)    	        { return &_outFormat;          }\
\
  virtual const MSSymbol& widgetType(void) const;\
  static const MSSymbol& symbol(void);\
\
protected:\
  virtual void editValue(const char *);\
  virtual void editorEscape(void);\
\
private:\
  static AplusFormatter _outFormat;\
  STANDARD_WIDGET_METHODS_H\
\
};

SliderType(AplusVScale, MSVScale)
SliderType(AplusHScale, MSHScale)
SliderType(AplusVGauge, MSVGauge)
SliderType(AplusHGauge, MSHGauge)

#endif  //AplusScaleHEADER

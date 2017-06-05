///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/AplusScale.H>
#include <AplusGUI/AplusCommon.H>

extern long dbg_tmstk;
extern void busyEnable(MSBoolean);
extern MSBoolean busyEnable(void);

#define SliderMethodsMacro(AplusSliderType, Type) \
\
AplusFormatter AplusSliderType::_outFormat;\
\
AplusSliderType::AplusSliderType(MSWidget *owner_) : Type(owner_)\
{\
}\
\
AplusSliderType::~AplusSliderType(void)\
{}\
\
void AplusSliderType::receiveEvent(MSEvent &event_)\
{\
  if (event_.type() == AplusEvent::symbol())\
   {\
     if (dbg_tmstk) cout << "Received UpdateEvent in AplusSliderType"  << endl;\
     update(MSIndexVector::nullVector());\
   }\
  if (event_.type() == AplusVerifyEvent::symbol())\
   {\
     if (dbg_tmstk) cout << "Received VerifyEvent in AplusSliderType"  << endl;\
\
     AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;\
     ave->result(verifyData(ave->aplusVar(), ave->a()));\
   }\
}\
\
void AplusSliderType::addSenderNotify(MSEventSender *m_)\
{\
  INTERNAL_COUPLE(((AplusModel *) m_));\
}\
\
double AplusSliderType::currentValue(void) const \
{\
  /* this method should never be called from within aplus */ \
  return 0.0;\
}\
\
double AplusSliderType::currentValue(void)\
{\
  double x=0;\
  \
  if (model() && model()->aplusVar()!=0)\
   {\
     A av=(A)model()->aplusVar()->a;  \
     if (av->r==0&&av->n==1) \
      {\
	P p; p.i=av->p;\
	x=av->t==It?(double)p.i[0]:p.f[0];\
      }\
   }\
  return x>valueMax()?valueMax():x<valueMin()?valueMin():x;\
}\
\
\
MSBoolean AplusSliderType::validate(const char *str_)\
{\
  MSBoolean busyState=busyEnable();\
  busyEnable(MSFalse); \
  extern int safeAset(V,A,A,A);\
  V v = (model()!=0)?model()->aplusVar():0;\
  MSBoolean success = MSFalse;\
  \
  if (v)\
   {\
     A r = aplus_nl;\
     AInFunction *inFunc;\
     if ((inFunc=AplusModel::getInFunc(v))!=0)\
       r=inFunc->invoke(v,(char*)str_,-1,-1);\
     else r=defaultInFunc(v,str_);\
     if (r!=0) \
      {\
	if (aset((I)v,(I)r,0,0)==0) showError(qs);\
	else\
	 {\
	   AplusModel::doneCB(v,r,0,0);\
	   success=MSTrue;\
	 }\
      }\
   }\
  busyEnable(busyState); \
  return success;\
}\
\
MSBoolean AplusSliderType::assignValue(double x_)\
{\
  MSBoolean busyState=busyEnable();\
  busyEnable(MSFalse); \
  extern int safeAset(V,A,A,A);\
  MSBoolean success = MSFalse;\
  V v = (model()!=0)?model()->aplusVar():0;\
  \
  if (v)\
   {\
     double x=x_>valueMax()?valueMax():x_<valueMin()?valueMin():x_;\
     A av=(A)v->a;\
     A r=av->t==It?gi((int)x):gf((double)x);\
     if (aset((I)v,(I)r,0,0)==0) showError(qs); \
     else success = MSTrue;\
   }\
  busyEnable(busyState); \
  return success;\
}\
\
  \
A AplusSliderType::defaultInFunc(V v_,const char *string_)\
{\
  A r=aplus_nl;\
  if (v_!=0)\
   {\
     char *ptrchar=0;\
     A av=(A)v_->a;\
     double num;\
     \
     if (av->t==It)\
      {\
	num=strtol((char *)string_,&ptrchar,10); /* Base 10 */ \
	if (ptrchar==(char *)string_) \
	 {\
	   r = aplus_nl;\
	   showError("Unknown Number: Integer Expected");\
	 }\
	else r=gi((int)num );\
      }\
     else if (av->t==Ft)\
      {\
	num=strtod((char *)string_,&ptrchar);\
	if (ptrchar==(char *)string_) \
	 {\
	   r=aplus_nl;\
	   showError("Unknown Number: Float Expected");\
	 }\
	else r=gf((double)num);\
      }\
   }\
  return (A)r;\
}\
\
\
void AplusSliderType::editValue(const char *pString_)\
{\
  Type::editValue(pString_);\
  if (editor()->mapped()==MSTrue)\
    {\
      activateCallback(MSWidgetCallback::editbegin);\
    }\
}\
\
\
void AplusSliderType::editorEscape(void)\
{\
  MSBoolean mapped = editor()->mapped();\
  Type::editorEscape();\
  if (mapped==MSTrue && editor()->mapped()==MSFalse)\
    {\
      activateCallback(MSWidgetCallback::editend);\
    }\
}\
\
\
const MSSymbol& AplusSliderType::widgetType(void) const\
{\
  return symbol();\
}\
\
\
const MSSymbol& AplusSliderType::symbol(void)\
{\
  static MSSymbol sym(#AplusSliderType);\
  return sym;\
}\


SliderMethodsMacro(AplusVScale, MSVScale)
SliderMethodsMacro(AplusHScale, MSHScale)
SliderMethodsMacro(AplusVGauge, MSVGauge)
SliderMethodsMacro(AplusHGauge, MSHGauge)

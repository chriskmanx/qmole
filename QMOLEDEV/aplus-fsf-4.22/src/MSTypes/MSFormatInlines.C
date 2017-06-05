#ifndef MSFormatINLINES
#define MSFormatINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


INLINELINKAGE MSFormat::MSFormatType MSFormat::formatType(void) const
{ return _formatType; }
INLINELINKAGE unsigned long MSFormat::formatModifier(void) const
{ return _formatModifier; }

INLINELINKAGE MSBool::MSBoolFormat MSFormat::boolFormat(void) const 
{ return _format._bool; }
INLINELINKAGE MSDate::MSDateFormat MSFormat::dateFormat(void) const 
{ return _format._date; } 
INLINELINKAGE MSFloat::MSFloatFormat MSFormat::floatFormat(void) const 
{ return _format._float; }
INLINELINKAGE MSInt::MSIntFormat MSFormat::intFormat(void) const 
{ return _format._int; }
INLINELINKAGE MSMoney::MSMoneyFormat MSFormat::moneyFormat(void) const 
{ return _format._money; }
INLINELINKAGE MSRate::MSRateFormat MSFormat::rateFormat(void) const 
{ return _format._rate; }  
INLINELINKAGE MSTerm::MSTermFormat MSFormat::termFormat(void) const 
{ return _format._term; } 
INLINELINKAGE MSTime::MSTimeFormat MSFormat::timeFormat(void) const 
{ return _format._time; } 
#endif


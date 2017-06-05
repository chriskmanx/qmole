#ifndef MSBoolINLINES
#define MSBoolINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


INLINELINKAGE MSBool::MSBool(MSBoolean aBoolean_)
{ _isSet=MSTrue; _bool=aBoolean_; }
INLINELINKAGE MSBool::MSBool(void)
{ _isSet=MSFalse; _bool=MSFalse; }

INLINELINKAGE MSBoolean MSBool::isSet(void) const
{ return _isSet; }
INLINELINKAGE MSBoolean MSBool::isTrue(void) const
{ return _bool; }
INLINELINKAGE MSBoolean MSBool::isFalse(void) const
{ return MSBoolean(_bool==MSFalse); }

INLINELINKAGE MSBool::operator int() const
{ return _bool; }
INLINELINKAGE MSBool::operator MSBoolean() const
{ return _bool; }

INLINELINKAGE MSBoolean MSBool::operator==(const MSBool& aBoolean_) const
{ return MSBoolean(_bool==aBoolean_._bool); }
INLINELINKAGE MSBoolean MSBool::operator==(int i_) const
{ return MSBoolean(i_?_bool:!_bool); }

INLINELINKAGE MSBoolean MSBool::operator!=(const MSBool& aBoolean_) const
{ return MSBoolean(_bool!=aBoolean_._bool); }
INLINELINKAGE MSBoolean MSBool::operator!=(int i_) const
{ return MSBoolean(i_?!_bool:_bool); }

INLINELINKAGE MSBoolean MSBool::operator <(const MSBool& aBoolean_) const
{ return _bool==MSFalse&&aBoolean_._bool==MSTrue?MSTrue:MSFalse; }
INLINELINKAGE MSBoolean MSBool::operator >(const MSBool& aBoolean_) const
{ return _bool==MSTrue&&aBoolean_._bool==MSFalse?MSTrue:MSFalse; }
INLINELINKAGE MSBoolean MSBool::operator<=(const MSBool& aBoolean_) const
{ return _bool==aBoolean_._bool||aBoolean_._bool==MSTrue?MSTrue:MSFalse; }
INLINELINKAGE MSBoolean MSBool::operator>=(const MSBool& aBoolean_) const
{ return _bool==aBoolean_._bool||aBoolean_._bool==MSFalse?MSFalse:MSTrue; }

INLINELINKAGE MSBool& MSBool::operator=(const MSBool& aBoolean_)
{ _isSet=aBoolean_._isSet,_bool=aBoolean_._bool; return changed(),*this; }
INLINELINKAGE MSBool& MSBool::operator=(MSBoolean aBoolean_)
{ _isSet=MSTrue,_bool=aBoolean_; return changed(),*this; }
INLINELINKAGE MSBool& MSBool::operator=(int i_)
{ _isSet=MSTrue,_bool=(i_?MSTrue:MSFalse); return changed(),*this; }

#if !defined (MS_ENUM_COMPARE_BUG)
INLINELINKAGE MSBoolean MSBool::operator!=(MSBoolean bool_) const
{ return MSBoolean(_bool!=bool_); }
INLINELINKAGE MSBoolean MSBool::operator==(MSBoolean bool_) const
{ return MSBoolean(_bool==bool_); }
#endif


#endif
